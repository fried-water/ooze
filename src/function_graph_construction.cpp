#include "pch.h"

#include "constructing_graph.h"
#include "function_graph_construction.h"

namespace ooze {

namespace {

std::vector<PassBy> pass_bys_of(
  const std::unordered_set<TypeID>& copy_types, const TypeGraph& g, Type t, std::vector<PassBy> pass_bys = {}) {
  preorder(g, t, [&](Type t) {
    switch(g.get<TypeTag>(t)) {
    case TypeTag::Leaf:
      pass_bys.push_back(copy_types.find(g.get<TypeID>(t)) != copy_types.end() ? PassBy::Copy : PassBy::Move);
      return false;
    case TypeTag::Fn: pass_bys.push_back(PassBy::Copy); return false;
    case TypeTag::Borrow: pass_bys.push_back(PassBy::Borrow); return false;
    case TypeTag::Floating: assert(false);
    case TypeTag::Tuple: return true;
    }
    assert(false);
    return false;
  });

  return pass_bys;
}

int borrow_count(const TypeGraph& g, const Type& t) {
  int borrows = 0;

  preorder(g, t, [&](Type t) {
    switch(g.get<TypeTag>(t)) {
    case TypeTag::Leaf:
    case TypeTag::Fn: return false;
    case TypeTag::Borrow: borrows++; return false;
    case TypeTag::Floating: assert(false);
    case TypeTag::Tuple: return true;
    }
    assert(false);
    return false;
  });

  return borrows;
}

auto find_captures(const AST& ast, const Map<ASTID, ASTID>& overloads, ASTID expr_id) {
  std::vector<std::pair<ASTID, ASTID>> values;
  std::vector<std::pair<ASTID, ASTID>> borrows;

  for(const ASTID id : ast.forest.pre_order_ids(expr_id)) {
    if(ast.forest[id] != ASTTag::ExprIdent) continue;

    const auto it = overloads.find(id);
    assert(it != overloads.end() && !ast.forest.is_root(it->second));

    const ASTID binding = it->second;

    const auto ancestors = ast.forest.ancestor_ids(binding);
    if(stdr::find(ancestors, expr_id) == ancestors.end()) {
      const auto parent = ast.forest.parent(id);
      const bool borrowed = parent && ast.forest[*parent] == ASTTag::ExprBorrow;

      if(borrowed && stdr::none_of(borrows, [&](auto p) { return p.second == binding; })) {
        borrows.emplace_back(id, binding);
      } else if(!borrowed && stdr::none_of(values, [&](auto p) { return p.second == binding; })) {
        values.emplace_back(id, binding);
      }
    }
  }

  auto value_bindings = transform_to_vec(values, Get<1>{});
  auto borrow_bindings = remove_if(transform_to_vec(borrows, Get<1>{}), [&](ASTID binding) {
    return stdr::find(value_bindings, binding) != value_bindings.end();
  });

  return std::tuple(std::move(value_bindings), std::move(borrow_bindings));
}

struct GraphContext {
  ConstructingGraph cg;
  Map<ASTID, std::vector<Oterm>> bindings;
  Map<Oterm, ASTID> oterm_srcs;
};

std::pair<GraphContext, int>
append_bindings(const AST& ast, ASTID pattern, const std::vector<Oterm>& terms, GraphContext ctx, int offset = 0) {
  assert(offset <= terms.size());
  auto it = terms.begin() + offset;

  for(const ASTID id : ast.forest.pre_order_ids(pattern)) {
    if(ast.forest[id] == ASTTag::PatternWildCard) {
      it += size_of(ast.tg, ast.types[id.get()]);
    } else if(ast.forest[id] == ASTTag::PatternIdent) {
      const auto count = size_of(ast.tg, ast.types[id.get()]);
      assert(offset + count <= terms.size());
      ctx.bindings[id] = std::vector<Oterm>(it, it + count);
      std::for_each(it, it + count, [&](Oterm oterm) { ctx.oterm_srcs.emplace(oterm, id); });
      it += count;
    }
  }

  return {std::move(ctx), int(std::distance(terms.begin(), it))};
}

ContextualResult<std::pair<GraphContext, std::vector<Oterm>>, Program>
add_expr(Program,
         GraphContext,
         const AST&,
         const std::unordered_set<TypeID>& copy_types,
         const Map<ASTID, ASTID>& overloads,
         ASTID);

template <typename T, size_t Count>
struct Grouping {
  std::vector<T> vec;
  std::array<int, Count - 1> points;

  Span<T> operator[](int i) const {
    const int start = i == 0 ? 0 : points[i - 1];
    const int end = i == points.size() ? int(vec.size()) : points[i];
    return Span<T>(vec.data() + start, vec.data() + end);
  };
};

std::vector<Oterm> append_binding_terms(
  const Map<ASTID, std::vector<Oterm>>& ctx_bindings, Span<ASTID> bindings, std::vector<Oterm> terms) {
  return fold(bindings, std::move(terms), [&](auto terms, ASTID binding) {
    return to_vec(ctx_bindings.at(binding), std::move(terms));
  });
}

std::tuple<i32, i32>
find_offsets(const TypeGraph& tg,
             const std::vector<Type>& ast_types,
             Span<ASTID> owned_bindings,
             Span<ASTID> borrowed_bindings,
             i32 owned_offset = 0,
             i32 borrow_offset = 0) {
  for(const ASTID id : owned_bindings) {
    const i32 borrows = borrow_count(tg, ast_types[id.get()]);
    borrow_offset += borrows;
    owned_offset += size_of(tg, ast_types[id.get()]) - borrows;
  }

  return std::tuple(owned_offset, fold(borrowed_bindings, borrow_offset, [&](i32 acc, ASTID id) {
                      return acc + size_of(tg, ast_types[id.get()]);
                    }));
}

Grouping<ASTID, 3> find_owned_grouping(Span<ASTID> v1, Span<ASTID> v2, Span<ASTID> b1, Span<ASTID> b2) {
  std::vector<ASTID> values;
  values.reserve(v1.size() + v2.size()); // reserve so address is stable

  values = filter_to_vec(v1, contained_fn(v2), std::move(values));

  // Promote values owned in one branch and borrowed in the other to entirely owning
  values = filter_to_vec(b2, contained_fn(v1), std::move(values));
  values = filter_to_vec(b1, contained_fn(v2), std::move(values));

  const auto shared = std::span(values.begin(), values.end());

  const int shared_count = int(values.size());
  values = filter_to_vec(v1, std::not_fn(contained_fn(shared)), std::move(values));
  const int v1_count = int(values.size());
  values = filter_to_vec(v2, std::not_fn(contained_fn(shared)), std::move(values));
  return {std::move(values), {shared_count, v1_count}};
}

Grouping<ASTID, 3> find_shared_grouping(Span<ASTID> v1, Span<ASTID> v2, Span<ASTID> owned) {
  const auto not_owned = std::not_fn(contained_fn(owned));

  std::vector<ASTID> values = filter_to_vec(v1, contained_fn(v2));
  const int shared_count = int(values.size());
  values = filter_to_vec(v1, and_fn(not_owned, std::not_fn(contained_fn(v2))), std::move(values));
  const int v1_count = int(values.size());
  values = filter_to_vec(v2, and_fn(not_owned, std::not_fn(contained_fn(v1))), std::move(values));
  return {std::move(values), {shared_count, v1_count}};
}

FunctionGraph reparameterize_graph(
  const TypeGraph& tg,
  const std::vector<Type>& ast_types,
  const FunctionGraphData& data,
  Span<PassBy> pass_bys,
  Span<Span<ASTID>> owned,
  Span<Span<ASTID>> borrowed) {

  std::vector<bool> input_borrows;
  std::vector<std::pair<ASTID, int>> offsets;

  for(Span<ASTID> ids : owned) {
    for(const ASTID id : ids) {
      offsets.emplace_back(id, int(input_borrows.size()));
      input_borrows = borrows_of(tg, ast_types[id.get()], std::move(input_borrows));
    }
  }

  for(Span<ASTID> ids : borrowed) {
    for(const ASTID id : ids) {
      offsets.emplace_back(id, int(input_borrows.size()));
      input_borrows.insert(input_borrows.end(), size_of(tg, ast_types[id.get()]), true);
    }
  }

  offsets.emplace_back(ASTID::Invalid(), int(input_borrows.size()));

  auto [cg, inputs] = make_graph(std::move(input_borrows));

  std::vector<Oterm> terms;
  terms.reserve(inputs.size());

  for(const ASTID id : data.captured_values) {
    const auto it = stdr::find_if(offsets, [&](auto p) { return p.first == id; });
    for(int i = it->second; i < (it + 1)->second; i++) terms.push_back(inputs[i]);
  }

  for(const ASTID id : data.captured_borrows) {
    const auto it = stdr::find_if(offsets, [&](auto p) { return p.first == id; });
    for(int i = it->second; i < (it + 1)->second; i++) terms.push_back(inputs[i]);
  }

  const auto output_terms = cg.add(data.graph, terms);
  return std::move(cg).finalize(output_terms, pass_bys);
}

ContextualResult<std::pair<GraphContext, std::vector<Oterm>>, Program>
add_if_expr(Program p,
            GraphContext ctx,
            const AST& ast,
            const std::unordered_set<TypeID>& copy_types,
            const Map<ASTID, ASTID>& overloads,
            ASTID id) {
  const auto [cond_id, if_id, else_id] = ast.forest.child_ids(id).take<3>();

  auto cond_result = add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, cond_id);
  std::tie(p) = std::move(cond_result.state());

  auto if_result = create_graph(std::move(p), ast, copy_types, overloads, if_id);
  std::tie(p) = std::move(if_result.state());

  auto else_result = create_graph(std::move(p), ast, copy_types, overloads, else_id);
  std::tie(p) = std::move(else_result.state());

  return join(std::move(cond_result), std::move(if_result), std::move(else_result))
    .append_state(std::move(p))
    .map(flattened([&](auto pair, FunctionGraphData if_graph, FunctionGraphData else_graph, Program p) {
      auto [ctx, outer_terms] = std::move(pair);

      assert(outer_terms.size() == 1);
      const Grouping<ASTID, 3> owned_grouping = find_owned_grouping(
        if_graph.captured_values, else_graph.captured_values, if_graph.captured_borrows, else_graph.captured_borrows);
      const Grouping<ASTID, 3> borrowed_grouping =
        find_shared_grouping(if_graph.captured_borrows, else_graph.captured_borrows, owned_grouping[0]);

      outer_terms = append_binding_terms(ctx.bindings, owned_grouping.vec, std::move(outer_terms));
      outer_terms = append_binding_terms(ctx.bindings, borrowed_grouping.vec, std::move(outer_terms));

      std::array<i32, 2> value_term_sizes = {};
      std::array<i32, 2> borrow_term_sizes = {};

      for(i32 i = 0; i < 2; i++) {
        std::tie(value_term_sizes[i], borrow_term_sizes[i]) =
          find_offsets(ast.tg, ast.types, owned_grouping[i], borrowed_grouping[i]);
      }

      const i32 output_count = size_of(ast.tg, ast.types[id.get()]);

      const std::vector<PassBy> expr_pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[id.get()]);
      const Inst if_inst = p.add(reparameterize_graph(
        ast.tg,
        ast.types,
        if_graph,
        expr_pass_bys,
        std::array{owned_grouping[0], owned_grouping[1]},
        std::array{borrowed_grouping[0], borrowed_grouping[1]}));
      const Inst else_inst = p.add(reparameterize_graph(
        ast.tg,
        ast.types,
        else_graph,
        expr_pass_bys,
        std::array{owned_grouping[0], owned_grouping[2]},
        std::array{borrowed_grouping[0], borrowed_grouping[2]}));
      const Inst inst =
        p.add(IfInst{if_inst,
                     else_inst,
                     value_term_sizes[0],
                     value_term_sizes[0] + value_term_sizes[1],
                     borrow_term_sizes[0],
                     borrow_term_sizes[0] + borrow_term_sizes[1]},
              output_count);

      std::vector<PassBy> pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[cond_id.get()]);

      for(const ASTID id : owned_grouping.vec)
        pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[id.get()], std::move(pass_bys));

      for(const ASTID id : borrowed_grouping.vec)
        pass_bys.insert(pass_bys.end(), size_of(ast.tg, ast.types[id.get()]), PassBy::Borrow);

      std::vector<Oterm> output_terms = ctx.cg.add(inst, std::move(outer_terms), pass_bys, output_count);
      return std::tuple(std::pair(std::move(ctx), std::move(output_terms)), std::move(p));
    }));
}

ContextualResult<std::pair<GraphContext, std::vector<Oterm>>, Program>
add_call_expr(Program p,
              GraphContext ctx,
              const AST& ast,
              const std::unordered_set<TypeID>& copy_types,
              const Map<ASTID, ASTID>& overloads,
              ASTID id) {
  const auto [callee_id, arg_id] = ast.forest.child_ids(id).take<2>();

  return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, callee_id)
    .and_then(flattened([&](GraphContext ctx, std::vector<Oterm> callee_terms, Program p) {
      return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, arg_id)
        .map(flattened([&](GraphContext ctx, std::vector<Oterm> arg_terms, Program p) {
          return std::tuple(std::tuple(std::move(ctx), std::move(callee_terms), std::move(arg_terms)), std::move(p));
        }));
    }))
    .map(flattened([&](GraphContext ctx, auto callee_terms, auto arg_terms, Program p) {
      assert(callee_terms.size() == 1);
      arg_terms.insert(arg_terms.begin(), callee_terms.front());

      std::vector<PassBy> pass_bys;
      pass_bys.reserve(arg_terms.size());
      pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[callee_id.get()], std::move(pass_bys));
      pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[arg_id.get()], std::move(pass_bys));

      const int output_count = size_of(ast.tg, ast.types[id.get()]);
      std::vector<Oterm> terms = ctx.cg.add(p.add(FunctionalInst{}, output_count), arg_terms, pass_bys, output_count);
      return std::tuple(std::pair(std::move(ctx), std::move(terms)), std::move(p));
    }));
}

ContextualResult<std::pair<GraphContext, std::vector<Oterm>>, Program>
add_expr(Program p,
         GraphContext ctx,
         const AST& ast,
         const std::unordered_set<TypeID>& copy_types,
         const Map<ASTID, ASTID>& overloads,
         ASTID id) {
  switch(ast.forest[id]) {
  case ASTTag::PatternWildCard:
  case ASTTag::PatternIdent:
  case ASTTag::PatternTuple:
  case ASTTag::Fn:
  case ASTTag::Assignment:
  case ASTTag::ModuleRef:
  case ASTTag::Module:
  case ASTTag::EnvValue: assert(false); return {std::pair(std::move(ctx), std::vector<Oterm>{}), std::move(p)};
  case ASTTag::ExprLiteral: {
    std::vector<Oterm> terms =
      std::visit([&](const auto& v) { return ctx.cg.add(p.add(Any(v)), {}, {}, 1); }, lookup_literal(ast, id));
    return {std::pair(std::move(ctx), std::move(terms)), std::move(p)};
  }
  case ASTTag::ExprCall: return add_call_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, id);
  case ASTTag::ExprIf: return add_if_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, id);
  case ASTTag::ExprBorrow:
    return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, *ast.forest.first_child(id));
  case ASTTag::ExprWith: {
    const auto [assignment, expr] = ast.forest.child_ids(id).take<2>();
    const auto [pattern, assign_expr] = ast.forest.child_ids(assignment).take<2>();

    return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, assign_expr)
      .and_then(flattened([&](GraphContext ctx, auto assign_terms, Program p) {
        ctx = append_bindings(ast, pattern, assign_terms, std::move(ctx)).first;
        return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, expr);
      }));
  }
  case ASTTag::ExprTuple:
    return fold(ast.forest.child_ids(id),
                ContextualResult<std::pair<GraphContext, std::vector<Oterm>>, Program>{
                  std::pair(std::move(ctx), std::vector<Oterm>{}), std::move(p)},
                [&](auto result, ASTID tuple_ele) {
                  return std::move(result).and_then(flattened([&](GraphContext ctx, auto terms, Program p) {
                    return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, tuple_ele)
                      .map(flattened([&](GraphContext ctx, auto new_terms, Program p) {
                        return std::tuple(std::pair(std::move(ctx), to_vec(std::move(new_terms), std::move(terms))),
                                          std::move(p));
                      }));
                  }));
                });
  case ASTTag::ExprQualified:
  case ASTTag::ExprIdent: {
    const auto it = overloads.find(id);
    assert(it != overloads.end());
    assert(ctx.bindings.find(it->second) != ctx.bindings.end());
    std::vector<Oterm> terms = ctx.bindings.at(it->second);
    return {std::pair(std::move(ctx), std::move(terms)), std::move(p)};
  }
  }
  return {std::pair(std::move(ctx), std::vector<Oterm>{}), std::move(p)};
}

std::vector<ContextualError> find_borrow_move_dependency_errors(
  const Program& p, const FunctionGraph& g, const std::vector<SrcRef>& srcs, const Map<Oterm, ASTID>& oterm_srcs) {
  std::vector<ContextualError> errors;

  for(int node = 0; node < std::ssize(g.owned_fwds); node++) {
    for(int term = 0; term < std::ssize(g.owned_fwds[node]); term++) {
      const Oterm oterm{node, term};
      const ValueForward& fwd = g.owned_fwds[node][term];

      if(fwd.copy_end == fwd.move_end || fwd.move_end == fwd.terms.size()) continue;

      const auto ref_nodes = std::span(fwd.terms.begin() + fwd.move_end, fwd.terms.end()) |
                             stdr::views::transform([](Term t) { return t.node_id; });

      const int max_node = *stdr::max_element(ref_nodes);

      std::vector<int> stack{fwd.terms[fwd.copy_end].node_id};

      while(!stack.empty()) {
        const int node = stack.back();
        stack.pop_back();

        if(stdr::find(ref_nodes, node) != ref_nodes.end()) {
          errors.push_back({srcs[oterm_srcs.at(oterm).get()], "Dependency Error"});
          break;
        } else if(node < max_node) {
          for(const ValueForward& fwd : g.owned_fwds[node + 1]) {
            stack = transform_to_vec(
              fwd.terms, [](Term t) { return t.node_id; }, std::move(stack));
          }
        }
      }
    }
  }

  return errors;
}

} // namespace

ContextualResult<FunctionGraphData, Program>
create_graph(Program p,
             const AST& ast,
             const std::unordered_set<TypeID>& copy_types,
             const Map<ASTID, ASTID>& overloads,
             ASTID id) {
  const auto [pattern, expr] =
    ast.forest[id] == ASTTag::Fn ? ast.forest.child_ids(id).take<2>() : std::array{ASTID::Invalid(), id};

  std::vector<bool> borrows = pattern.is_valid() ? borrows_of(ast.tg, ast.types[pattern.get()]) : std::vector<bool>();

  auto [captured_values, captured_borrows] = find_captures(ast, overloads, id);

  for(const ASTID id : captured_values) {
    borrows = borrows_of(ast.tg, ast.types[id.get()], std::move(borrows));
  }

  for(const ASTID id : captured_borrows) {
    borrows.insert(borrows.end(), size_t(size_of(ast.tg, ast.types[id.get()])), true);
  }

  auto [cg, terms] = make_graph(std::move(borrows));

  GraphContext ctx = {std::move(cg)};
  int offset = 0;

  if(pattern.is_valid()) {
    std::tie(ctx, offset) = append_bindings(ast, pattern, terms, std::move(ctx), offset);
  }

  for(const ASTID id : captured_values) {
    std::tie(ctx, offset) = append_bindings(ast, id, terms, std::move(ctx), offset);
  }

  for(const ASTID id : captured_borrows) {
    std::tie(ctx, offset) = append_bindings(ast, id, terms, std::move(ctx), offset);
  }

  return add_expr(std::move(p), std::move(ctx), ast, copy_types, overloads, expr)
    .and_then(flattened([&](GraphContext ctx, std::vector<Oterm> terms, Program p) {
      FunctionGraph g = std::move(ctx.cg).finalize(terms, pass_bys_of(copy_types, ast.tg, ast.types[expr.get()]));

      auto errors = find_borrow_move_dependency_errors(p, g, ast.srcs, ctx.oterm_srcs);
      return value_or_errors(FunctionGraphData{std::move(captured_values), std::move(captured_borrows), std::move(g)},
                             std::move(errors),
                             std::move(p));
    }));
}

} // namespace ooze
