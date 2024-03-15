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
  Program p;
  ConstructingGraph cg;
  Map<ASTID, std::vector<Oterm>> bindings;
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
      it += count;
    }
  }

  return {std::move(ctx), int(std::distance(terms.begin(), it))};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const AST&,
         const std::unordered_set<TypeID>& copy_types,
         const Map<ASTID, ASTID>& overloads,
         const Map<ASTID, std::vector<ASTID>>& loop_results,
         ASTID,
         GraphContext);

std::pair<GraphContext, std::vector<Oterm>> add_select_expr(
  const AST& ast,
  const std::unordered_set<TypeID>& copy_types,
  const Map<ASTID, ASTID>& overloads,
  const Map<ASTID, std::vector<ASTID>>& loop_results,
  ASTID id,
  GraphContext ctx) {
  std::vector<Oterm> cond_terms;
  std::vector<Oterm> if_terms;
  std::vector<Oterm> else_terms;

  const auto [cond_id, if_id, else_id] = ast.forest.child_ids(id).take<3>();

  std::tie(ctx, cond_terms) = add_expr(ast, copy_types, overloads, loop_results, cond_id, std::move(ctx));
  std::tie(ctx, if_terms) = add_expr(ast, copy_types, overloads, loop_results, if_id, std::move(ctx));
  std::tie(ctx, else_terms) = add_expr(ast, copy_types, overloads, loop_results, else_id, std::move(ctx));

  assert(cond_terms.size() == 1);
  assert(if_terms.size() == else_terms.size());

  std::vector<PassBy> pass_bys;
  pass_bys.reserve(cond_terms.size() + if_terms.size() + else_terms.size());
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[cond_id.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[if_id.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[else_id.get()], std::move(pass_bys));

  const int output_count = size_of(ast.tg, ast.types[id.get()]);
  std::vector<Oterm> terms =
    ctx.cg.add(ctx.p.add(SelectInst{}, output_count),
               flatten(std::move(cond_terms), std::move(if_terms), std::move(else_terms)),
               pass_bys,
               output_count);
  return {std::move(ctx), std::move(terms)};
}

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

Grouping<ASTID, 3> find_shared(Span<ASTID> v1, Span<ASTID> v2) {
  std::vector<ASTID> values = filter_to_vec(v1, contained_fn(v2));
  const int shared_count = int(values.size());
  values = filter_to_vec(v1, std::not_fn(contained_fn(v2)), std::move(values));
  const int v1_count = int(values.size());
  values = filter_to_vec(v2, std::not_fn(contained_fn(v1)), std::move(values));
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

std::pair<GraphContext, std::vector<Oterm>>
add_if_expr(const AST& ast,
            const std::unordered_set<TypeID>& copy_types,
            const Map<ASTID, ASTID>& overloads,
            const Map<ASTID, std::vector<ASTID>>& loop_results,
            ASTID id,
            GraphContext ctx) {
  const auto [cond_id, if_id, else_id] = ast.forest.child_ids(id).take<3>();

  FunctionGraphData if_graph = create_graph(std::move(ctx.p), ast, copy_types, overloads, loop_results, if_id);
  FunctionGraphData else_graph =
    create_graph(std::move(if_graph.program), ast, copy_types, overloads, loop_results, else_id);
  ctx.p = std::move(else_graph.program);

  const Grouping<ASTID, 3> owned_grouping = find_shared(if_graph.captured_values, else_graph.captured_values);
  const Grouping<ASTID, 3> borrowed_grouping = find_shared(if_graph.captured_borrows, else_graph.captured_borrows);

  std::vector<Oterm> outer_terms;
  std::tie(ctx, outer_terms) = add_expr(ast, copy_types, overloads, loop_results, cond_id, std::move(ctx));
  assert(outer_terms.size() == 1);

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
  const Inst if_inst = ctx.p.add(reparameterize_graph(
    ast.tg,
    ast.types,
    if_graph,
    expr_pass_bys,
    std::array{owned_grouping[0], owned_grouping[1]},
    std::array{borrowed_grouping[0], borrowed_grouping[1]}));
  const Inst else_inst = ctx.p.add(reparameterize_graph(
    ast.tg,
    ast.types,
    else_graph,
    expr_pass_bys,
    std::array{owned_grouping[0], owned_grouping[2]},
    std::array{borrowed_grouping[0], borrowed_grouping[2]}));
  const Inst inst = ctx.p.add(
    IfInst{if_inst,
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
  return {std::move(ctx), std::move(output_terms)};
}

Grouping<ASTID, 5> find_while_grouping(Span<ASTID> results, Span<ASTID> cond, Span<ASTID> body) {
  std::vector<ASTID> values = to_vec(body);

  const auto ret_it = std::stable_partition(values.begin(), values.end(), contained_fn(results));
  const auto ret_both_it = std::stable_partition(values.begin(), ret_it, contained_fn(cond));
  const auto inv_both_it = std::stable_partition(ret_it, values.end(), contained_fn(cond));

  const int ret_both = int(std::distance(values.begin(), ret_both_it));
  const int ret = int(std::distance(values.begin(), ret_it));
  const int inv_both = int(std::distance(values.begin(), inv_both_it));

  for(const ASTID id : cond) {
    if(!contained_fn(std::span(values.data(), ret_both))(id)) {
      values.push_back(id);
    }
  }

  return {std::move(values), {ret_both, ret, inv_both, int(body.size())}};
}

std::pair<GraphContext, std::vector<Oterm>> add_while_expr(
  const AST& ast,
  const std::unordered_set<TypeID>& copy_types,
  const Map<ASTID, ASTID>& overloads,
  const Map<ASTID, std::vector<ASTID>>& loop_results,
  ASTID id,
  GraphContext ctx) {
  const auto [cond_id, body_id] = ast.forest.child_ids(id).take<2>();

  FunctionGraphData cond_graph = create_graph(std::move(ctx.p), ast, copy_types, overloads, loop_results, cond_id);
  FunctionGraphData body_graph =
    create_graph(std::move(cond_graph.program), ast, copy_types, overloads, loop_results, body_id);
  ctx.p = std::move(body_graph.program);

  const auto it = loop_results.find(id);
  assert(it != loop_results.end());
  const Grouping<ASTID, 5> owned_grouping =
    find_while_grouping(it->second, cond_graph.captured_values, body_graph.captured_values);

  const Grouping<ASTID, 3> borrowed_grouping =
    find_shared(sorted(std::move(body_graph.captured_borrows)), sorted(std::move(cond_graph.captured_borrows)));

  const i32 output_count = size_of(ast.tg, ast.types[id.get()]);

  const Inst cond_inst = ctx.p.add(reparameterize_graph(
    ast.tg,
    ast.types,
    cond_graph,
    std::array{PassBy::Copy},
    std::array{owned_grouping[0], owned_grouping[2], owned_grouping[4]},
    std::array{borrowed_grouping[0], borrowed_grouping[2]}));

  const Inst body_inst = ctx.p.add(reparameterize_graph(
    ast.tg,
    ast.types,
    body_graph,
    pass_bys_of(copy_types, ast.tg, ast.types[id.get()]),
    std::array{owned_grouping[0], owned_grouping[1], owned_grouping[2], owned_grouping[3]},
    std::array{borrowed_grouping[0], borrowed_grouping[1]}));

  // [ret_common, ret_body, inv_common, inv_body, inv_cond]
  std::array<i32, 4> value_offsets = {};

  // [common, body, cond]
  std::array<i32, 2> borrow_offsets = {};

  std::tie(value_offsets[0], std::ignore) = find_offsets(ast.tg, ast.types, owned_grouping[0], {});
  std::tie(value_offsets[1], std::ignore) = find_offsets(ast.tg, ast.types, owned_grouping[1], {}, value_offsets[0], 0);

  for(i32 i = 0; i < 2; i++) {
    std::tie(value_offsets[i + 2], borrow_offsets[i]) = find_offsets(
      ast.tg,
      ast.types,
      owned_grouping[i + 2],
      borrowed_grouping[i],
      value_offsets[i + 1],
      i == 0 ? 0 : borrow_offsets[i - 1]);
  }

  const Inst while_inst = ctx.p.add(WhileInst{cond_inst, body_inst, value_offsets, borrow_offsets}, output_count);

  std::vector<Oterm> outer_terms = append_binding_terms(ctx.bindings, owned_grouping.vec, {});
  outer_terms = append_binding_terms(ctx.bindings, borrowed_grouping.vec, std::move(outer_terms));

  std::vector<PassBy> pass_bys =
    fold(owned_grouping.vec, std::vector<PassBy>{}, [&](std::vector<PassBy> pbs, ASTID id) {
      return pass_bys_of(copy_types, ast.tg, ast.types[id.get()], std::move(pbs));
    });

  pass_bys = fold(borrowed_grouping.vec, std::move(pass_bys), [&](std::vector<PassBy> pbs, ASTID id) {
    pbs.insert(pbs.end(), size_of(ast.tg, ast.types[id.get()]), PassBy::Borrow);
    return pbs;
  });

  std::vector<Oterm> output_terms = ctx.cg.add(while_inst, std::move(outer_terms), pass_bys, output_count);
  return {std::move(ctx), std::move(output_terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_call_expr(const AST& ast,
              const std::unordered_set<TypeID>& copy_types,
              const Map<ASTID, ASTID>& overloads,
              const Map<ASTID, std::vector<ASTID>>& loop_results,
              ASTID id,
              GraphContext ctx) {
  std::vector<Oterm> callee_terms;
  std::vector<Oterm> arg_terms;

  const auto [callee, arg] = ast.forest.child_ids(id).take<2>();

  std::tie(ctx, callee_terms) = add_expr(ast, copy_types, overloads, loop_results, callee, std::move(ctx));
  std::tie(ctx, arg_terms) = add_expr(ast, copy_types, overloads, loop_results, arg, std::move(ctx));

  assert(callee_terms.size() == 1);
  arg_terms.insert(arg_terms.begin(), callee_terms.front());

  std::vector<PassBy> pass_bys;
  pass_bys.reserve(arg_terms.size());
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[callee.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[arg.get()], std::move(pass_bys));

  const int output_count = size_of(ast.tg, ast.types[id.get()]);
  std::vector<Oterm> terms = ctx.cg.add(ctx.p.add(FunctionalInst{}, output_count), arg_terms, pass_bys, output_count);
  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const AST& ast,
         const std::unordered_set<TypeID>& copy_types,
         const Map<ASTID, ASTID>& overloads,
         const Map<ASTID, std::vector<ASTID>>& loop_results,
         ASTID id,
         GraphContext ctx) {
  switch(ast.forest[id]) {
  case ASTTag::PatternWildCard:
  case ASTTag::PatternIdent:
  case ASTTag::PatternTuple:
  case ASTTag::Fn:
  case ASTTag::Assignment:
  case ASTTag::ModuleRef:
  case ASTTag::Module:
  case ASTTag::EnvValue: assert(false); return {};
  case ASTTag::ExprLiteral: {
    std::vector<Oterm> terms =
      std::visit([&](const auto& v) { return ctx.cg.add(ctx.p.add(Any(v)), {}, {}, 1); }, lookup_literal(ast, id));
    return {std::move(ctx), std::move(terms)};
  }
  case ASTTag::ExprCall: return add_call_expr(ast, copy_types, overloads, loop_results, id, std::move(ctx));
  case ASTTag::ExprSelect: return add_select_expr(ast, copy_types, overloads, loop_results, id, std::move(ctx));
  case ASTTag::ExprIf: return add_if_expr(ast, copy_types, overloads, loop_results, id, std::move(ctx));
  case ASTTag::ExprWhile: return add_while_expr(ast, copy_types, overloads, loop_results, id, std::move(ctx));
  case ASTTag::ExprBorrow:
    return add_expr(ast, copy_types, overloads, loop_results, *ast.forest.first_child(id), std::move(ctx));
  case ASTTag::ExprWith: {
    const auto [assignment, expr] = ast.forest.child_ids(id).take<2>();
    const auto [pattern, assign_expr] = ast.forest.child_ids(assignment).take<2>();

    auto [assign_ctx, assign_terms] = add_expr(ast, copy_types, overloads, loop_results, assign_expr, std::move(ctx));
    ctx = append_bindings(ast, pattern, assign_terms, std::move(assign_ctx)).first;
    return add_expr(ast, copy_types, overloads, loop_results, expr, std::move(ctx));
  }
  case ASTTag::ExprTuple:
    return fold(ast.forest.child_ids(id), std::pair(std::move(ctx), std::vector<Oterm>{}), [&](auto pair, ASTID tuple_ele) {
        auto [ctx, terms] = add_expr(ast, copy_types, overloads, loop_results, tuple_ele, std::move(pair.first));
        return std::pair(std::move(ctx), to_vec(std::move(terms), std::move(pair.second)));
      });
  case ASTTag::ExprQualified:
  case ASTTag::ExprIdent: {
    const auto it = overloads.find(id);
    assert(it != overloads.end());
    assert(ctx.bindings.find(it->second) != ctx.bindings.end());
    std::vector<Oterm> terms = ctx.bindings.at(it->second);
    return {std::move(ctx), std::move(terms)};
  }
  }
  return {};
}

} // namespace

FunctionGraphData create_graph(Program p,
                               const AST& ast,
                               const std::unordered_set<TypeID>& copy_types,
                               const Map<ASTID, ASTID>& overloads,
                               const Map<ASTID, std::vector<ASTID>>& loop_results,
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

  GraphContext ctx = {std::move(p), std::move(cg)};
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

  auto [final_ctx, output_terms] = add_expr(ast, copy_types, overloads, loop_results, expr, std::move(ctx));
  return {std::move(final_ctx.p),
          std::move(captured_values),
          std::move(captured_borrows),
          std::move(final_ctx.cg).finalize(output_terms, pass_bys_of(copy_types, ast.tg, ast.types[expr.get()]))};
}

} // namespace ooze
