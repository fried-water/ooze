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

std::vector<bool> borrows_of(const TypeGraph& g, const Type& t, std::vector<bool> borrows = {}) {
  preorder(g, t, [&](Type t) {
    switch(g.get<TypeTag>(t)) {
    case TypeTag::Leaf:
    case TypeTag::Fn: borrows.push_back(false); return false;
    case TypeTag::Borrow: borrows.push_back(true); return false;
    case TypeTag::Floating: assert(false);
    case TypeTag::Tuple: return true;
    }
    assert(false);
    return false;
  });

  return borrows;
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

auto find_captures(const AST& ast, const Map<ASTID, ASTID>& binding_of, ASTID expr_id) {
  std::vector<ASTID> values;
  std::vector<ASTID> borrows;

  for(const ASTID id : ast.forest.pre_order_ids(expr_id)) {
    if(ast.forest[id] != ASTTag::ExprIdent) continue;

    const auto it = binding_of.find(id);
    assert(it != binding_of.end() && !ast.forest.is_root(it->second));

    const ASTID binding = it->second;

    const auto ancestors = ast.forest.ancestor_ids(binding);
    if(find(ancestors, expr_id) == ancestors.end()) {
      const auto parent = ast.forest.parent(id);
      const bool borrowed = parent && ast.forest[*parent] == ASTTag::ExprBorrow;

      if(borrowed) {
        borrows.push_back(binding);
      } else {
        values.push_back(binding);
      }
    }
  }

  values = unique(sorted(std::move(values)));
  borrows = remove_if(unique(sorted(std::move(borrows))), [&](ASTID id) { return find(values, id) != values.end(); });

  return std::pair(std::move(values), std::move(borrows));
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

std::pair<GraphContext, std::vector<Oterm>> add_expr(
  const AST&, const std::unordered_set<TypeID>& copy_types, const Map<ASTID, ASTID>& binding_of, ASTID, GraphContext);

std::pair<GraphContext, std::vector<Oterm>>
add_select_expr(const AST& ast,
                const std::unordered_set<TypeID>& copy_types,
                const Map<ASTID, ASTID>& binding_of,
                ASTID id,
                GraphContext ctx) {
  std::vector<Oterm> cond_terms;
  std::vector<Oterm> if_terms;
  std::vector<Oterm> else_terms;

  const auto [cond_id, if_id, else_id] = ast.forest.child_ids(id).take<3>();

  std::tie(ctx, cond_terms) = add_expr(ast, copy_types, binding_of, cond_id, std::move(ctx));
  std::tie(ctx, if_terms) = add_expr(ast, copy_types, binding_of, if_id, std::move(ctx));
  std::tie(ctx, else_terms) = add_expr(ast, copy_types, binding_of, else_id, std::move(ctx));

  assert(cond_terms.size() == 1);
  assert(if_terms.size() == else_terms.size());

  std::vector<PassBy> pass_bys;
  pass_bys.reserve(cond_terms.size() + if_terms.size() + else_terms.size());
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[cond_id.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[if_id.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[else_id.get()], std::move(pass_bys));

  std::vector<Oterm> terms =
    ctx.cg.add(ctx.p.add(SelectInst{}),
               flatten(std::move(cond_terms), std::move(if_terms), std::move(else_terms)),
               pass_bys,
               size_of(ast.tg, ast.types[id.get()]));
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

FunctionGraph wrap_conditional_branch(
  const TypeGraph& tg,
  const std::vector<Type>& ast_types,
  const FunctionGraphData& data,
  Span<PassBy> pass_bys,
  Span<ASTID> owned_common,
  Span<ASTID> owned_exclusive,
  Span<ASTID> borrowed_common,
  Span<ASTID> borrowed_exclusive) {
  std::vector<bool> input_borrows;

  std::vector<std::pair<ASTID, int>> offsets;

  for(const ASTID id : owned_common) {
    offsets.emplace_back(id, int(input_borrows.size()));
    input_borrows = borrows_of(tg, ast_types[id.get()], std::move(input_borrows));
  }

  for(const ASTID id : owned_exclusive) {
    offsets.emplace_back(id, int(input_borrows.size()));
    input_borrows = borrows_of(tg, ast_types[id.get()], std::move(input_borrows));
  }

  for(const ASTID id : borrowed_common) {
    offsets.emplace_back(id, int(input_borrows.size()));
    input_borrows.insert(input_borrows.end(), size_of(tg, ast_types[id.get()]), true);
  }

  for(const ASTID id : borrowed_exclusive) {
    offsets.emplace_back(id, int(input_borrows.size()));
    input_borrows.insert(input_borrows.end(), size_of(tg, ast_types[id.get()]), true);
  }

  offsets.emplace_back(ASTID::Invalid(), int(input_borrows.size()));

  auto [cg, inputs] = make_graph(std::move(input_borrows));

  std::vector<Oterm> terms;
  terms.reserve(inputs.size());

  for(const ASTID id : data.captured_values) {
    const auto it = find_if(offsets, [&](auto p) { return p.first == id; });
    for(int i = it->second; i < (it + 1)->second; i++) terms.push_back(inputs[i]);
  }

  for(const ASTID id : data.captured_borrows) {
    const auto it = find_if(offsets, [&](auto p) { return p.first == id; });
    for(int i = it->second; i < (it + 1)->second; i++) terms.push_back(inputs[i]);
  }

  const auto output_terms = cg.add(data.graph, terms);
  return std::move(cg).finalize(output_terms, pass_bys);
}

std::pair<GraphContext, std::vector<Oterm>>
add_if_expr(const AST& ast,
            const std::unordered_set<TypeID>& copy_types,
            const Map<ASTID, ASTID>& binding_of,
            ASTID id,
            GraphContext ctx) {
  const auto [cond_id, if_id, else_id] = ast.forest.child_ids(id).take<3>();

  std::vector<Oterm> outer_terms;
  std::tie(ctx, outer_terms) = add_expr(ast, copy_types, binding_of, cond_id, std::move(ctx));
  assert(outer_terms.size() == 1);

  FunctionGraphData if_graph = create_graph(std::move(ctx.p), ast, copy_types, binding_of, if_id);
  FunctionGraphData else_graph = create_graph(std::move(if_graph.program), ast, copy_types, binding_of, else_id);
  ctx.p = std::move(else_graph.program);

  const auto find_shared = [](const auto& v1, const auto& v2) {
    std::vector<ASTID> values;
    std::set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(values));
    const int shared_count = int(values.size());
    std::set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(values));
    const int v1_count = int(values.size());
    std::set_difference(v2.begin(), v2.end(), v1.begin(), v1.end(), std::back_inserter(values));
    return Grouping<ASTID, 3>{std::move(values), {shared_count, v1_count}};
  };

  const Grouping<ASTID, 3> owned_grouping = find_shared(if_graph.captured_values, else_graph.captured_values);
  const Grouping<ASTID, 3> borrowed_grouping = find_shared(if_graph.captured_borrows, else_graph.captured_borrows);

  std::array<int, 3> value_term_sizes = {};
  std::array<int, 3> borrow_term_sizes = {};

  for(int i = 0; i < 3; i++) {
    for(const ASTID id : owned_grouping[i]) {
      outer_terms = to_vec(ctx.bindings.at(id), std::move(outer_terms));
      const int size = size_of(ast.tg, ast.types[id.get()]);
      const int borrows = borrow_count(ast.tg, ast.types[id.get()]);

      borrow_term_sizes[i] += borrows;
      value_term_sizes[i] += size - borrows;
    }
  }

  for(int i = 0; i < 3; i++) {
    for(const ASTID id : borrowed_grouping[i]) {
      outer_terms = to_vec(ctx.bindings.at(id), std::move(outer_terms));
      borrow_term_sizes[i] += size_of(ast.tg, ast.types[id.get()]);
    }
  }

  const i32 output_count = size_of(ast.tg, ast.types[id.get()]);

  const std::vector<PassBy> expr_pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[id.get()]);
  const Inst if_inst = ctx.p.add(wrap_conditional_branch(
    ast.tg,
    ast.types,
    if_graph,
    expr_pass_bys,
    owned_grouping[0],
    owned_grouping[1],
    borrowed_grouping[0],
    borrowed_grouping[1]));
  const Inst else_inst = ctx.p.add(wrap_conditional_branch(
    ast.tg,
    ast.types,
    else_graph,
    expr_pass_bys,
    owned_grouping[0],
    owned_grouping[2],
    borrowed_grouping[0],
    borrowed_grouping[2]));
  const Inst inst = ctx.p.add(IfInst{
    if_inst,
    else_inst,
    output_count,
    value_term_sizes[0],
    value_term_sizes[0] + value_term_sizes[1],
    borrow_term_sizes[0],
    borrow_term_sizes[0] + borrow_term_sizes[1]});

  std::vector<PassBy> pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[cond_id.get()]);

  for(const ASTID id : owned_grouping.vec)
    pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[id.get()], std::move(pass_bys));

  for(const ASTID id : borrowed_grouping.vec)
    pass_bys.insert(pass_bys.end(), size_of(ast.tg, ast.types[id.get()]), PassBy::Borrow);

  std::vector<Oterm> output_terms = ctx.cg.add(inst, std::move(outer_terms), pass_bys, output_count);
  return {std::move(ctx), std::move(output_terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_call_expr(const AST& ast,
              const std::unordered_set<TypeID>& copy_types,
              const Map<ASTID, ASTID>& binding_of,
              ASTID id,
              GraphContext ctx) {
  std::vector<Oterm> callee_terms;
  std::vector<Oterm> arg_terms;

  const auto [callee, arg] = ast.forest.child_ids(id).take<2>();

  std::tie(ctx, callee_terms) = add_expr(ast, copy_types, binding_of, callee, std::move(ctx));
  std::tie(ctx, arg_terms) = add_expr(ast, copy_types, binding_of, arg, std::move(ctx));

  assert(callee_terms.size() == 1);
  arg_terms.insert(arg_terms.begin(), callee_terms.front());

  std::vector<PassBy> pass_bys;
  pass_bys.reserve(arg_terms.size());
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[callee.get()], std::move(pass_bys));
  pass_bys = pass_bys_of(copy_types, ast.tg, ast.types[arg.get()], std::move(pass_bys));

  const int output_count = size_of(ast.tg, ast.types[id.get()]);
  std::vector<Oterm> terms = ctx.cg.add(ctx.p.add(FunctionalInst{output_count}), arg_terms, pass_bys, output_count);
  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const AST& ast,
         const std::unordered_set<TypeID>& copy_types,
         const Map<ASTID, ASTID>& binding_of,
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
  case ASTTag::ExprCall: return add_call_expr(ast, copy_types, binding_of, id, std::move(ctx));
  case ASTTag::ExprSelect: return add_select_expr(ast, copy_types, binding_of, id, std::move(ctx));
  case ASTTag::ExprIf: return add_if_expr(ast, copy_types, binding_of, id, std::move(ctx));
  case ASTTag::ExprBorrow: return add_expr(ast, copy_types, binding_of, *ast.forest.first_child(id), std::move(ctx));
  case ASTTag::ExprWith: {
    const auto [assignment, expr] = ast.forest.child_ids(id).take<2>();
    const auto [pattern, assign_expr] = ast.forest.child_ids(assignment).take<2>();

    auto [assign_ctx, assign_terms] = add_expr(ast, copy_types, binding_of, assign_expr, std::move(ctx));
    ctx = append_bindings(ast, pattern, assign_terms, std::move(assign_ctx)).first;
    return add_expr(ast, copy_types, binding_of, expr, std::move(ctx));
  }
  case ASTTag::ExprTuple:
    return knot::accumulate(
      ast.forest.child_ids(id), std::pair(std::move(ctx), std::vector<Oterm>{}), [&](auto pair, ASTID tuple_element) {
        auto [ctx, terms] = add_expr(ast, copy_types, binding_of, tuple_element, std::move(pair.first));
        return std::pair(std::move(ctx), to_vec(std::move(terms), std::move(pair.second)));
      });
  case ASTTag::ExprQualified:
  case ASTTag::ExprIdent: {
    const auto it = binding_of.find(id);
    assert(it != binding_of.end());
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
                               const Map<ASTID, ASTID>& binding_of,
                               ASTID id) {
  const auto [pattern, expr] =
    ast.forest[id] == ASTTag::Fn ? ast.forest.child_ids(id).take<2>() : std::tuple(ASTID::Invalid(), id);

  std::vector<bool> borrows = pattern.is_valid() ? borrows_of(ast.tg, ast.types[pattern.get()]) : std::vector<bool>();

  auto [captured_values, captured_borrows] = find_captures(ast, binding_of, id);

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

  auto [final_ctx, output_terms] = add_expr(ast, copy_types, binding_of, expr, std::move(ctx));
  return {std::move(final_ctx.p),
          std::move(captured_values),
          std::move(captured_borrows),
          std::move(final_ctx.cg).finalize(output_terms, pass_bys_of(copy_types, ast.tg, ast.types[expr.get()]))};
}

} // namespace ooze
