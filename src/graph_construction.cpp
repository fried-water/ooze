#include "pch.h"

#include "graph_construction.h"
#include "graph_inner.h"
#include "ooze/tree.h"

#include <numeric>

namespace ooze {

namespace {

std::vector<TypeProperties> input_types(const CompoundType<TypeID>& type) {
  std::vector<TypeProperties> types;

  knot::preorder(
    type,
    Overloaded{
      [&](TypeID t) {
        types.push_back(TypeProperties{t, true});
        return false;
      },
      [&](const Borrow<TypeID>& t) {
        assert(std::holds_alternative<TypeID>(t.type->v));
        types.push_back(TypeProperties{std::get<TypeID>(t.type->v), false});
        return false;
      },
      [&](const FunctionType<TypeID>& t) {
        types.push_back(TypeProperties{type_id<FunctionGraph>(), true});
        return false;
      },
    });

  return types;
}

std::vector<TypeID> output_types(const CompoundType<TypeID>& type) {
  std::vector<TypeID> types;
  knot::preorder(type, [&](TypeID t) { types.push_back(t); });
  return types;
}

struct GraphContext {
  ConstructingGraph cg;
  std::vector<Map<std::string, std::tuple<CompoundType<TypeID>, std::vector<Oterm>>>> bindings;
};

struct GraphResult {
  std::vector<Oterm> terms;
  std::vector<TypeProperties> fn_input_types;
  std::vector<TypeID> fn_output_types;
};

GraphContext append_bindings(
  const ast::Pattern& pattern, const CompoundType<TypeID>& type, const std::vector<Oterm>& terms, GraphContext ctx) {
  int i = 0;
  co_visit(pattern,
           type,
           Overloaded{[&](const auto&, const auto& type, const ast::Ident& ident, const auto&) {
                        ctx.bindings.back()[ident.name] =
                          std::tuple(type, knot::preorder_accumulate(type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                                       v.push_back(terms[i++]);
                                       return v;
                                     }));
                      },
                      [&](const ast::WildCard&, const auto& type) { knot::preorder(type, [&](TypeID) { i++; }); }});
  return ctx;
}

std::pair<GraphContext, GraphResult> add_expr(const Env&, const CheckedExpr&, GraphContext);

std::pair<GraphContext, GraphResult> add_expr(const Env& e, const std::vector<CheckedExpr>& exprs, GraphContext ctx) {
  return knot::accumulate(exprs, std::pair(std::move(ctx), GraphResult{}), [&](auto pair, const CheckedExpr& expr) {
    auto [ctx, result] = add_expr(e, expr, std::move(pair.first));
    return std::pair(std::move(ctx), GraphResult{to_vec(std::move(result.terms), std::move(pair.second.terms))});
  });
}

std::pair<GraphContext, GraphResult>
add_expr(const Env& e, const ast::ScopeExpr<TypeID, EnvFunctionRef>& scope, GraphContext ctx) {
  ctx.bindings.emplace_back();

  ctx = knot::accumulate(scope.assignments, std::move(ctx), [&](GraphContext ctx, const CheckedAssignment& assignment) {
    GraphResult res;
    std::tie(ctx, res) = add_expr(e, *assignment.expr, std::move(ctx));
    return append_bindings(assignment.pattern, assignment.type, res.terms, std::move(ctx));
  });

  GraphResult res;
  std::tie(ctx, res) = add_expr(e, *scope.result, std::move(ctx));
  ctx.bindings.pop_back();

  return {std::move(ctx), std::move(res)};
}

std::pair<GraphContext, GraphResult>
add_expr(const Env& e, const ast::SelectExpr<TypeID, EnvFunctionRef>& select, GraphContext ctx) {
  GraphResult cond_result;
  GraphResult if_result;
  GraphResult else_result;

  std::tie(ctx, cond_result) = add_expr(e, *select.condition, std::move(ctx));
  std::tie(ctx, if_result) = add_expr(e, *select.if_expr, std::move(ctx));
  std::tie(ctx, else_result) = add_expr(e, *select.else_expr, std::move(ctx));

  assert(cond_result.terms.size() == 1);
  assert(if_result.terms.size() == else_result.terms.size());

  GraphResult result = {ctx.cg.add_select(cond_result.terms[0], if_result.terms, else_result.terms)};
  return {std::move(ctx), std::move(result)};
}

std::pair<GraphContext, GraphResult>
add_expr(const Env& e, const ast::CallExpr<TypeID, EnvFunctionRef>& call, GraphContext ctx) {
  GraphResult args_result;
  std::tie(ctx, args_result) = add_expr(e, *call.arg, std::move(ctx));
  if(const auto* ref = std::get_if<EnvFunctionRef>(&call.callee->v); ref) {
    const auto it = e.functions.find(ref->name);
    assert(it != e.functions.end() && ref->overload_idx < it->second.size());
    const EnvFunction& env_function = it->second[ref->overload_idx];

    GraphResult result = {std::visit(
      Overloaded{[&](const auto& f) { return ctx.cg.add(f, args_result.terms); },
                 [&](const TypedFunction&) {
                   const auto it = find_if(env_function.instatiations,
                                           applied([&](const auto& type, const auto&) { return ref->type == type; }));
                   assert(it != env_function.instatiations.end());
                   return ctx.cg.add(it->second, args_result.terms);
                 }},
      env_function.f)};
    return {std::move(ctx), std::move(result)};
  } else {
    auto [ctx2, callee_result] = add_expr(e, *call.callee, std::move(ctx));
    assert(callee_result.terms.size() == 1);
    GraphResult result = {ctx2.cg.add_functional(
      callee_result.fn_input_types,
      std::move(callee_result.fn_output_types),
      callee_result.terms.front(),
      args_result.terms)};
    return {std::move(ctx2), std::move(result)};
  }
}

std::pair<GraphContext, GraphResult>
add_expr(const Env& e, const ast::BorrowExpr<TypeID, EnvFunctionRef>& borrow, GraphContext ctx) {
  return add_expr(e, *borrow.expr, std::move(ctx));
}

std::pair<GraphContext, GraphResult> add_expr(const Env& e, const ast::Ident& ident, GraphContext ctx) {
  auto terms_types = std::accumulate(
    ctx.bindings.rbegin(),
    ctx.bindings.rend(),
    std::optional<std::tuple<CompoundType<TypeID>, std::vector<Oterm>>>{},
    [&](auto acc, const auto& bindings) {
      if(acc) {
        return acc;
      } else {
        const auto it = bindings.find(ident.name);
        return it != bindings.end() ? std::optional(it->second) : std::nullopt;
      }
    });

  assert(terms_types);

  GraphResult result = {std::move(std::get<1>(*terms_types))};

  knot::visit(std::get<0>(*terms_types).v, [&](const FunctionType<TypeID>& f) {
    result.fn_input_types = input_types(*f.input);
    result.fn_output_types = output_types(*f.output);
  });

  return {std::move(ctx), std::move(result)};
}

std::pair<GraphContext, GraphResult> add_expr(const Env& e, const Literal& literal, GraphContext ctx) {
  GraphResult result{
    std::visit([&](const auto& value) { return ctx.cg.add(AnyFunction([=]() { return value; }), {}); }, literal)};
  return {std::move(ctx), std::move(result)};
}

std::pair<GraphContext, GraphResult> add_expr(const Env& e, const EnvFunctionRef& fn_ref, GraphContext ctx) {
  const EnvFunction& ef = e.functions.at(fn_ref.name)[fn_ref.overload_idx];

  AnyFunction f = std::visit(
    Overloaded{[](const AnyFunction& f) { return AnyFunction([=]() { return make_graph(f); }); },
               [](const FunctionGraph& f) { return AnyFunction([=]() { return f; }); },
               [&](const TypedFunction&) {
                 const auto it = find_if(ef.instatiations, [&](const auto& p) { return fn_ref.type == p.first; });
                 assert(it != ef.instatiations.end());
                 return AnyFunction([f = it->second]() { return f; });
               }},
    ef.f);

  GraphResult result{ctx.cg.add(std::move(f), {}), input_types(*fn_ref.type.input), output_types(*fn_ref.type.output)};

  return {std::move(ctx), std::move(result)};
}

std::pair<GraphContext, GraphResult> add_expr(const Env& e, const CheckedExpr& expr, GraphContext ctx) {
  return std::visit([&](const auto& sub_expr) { return add_expr(e, sub_expr, std::move(ctx)); }, expr.v);
}

} // namespace

FunctionGraph create_graph(const Env& e, const CheckedFunction& f) {
  auto [cg, terms] = make_graph(input_types(*f.header.type.input));
  auto [ctx, result] = add_expr(
    e, f.expr, append_bindings(f.header.pattern, *f.header.type.input, terms, GraphContext{std::move(cg), {{}}}));
  return std::move(ctx.cg).finalize(result.terms);
}

} // namespace ooze
