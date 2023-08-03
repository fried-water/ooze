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
  std::vector<Oterm> terms;
  std::vector<TypeProperties> fn_input_types;
  std::vector<TypeID> fn_output_types;
};

GraphContext append_bindings(const ast::Pattern& pattern, const CompoundType<TypeID>& type, GraphContext ctx) {
  int i = 0;
  co_visit(pattern,
           type,
           Overloaded{[&](const auto&, const auto& type, const ast::Ident& ident, const auto&) {
                        ctx.bindings.back()[ident.name] =
                          std::tuple(type, knot::preorder_accumulate(type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                                       v.push_back(ctx.terms[i++]);
                                       return v;
                                     }));
                      },
                      [&](const ast::WildCard&, const auto& type) { knot::preorder(type, [&](TypeID) { i++; }); }});

  ctx.terms = {};

  return ctx;
}

GraphContext add_expr(const Env&, const CheckedExpr&, GraphContext);

GraphContext add_expr(const Env& e, const std::vector<CheckedExpr>& exprs, GraphContext ctx) {
  std::vector<Oterm> terms;

  for(const CheckedExpr& expr : exprs) {
    ctx = add_expr(e, expr, std::move(ctx));
    terms.insert(terms.end(), ctx.terms.begin(), ctx.terms.end());
  }

  ctx.terms = std::move(terms);

  return ctx;
}

GraphContext add_expr(const Env& e, const ast::ScopeExpr<TypeID, EnvFunctionRef>& scope, GraphContext ctx) {
  ctx.bindings.emplace_back();

  ctx = knot::accumulate(scope.assignments, std::move(ctx), [&](GraphContext ctx, const CheckedAssignment& assignment) {
    ctx = add_expr(e, *assignment.expr, std::move(ctx));
    return append_bindings(assignment.pattern, assignment.type, std::move(ctx));
  });
  ctx = add_expr(e, *scope.result, std::move(ctx));
  ctx.bindings.pop_back();

  return ctx;
}

GraphContext add_expr(const Env& e, const ast::CallExpr<TypeID, EnvFunctionRef>& call, GraphContext ctx) {
  ctx = add_expr(e, *call.arg, std::move(ctx));
  if(const auto* ref = std::get_if<EnvFunctionRef>(&call.callee->v); ref) {
    const auto it = e.functions.find(ref->name);
    assert(it != e.functions.end() && ref->overload_idx < it->second.size());
    const EnvFunction& env_function = it->second[ref->overload_idx];

    ctx.terms = std::visit(
      Overloaded{[&](const auto& f) { return ctx.cg.add(f, ctx.terms); },
                 [&](const TypedFunction&) {
                   const auto it = find_if(env_function.instatiations,
                                           applied([&](const auto& type, const auto&) { return ref->type == type; }));
                   assert(it != env_function.instatiations.end());
                   return ctx.cg.add(it->second, ctx.terms);
                 }},
      env_function.f);
    return ctx;
  } else {
    std::vector<Oterm> fn_terms = std::move(ctx.terms);
    ctx = add_expr(e, *call.callee, std::move(ctx));
    assert(ctx.terms.size() == 1);
    ctx.terms = ctx.cg.add_functional(ctx.fn_input_types, std::move(ctx.fn_output_types), ctx.terms.front(), fn_terms);
    ctx.fn_input_types = {};
    return ctx;
  }
}

GraphContext add_expr(const Env& e, const ast::BorrowExpr<TypeID, EnvFunctionRef>& borrow, GraphContext ctx) {
  return add_expr(e, *borrow.expr, std::move(ctx));
}

GraphContext add_expr(const Env& e, const ast::Ident& ident, GraphContext ctx) {
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

  ctx.terms = std::move(std::get<1>(*terms_types));

  knot::visit(std::get<0>(*terms_types).v, [&](const FunctionType<TypeID>& f) {
    ctx.fn_input_types = input_types(*f.input);
    ctx.fn_output_types = output_types(*f.output);
  });

  return ctx;
}

GraphContext add_expr(const Env& e, const Literal& literal, GraphContext ctx) {
  ctx.terms =
    std::visit([&](const auto& value) { return ctx.cg.add(AnyFunction([=]() { return value; }), {}); }, literal);
  return ctx;
}

GraphContext add_expr(const Env& e, const EnvFunctionRef& fn_ref, GraphContext ctx) {
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

  ctx.terms = ctx.cg.add(std::move(f), {});
  ctx.fn_input_types = input_types(*fn_ref.type.input);
  ctx.fn_output_types = output_types(*fn_ref.type.output);

  return ctx;
}

GraphContext add_expr(const Env& e, const CheckedExpr& expr, GraphContext ctx) {
  return std::visit([&](const auto& sub_expr) { return add_expr(e, sub_expr, std::move(ctx)); }, expr.v);
}

} // namespace

FunctionGraph create_graph(const Env& e, const CheckedFunction& f) {
  auto [cg, terms] = make_graph(input_types(*f.header.type.input));
  GraphContext ctx = add_expr(
    e,
    f.expr,
    append_bindings(f.header.pattern, *f.header.type.input, GraphContext{std::move(cg), {{}}, std::move(terms)}));
  return std::move(ctx.cg).finalize(ctx.terms);
}

} // namespace ooze
