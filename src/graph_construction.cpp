#include "pch.h"

#include "graph_construction.h"
#include "ooze/tree.h"
#include "pretty_print.h"

#include <numeric>

namespace ooze {

namespace {

using anyf::FunctionGraph;
using anyf::Oterm;

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
        types.push_back(TypeProperties{anyf::type_id<anyf::FunctionGraph>(), true});
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
  anyf::ConstructingGraph cg;
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

ContextualResult<GraphContext> add_expr(const Env&, const CheckedExpr&, GraphContext);

ContextualResult<GraphContext> add_expr(const Env& e, const std::vector<CheckedExpr>& exprs, GraphContext ctx) {
  std::vector<Oterm> terms;

  for(const CheckedExpr& expr : exprs) {
    if(auto r = add_expr(e, expr, std::move(ctx)); r) {
      ctx = std::move(*r);
      terms.insert(terms.end(), ctx.terms.begin(), ctx.terms.end());
    } else {
      return Failure{std::move(r.error())};
    }
  }

  ctx.terms = std::move(terms);

  return ctx;
}

ContextualResult<GraphContext>
add_expr(const Env& e, const ast::ScopeExpr<TypeID, EnvFunctionRef>& scope, GraphContext ctx) {
  ctx.bindings.emplace_back();

  return knot::accumulate(scope.assignments,
                          ContextualResult<GraphContext>{std::move(ctx)},
                          [&](auto acc, const CheckedAssignment& assignment) {
                            return std::move(acc).and_then([&](GraphContext ctx) {
                              return add_expr(e, *assignment.expr, std::move(ctx)).map([&](GraphContext ctx) {
                                return append_bindings(assignment.pattern, assignment.type, std::move(ctx));
                              });
                            });
                          })
    .and_then([&](GraphContext ctx) { return add_expr(e, *scope.result, std::move(ctx)); })
    .map([](GraphContext ctx) {
      ctx.bindings.pop_back();
      return ctx;
    });
}

ContextualResult<GraphContext>
add_expr(const Env& e, const ast::CallExpr<TypeID, EnvFunctionRef>& call, GraphContext ctx) {
  return add_expr(e, *call.arg, std::move(ctx)).and_then([&](GraphContext ctx) {
    return std::visit(
      Overloaded{
        [&](const EnvFunctionRef& ef) {
          const auto it = e.functions.find(ef.name);
          assert(it != e.functions.end() && ef.overload_idx < it->second.size());
          const EnvFunction& env_function = it->second[ef.overload_idx];

          ctx.terms =
            std::visit(Overloaded{[&](const auto& f) { return ctx.cg.add(f, ctx.terms).value(); },
                                  [&](const TypedFunction&) {
                                    const auto it =
                                      find_if(env_function.instatiations,
                                              applied([&](const auto& type, const auto&) { return ef.type == type; }));
                                    assert(it != env_function.instatiations.end());
                                    return ctx.cg.add(it->second, ctx.terms).value();
                                  }},
                       env_function.f);
          return ContextualResult<GraphContext>(std::move(ctx));
        },
        [&](const auto&) {
          std::vector<Oterm> fn_terms = std::move(ctx.terms);
          return add_expr(e, *call.callee, std::move(ctx)).map([&](GraphContext ctx) {
            assert(ctx.terms.size() == 1);
            ctx.terms =
              ctx.cg.add_functional(ctx.fn_input_types, std::move(ctx.fn_output_types), ctx.terms.front(), fn_terms)
                .value();
            ctx.fn_input_types = {};
            return ctx;
          });
        }},
      call.callee->v);
  });
}

ContextualResult<GraphContext>
add_expr(const Env& e, const ast::BorrowExpr<TypeID, EnvFunctionRef>& borrow, GraphContext ctx) {
  return add_expr(e, *borrow.expr, std::move(ctx));
}

ContextualResult<GraphContext> add_expr(const Env& e, const ast::Ident& ident, GraphContext ctx) {
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

ContextualResult<GraphContext> add_expr(const Env& e, const Literal& literal, GraphContext ctx) {
  ctx.terms = std::visit(
    [&](const auto& value) { return ctx.cg.add(AnyFunction([=]() { return value; }), {}).value(); }, literal);
  return ctx;
}

ContextualResult<GraphContext> add_expr(const Env& e, const EnvFunctionRef& fn_ref, GraphContext ctx) {
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

  ctx.terms = ctx.cg.add(std::move(f), {}).value();
  ctx.fn_input_types = input_types(*fn_ref.type.input);
  ctx.fn_output_types = output_types(*fn_ref.type.output);

  return ctx;
}

ContextualResult<GraphContext> add_expr(const Env& e, const CheckedExpr& expr, GraphContext ctx) {
  return std::visit([&](const auto& sub_expr) { return add_expr(e, sub_expr, std::move(ctx)); }, expr.v);
}

} // namespace

ContextualResult<anyf::FunctionGraph> create_graph(const Env& e, const CheckedFunction& f) {
  auto [cg, terms] = anyf::make_graph(input_types(*f.header.type.input));
  return add_expr(
           e,
           f.expr,
           append_bindings(f.header.pattern, *f.header.type.input, GraphContext{std::move(cg), {{}}, std::move(terms)}))
    .map([&](GraphContext ctx) { return std::move(ctx.cg).finalize(ctx.terms).value(); });
}

} // namespace ooze
