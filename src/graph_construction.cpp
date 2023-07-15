#include "pch.h"

#include "graph_construction.h"
#include "ooze/tree.h"

#include <numeric>

namespace ooze {

namespace {

using anyf::FunctionGraph;
using anyf::Oterm;

template <typename T, typename E>
Result<T, E> convert(tl::expected<T, E> exp) {
  return exp ? Result<T, E>{std::move(exp.value())} : Failure{std::move(exp.error())};
}

std::vector<ContextualError> to_graph_error(anyf::GraphError error, const CheckedExpr& expr, bool indexed = true) {
  return std::visit(
    Overloaded{[&](const CheckedScopeExpr& s) { return to_graph_error(error, *s.result, indexed); },
               [&](const std::vector<CheckedExpr>& exprs) {
                 return indexed ? to_graph_error(error, exprs[std::get<anyf::AlreadyMoved>(error).index], false)
                                : std::vector{ContextualError{expr.ref, "binding already moved"}};
               },
               [&](const auto&) {
                 return std::vector{ContextualError{expr.ref, "binding already moved"}};
               }},
    expr.v);
}

std::vector<ContextualError> to_graph_error(anyf::GraphError error, const std::vector<CheckedExpr>& exprs) {
  return to_graph_error(error, exprs[std::get<anyf::AlreadyMoved>(error).index], false);
}

struct GraphContext {
  anyf::ConstructingGraph cg;
  std::vector<Map<std::string, std::vector<Oterm>>> bindings;
  std::vector<Oterm> terms;
};

GraphContext append_bindings(const ast::Pattern& pattern, const CompoundType<TypeID>& type, GraphContext ctx) {
  int i = 0;
  co_visit(pattern,
           type,
           Overloaded{[&](const ast::Ident& ident, const auto& type) {
                        ctx.bindings.back()[ident.name] =
                          knot::preorder_accumulate(type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                            v.push_back(ctx.terms[i++]);
                            return v;
                          });
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
  return add_expr(e, call.parameters, std::move(ctx)).and_then([&](GraphContext ctx) {
    const auto it = e.functions.find(call.function.name);
    assert(it != e.functions.end() && call.function.overload_idx < it->second.size());
    const EnvFunction& env_function = it->second[call.function.overload_idx];

    return std::visit(Overloaded{[&](const auto& f) { return convert(ctx.cg.add(f, ctx.terms)); },
                                 [&](const TypedFunction&) {
                                   const auto it =
                                     find_if(env_function.instatiations, applied([&](const auto& type, const auto&) {
                                               return call.function.type == type;
                                             }));
                                   assert(it != env_function.instatiations.end());
                                   return convert(ctx.cg.add(it->second, ctx.terms));
                                 }},
                      env_function.f)
      .map_error([&](anyf::GraphError error) { return to_graph_error(error, call.parameters); })
      .map([&](std::vector<Oterm> terms) {
        ctx.terms = std::move(terms);
        return std::move(ctx);
      });
  });
}

ContextualResult<GraphContext>
add_expr(const Env& e, const ast::BorrowExpr<TypeID, EnvFunctionRef>& borrow, GraphContext ctx) {
  return add_expr(e, *borrow.expr, std::move(ctx));
}

ContextualResult<GraphContext> add_expr(const Env& e, const ast::Ident& ident, GraphContext ctx) {
  auto terms = std::accumulate(
    ctx.bindings.rbegin(),
    ctx.bindings.rend(),
    std::optional<std::vector<Oterm>>{},
    [&](auto acc, const Map<std::string, std::vector<Oterm>>& bindings) {
      if(acc) {
        return acc;
      } else {
        const auto it = bindings.find(ident.name);
        return it != bindings.end() ? std::optional(it->second) : std::nullopt;
      }
    });

  assert(terms);

  ctx.terms = std::move(*terms);

  return ctx;
}

ContextualResult<GraphContext> add_expr(const Env& e, const Literal& literal, GraphContext ctx) {
  return std::visit(
    [&](const auto& value) {
      return convert(ctx.cg.add(AnyFunction([=]() { return value; }), {}))
        .map([&](std::vector<Oterm> terms) {
          ctx.terms = std::move(terms);
          return std::move(ctx);
        })
        .map_error([](const auto&) -> std::vector<ContextualError> {
          assert(false); // shouldnt ever fail
          exit(1);
        });
    },
    literal);
}

ContextualResult<GraphContext> add_expr(const Env& e, const CheckedExpr& expr, GraphContext ctx) {
  return std::visit([&](const auto& sub_expr) { return add_expr(e, sub_expr, std::move(ctx)); }, expr.v);
}

} // namespace

ContextualResult<anyf::FunctionGraph> create_graph(const Env& e, const CheckedFunction& f) {
  std::vector<TypeProperties> input_types;

  knot::preorder(
    *f.header.type.input,
    Overloaded{
      [&](TypeID t) {
        input_types.push_back(TypeProperties{t, true});
        return false;
      },
      [&](Borrow<TypeID> t) {
        assert(std::holds_alternative<TypeID>(t.type->v));
        input_types.push_back(TypeProperties{std::get<TypeID>(t.type->v), false});
        return false;
      },
    });

  auto [cg, terms] = anyf::make_graph(input_types);

  return add_expr(
           e,
           f.expr,
           append_bindings(f.header.pattern, *f.header.type.input, GraphContext{std::move(cg), {{}}, std::move(terms)}))
    .and_then([&](GraphContext ctx) {
      return convert(std::move(ctx.cg).finalize(ctx.terms).map_error([&](anyf::GraphError error) {
        return to_graph_error(error, f.expr);
      }));
    });
}

} // namespace ooze
