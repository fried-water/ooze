#include "pch.h"

#include "graph_construction.h"
#include "ooze/tree.h"
#include "queries.h"

namespace ooze {

namespace {

using anyf::FunctionGraph;
using anyf::Oterm;

auto to_graph_error(const std::vector<CheckedExpr>& exprs) {
  return [&](anyf::GraphError error) {
    const anyf::AlreadyMoved* err = std::get_if<anyf::AlreadyMoved>(&error);
    assert(err);

    return std::vector{ContextualError{exprs[err->index].ref, "binding already moved"}};
  };
}

auto to_graph_error(Slice ref) {
  return [=](anyf::GraphError error) {
    const anyf::AlreadyMoved* err = std::get_if<anyf::AlreadyMoved>(&error);
    assert(err);

    return std::vector{ContextualError{ref, "binding already moved"}};
  };
}

struct GraphContext {
  anyf::ConstructingGraph cg;
  Map<std::string, std::vector<Oterm>> bindings;
};

ContextualResult<std::vector<Oterm>> add_expr(GraphContext&, const CheckedExpr&, const Env&);

ContextualResult<std::vector<Oterm>>
accumulate_exprs(GraphContext& ctx, const std::vector<CheckedExpr>& exprs, const Env& e) {
  auto [terms, errors] = knot::accumulate(
    exprs, std::pair<std::vector<Oterm>, std::vector<ContextualError>>{}, [&](auto acc, const CheckedExpr& expr) {
      if(auto terms = add_expr(ctx, expr, e); terms) {
        acc.first.insert(acc.first.end(), terms->begin(), terms->end());
      } else {
        acc.second.insert(acc.second.end(), terms.error().begin(), terms.error().end());
      }
      return acc;
    });

  return value_or_errors(std::move(terms), std::move(errors));
}

ContextualResult<std::vector<Oterm>> add_expr(GraphContext& ctx, const CheckedExpr& expr, const Env& e) {
  return std::visit(
    Overloaded{[&](const ast::Call<EnvFunctionRef>& call) -> ContextualResult<std::vector<Oterm>> {
                 return accumulate_exprs(ctx, call.parameters, e)
                   .and_then([&](std::vector<Oterm> terms) -> ContextualResult<std::vector<Oterm>> {
                     const auto it = e.functions.find(call.function.name);
                     assert(it != e.functions.end() && call.function.overload_idx < it->second.size());

                     return std::visit(
                       [&](const auto& f) { return ctx.cg.add(f, terms).map_error(to_graph_error(call.parameters)); },
                       it->second[call.function.overload_idx].f);
                   });
               },
               [&](const ast::IdentExpr& ident) -> ContextualResult<std::vector<Oterm>> {
                 return ctx.bindings.find(ident.name)->second;
               },
               [&](const Literal& literal) -> ContextualResult<std::vector<Oterm>> {
                 return std::visit(
                   [&](const auto& value) {
                     return ctx.cg.add(AnyFunction([=]() { return value; }), {})
                       .map_error([](const auto&) -> std::vector<ContextualError> {
                         assert(false); // shouldnt ever fail
                         exit(1);
                       });
                   },
                   literal);
               },
               [&](const std::vector<CheckedExpr>& exprs) { return accumulate_exprs(ctx, exprs, e); },
               [&](const ast::BorrowExpr<EnvFunctionRef>& borrow) { return add_expr(ctx, *borrow.e, e); }},
    expr.v);
}

} // namespace

ContextualResult<anyf::FunctionGraph> create_graph(const Env& e, const CheckedFunction& f) {
  std::vector<TypeProperties> input_types;

  knot::preorder(*f.header.type.input,
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

  return ContextualResult<std::vector<TypeProperties>>{std::move(input_types)}
    .map([&](std::vector<TypeProperties> inputs) {
      auto cg_terms = anyf::make_graph(inputs);

      auto cg = std::move(std::get<0>(cg_terms));
      auto terms = std::move(std::get<1>(cg_terms));

      Map<std::string, std::vector<Oterm>> bindings;

      int i = 0;
      co_visit(f.header.pattern,
               *f.header.type.input,
               Overloaded{[&](const ast::Ident& ident, const auto& type) {
                            bindings[ident.name] =
                              knot::preorder_accumulate(type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                                v.push_back(terms[i++]);
                                return v;
                              });
                          },
                          [&](const ast::WildCard&, const auto& type) { knot::preorder(type, [&](TypeID) { i++; }); }});

      return GraphContext{std::move(cg), std::move(bindings)};
    })
    .and_then([&](GraphContext ctx) {
      return knot::accumulate(f.scope.assignments,
                              ContextualResult<GraphContext>{std::move(ctx)},
                              [&](auto acc, const CheckedAssignment& assignment) {
                                return std::move(acc).and_then([&](GraphContext ctx) {
                                  return add_expr(ctx, assignment.expr, e).map([&](std::vector<Oterm> terms) {
                                    int i = 0;
                                    co_visit(assignment.pattern,
                                             assignment.type,
                                             Overloaded{[&](const ast::Ident& ident, const auto& type) {
                                                          ctx.bindings[ident.name] = knot::preorder_accumulate(
                                                            type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                                                              v.push_back(terms[i++]);
                                                              return v;
                                                            });
                                                        },
                                                        [&](const ast::WildCard&, const auto& type) {
                                                          knot::preorder(type, [&](TypeID) { i++; });
                                                        }});

                                    return std::move(ctx);
                                  });
                                });
                              });
    })
    .and_then([&](GraphContext ctx) {
      return add_expr(ctx, f.scope.result, e).and_then([&](std::vector<Oterm> terms) {
        return std::move(ctx.cg).finalize(terms).map_error(to_graph_error(f.scope.result.ref));
      });
    });
}

} // namespace ooze
