#include "pch.h"

#include "graph_construction.h"
#include "queries.h"

namespace ooze {

namespace {

using anyf::FunctionGraph;
using anyf::Term;

auto to_graph_error(const std::vector<CheckedExpr>& exprs) {
  return [&](anyf::GraphError error) {
    const anyf::AlreadyMoved* err = std::get_if<anyf::AlreadyMoved>(&error);
    assert(err);

    return std::vector{ContextualError{exprs[err->index].ref, "binding already moved"}};
  };
}

struct GraphContext {
  anyf::ConstructingGraph cg;
  Map<std::string, Term> bindings;
};

ContextualResult<std::vector<Term>> add_expr(GraphContext&, const CheckedExpr&, const Env&);

ContextualResult<std::vector<Term>>
accumulate_exprs(GraphContext& ctx, const std::vector<CheckedExpr>& exprs, const Env& e) {
  auto [terms, errors] = accumulate<std::pair<std::vector<Term>, std::vector<ContextualError>>>(
    exprs, [&](auto acc, const CheckedExpr& expr) {
      if(auto terms = add_expr(ctx, expr, e); terms) {
        acc.first.insert(acc.first.end(), terms->begin(), terms->end());
      } else {
        acc.second.insert(acc.second.end(), terms.error().begin(), terms.error().end());
      }
      return acc;
    });

  return errors.empty() ? ContextualResult<std::vector<Term>>{std::move(terms)} : tl::unexpected{std::move(errors)};
}

ContextualResult<std::vector<Term>> add_expr(GraphContext& ctx, const CheckedExpr& expr, const Env& e) {
  return std::visit(
    Overloaded{[&](const ast::Call<EnvFunctionRef>& call) -> ContextualResult<std::vector<Term>> {
                 return accumulate_exprs(ctx, call.parameters, e)
                   .and_then([&](std::vector<Term> terms) -> ContextualResult<std::vector<Term>> {
                     const auto it = e.functions.find(call.function.name);
                     assert(it != e.functions.end() && call.function.overload_idx < it->second.size());

                     return std::visit(
                       [&](const auto& f) { return ctx.cg.add(f, terms).map_error(to_graph_error(call.parameters)); },
                       it->second[call.function.overload_idx]);
                   });
               },
               [&](const std::string& identifier) -> ContextualResult<std::vector<Term>> {
                 const auto it = ctx.bindings.find(identifier);
                 assert(it != ctx.bindings.end());
                 return std::vector<Term>{it->second};
               },
               [&](const Literal& literal) -> ContextualResult<std::vector<Term>> {
                 return std::visit(
                   [&](const auto& value) {
                     return ctx.cg.add(AnyFunction([=]() { return value; }), {})
                       .map_error([](const auto&) -> std::vector<ContextualError> {
                         assert(false); // shouldnt ever fail
                       });
                   },
                   literal);
               }},
    expr.v);
}

} // namespace

ContextualResult<anyf::FunctionGraph> create_graph(const Env& e, const CheckedFunction& f) {
  std::vector<TypeProperties> input_types;

  for(const auto& p : f.header.parameters) {
    input_types.push_back(TypeProperties{p.type, !p.borrow});
  }

  return ContextualResult<std::vector<TypeProperties>>{std::move(input_types)}
    .map([&](std::vector<TypeProperties> inputs) {
      auto [cg, iterms] = anyf::make_graph(inputs);

      Map<std::string, Term> bindings;

      for(int i = 0; i < iterms.size(); i++) {
        bindings.emplace(f.header.parameters[i].name, iterms[i]);
      }

      return GraphContext{std::move(cg), std::move(bindings)};
    })
    .and_then([&](GraphContext ctx) {
      return accumulate<ContextualResult<GraphContext>>(
        f.body.assignments,
        [&](auto acc, const CheckedAssignment& assignment) {
          return std::move(acc).and_then([&](GraphContext ctx) {
            return add_expr(ctx, assignment.expr, e).map([&](std::vector<Term> terms) {
              std::vector<TypeID> inputs(terms.size());
              std::transform(
                terms.begin(), terms.end(), inputs.begin(), [&](const auto& t) { return ctx.cg.type(t).id; });

              for(int i = 0; i < terms.size(); i++) {
                ctx.bindings.insert_or_assign(assignment.bindings[i].name, terms[i]);
              }
              return ctx;
            });
          });
        },
        std::move(ctx));
    })
    .and_then([&](GraphContext ctx) {
      return accumulate_exprs(ctx, f.body.result, e).and_then([&](std::vector<Term> terms) {
        return std::move(ctx.cg).finalize(terms).map_error(to_graph_error(f.body.result));
      });
    });
}

} // namespace ooze
