#include "pch.h"

#include "graph_construction.h"
#include "queries.h"

namespace ooze {

namespace {

using anyf::FunctionGraph;
using anyf::Term;

std::string msg(const Env& e, const std::string& function, const anyf::BadArity& err) {
  return fmt::format("{} expects {} arg(s), given {}", function, err.expected, err.given);
}

std::string msg(const Env& e, const std::string& function, const anyf::BadType& err) {
  return fmt::format("{} expects {} for arg {}, given {}",
                     function,
                     e.type_names.at(err.expected),
                     err.index,
                     type_name_or_id(e, err.given));
}

std::string msg(const Env& e, const std::string& function, const anyf::AlreadyMoved& err) {
  return fmt::format("{} value for arg {} already moved", function, err.index);
}

std::string msg(const Env& e, const std::string& function, const anyf::CannotCopy& err) {
  return fmt::format("Function input {} of type {} cannot be copied", err.index, e.type_names.at(err.type));
}

auto to_graph_error(const Env& e, std::string function) {
  return [&, f = std::move(function)](anyf::GraphError error) {
    return std::visit([&](auto err) { return std::vector{msg(e, f, err)}; }, error);
  };
}

struct GraphContext {
  anyf::ConstructingGraph cg;
  Map<std::string, Term> bindings;
};

Result<std::vector<Term>> add_expr(GraphContext&, const CheckedExpr&, const Env&);

Result<std::vector<Term>> accumulate_exprs(GraphContext& ctx, const std::vector<CheckedExpr>& exprs, const Env& e) {
  auto [terms, errors] =
    accumulate<std::pair<std::vector<Term>, std::vector<std::string>>>(exprs, [&](auto acc, const CheckedExpr& expr) {
      if(auto terms = add_expr(ctx, expr, e); terms) {
        acc.first.insert(acc.first.end(), terms->begin(), terms->end());
      } else {
        acc.second.insert(acc.second.end(), terms.error().begin(), terms.error().end());
      }
      return acc;
    });

  return errors.empty() ? Result<std::vector<Term>>{std::move(terms)} : tl::unexpected{std::move(errors)};
}

Result<std::vector<Term>> add_expr(GraphContext& ctx, const CheckedExpr& expr, const Env& e) {
  return std::visit(
    Overloaded{
      [&](const Indirect<ast::Call<EnvFunctionRef>>& call) -> Result<std::vector<Term>> {
        return accumulate_exprs(ctx, call->parameters, e)
          .and_then([&](std::vector<Term> terms) -> Result<std::vector<Term>> {
            const auto it = e.functions.find(call->function.name);
            assert(it != e.functions.end() && call->function.overload_idx < it->second.size());

            return std::visit(
              [&](const auto& f) { return ctx.cg.add(f, terms).map_error(to_graph_error(e, call->function.name)); },
              it->second[call->function.overload_idx]);
          });
      },
      [&](const std::string& identifier) -> Result<std::vector<Term>> {
        const auto it = ctx.bindings.find(identifier);
        assert(it != ctx.bindings.end());
        return std::vector<Term>{it->second};
      },
      [&](const Literal& literal) -> Result<std::vector<Term>> {
        return std::visit(
          Overloaded{[&](const std::string& value) {
                       return ctx.cg.add(AnyFunction([=]() { return value; }), {}).map_error(to_graph_error(e, value));
                     },
                     [&](const auto& value) {
                       return ctx.cg.add(AnyFunction([=]() { return value; }), {})
                         .map_error(to_graph_error(e, std::to_string(value)));
                     }},
          literal);
      }},
    expr.v);
}

} // namespace

Result<anyf::FunctionGraph> create_graph(const Env& e, const CheckedFunction& f) {
  std::vector<TypeProperties> input_types;
  std::vector<std::string> errors;

  for(const auto& p : f.header.parameters) {
    input_types.push_back(TypeProperties{p.type, !p.borrow});
  }

  return (errors.empty() ? Result<std::vector<TypeProperties>>{std::move(input_types)}
                         : tl::unexpected{std::move(errors)})
    .map([&](std::vector<TypeProperties> inputs) {
      auto [cg, iterms] = anyf::make_graph(inputs);

      Map<std::string, Term> bindings;

      for(int i = 0; i < iterms.size(); i++) {
        bindings.emplace(f.header.parameters[i].name, iterms[i]);
      }

      return GraphContext{std::move(cg), std::move(bindings)};
    })
    .and_then([&](GraphContext ctx) {
      return accumulate<Result<GraphContext>>(
        f.body.assignments,
        [&](auto acc, const CheckedAssignment& assignment) {
          return std::move(acc).and_then([&](GraphContext ctx) {
            return add_expr(ctx, assignment.expr, e).and_then([&](std::vector<Term> terms) -> Result<GraphContext> {
              std::vector<TypeID> inputs(terms.size());
              std::transform(
                terms.begin(), terms.end(), inputs.begin(), [&](const auto& t) { return ctx.cg.type(t).id; });

              for(int i = 0; i < terms.size(); i++) {
                ctx.bindings.insert_or_assign(assignment.bindings[i].name, terms[i]);
              }
              return std::move(ctx);
            });
          });
        },
        std::move(ctx));
    })
    .and_then([&](GraphContext ctx) {
      return accumulate_exprs(ctx, f.body.result, e).and_then([&](std::vector<Term> terms) -> Result<FunctionGraph> {
        return std::move(ctx.cg).finalize(terms).map_error(to_graph_error(e, fmt::format("return")));
      });
    });
}

} // namespace ooze
