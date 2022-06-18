#include "pch.h"

#include "graph_construction.h"

namespace ooze {

using namespace ast;

namespace {

using anyf::Term;

std::string msg(const Env& e, const std::string& function, const anyf::BadArity& err) {
  return fmt::format("{} expects {} arg(s), given {}", function, err.expected, err.given);
}

std::string msg(const Env& e, const std::string& function, const anyf::BadType& err) {
  return fmt::format(
    "{} expects {} for arg {}, given {}", function, e.type_name(err.expected), err.index, e.type_name(err.given));
}

std::string msg(const Env& e, const std::string& function, const anyf::AlreadyMoved& err) {
  return fmt::format("{} value for arg {} already moved", function, err.index);
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

Result<std::vector<Term>> add_expr(GraphContext&, const Expr&, const Env&, const Map<std::string, FunctionGraph>&);

Result<std::vector<Term>> accumulate_exprs(GraphContext& ctx,
                                           const std::vector<Expr>& exprs,
                                           const Env& e,
                                           const Map<std::string, FunctionGraph>& graphs) {
  auto [terms, errors] =
    accumulate<std::pair<std::vector<Term>, std::vector<std::string>>>(exprs, [&](auto acc, const Expr& expr) {
      if(auto terms = add_expr(ctx, expr, e, graphs); terms) {
        acc.first.insert(acc.first.end(), terms->begin(), terms->end());
      } else {
        acc.second.insert(acc.second.end(), terms.error().begin(), terms.error().end());
      }
      return acc;
    });

  return errors.empty() ? Result<std::vector<Term>>{std::move(terms)} : tl::unexpected{std::move(errors)};
}

Result<std::vector<Term>>
add_expr(GraphContext& ctx, const Expr& expr, const Env& e, const Map<std::string, FunctionGraph>& graphs) {
  return std::visit(
    Overloaded{
      [&](const Indirect<Call>& call) -> Result<std::vector<Term>> {
        auto parameter_result = accumulate_exprs(ctx, call->parameters, e, graphs);

        if(const auto it = graphs.find(call->name); it != graphs.end()) {
          return parameter_result ? ctx.cg.add(it->second, *parameter_result).map_error(to_graph_error(e, call->name))
                                  : tl::unexpected{std::move(parameter_result.error())};
        } else if(e.contains_function(call->name)) {
          return parameter_result
                   ? ctx.cg.add(e.function(call->name), *parameter_result).map_error(to_graph_error(e, call->name))
                   : tl::unexpected{std::move(parameter_result.error())};
        } else {
          std::vector<std::string> errors =
            parameter_result ? std::vector<std::string>{} : std::move(parameter_result.error());
          errors.insert(errors.begin(), fmt::format("Cannot find function {}()", call->name));
          return tl::unexpected{std::move(errors)};
        }
      },
      [&](const std::string& identifier) {
        const auto it = ctx.bindings.find(identifier);
        return it != ctx.bindings.end() ? Result<std::vector<Term>>{std::vector<Term>{it->second}}
                                        : err(fmt::format("{} not found", identifier));
      },
      [&](const Literal& l) {
        error("TODO support function binding");
        return Result<std::vector<Term>>{};
      }},
    expr.v);
}

Result<FunctionGraph> create_graph(const Function& f, const Env& e, const Map<std::string, FunctionGraph>& graphs) {
  std::vector<TypeProperties> input_types;
  std::vector<std::string> errors;

  for(const Parameter& p : f.parameters) {
    if(!e.contains_type(p.type)) {
      errors.push_back(fmt::format("type {} not found", p.type));
    } else {
      input_types.push_back(p.borrow ? e.type(p.type).borrowed_type : e.type(p.type).type);
    }
  }

  return (errors.empty() ? Result<std::vector<TypeProperties>>{std::move(input_types)}
                         : tl::unexpected{std::move(errors)})
    .map([&](std::vector<TypeProperties> inputs) {
      auto [cg, iterms] = anyf::make_graph(inputs);

      Map<std::string, Term> bindings;

      for(int i = 0; i < iterms.size(); i++) {
        bindings.emplace(f.parameters[i].name, iterms[i]);
      }

      return GraphContext{std::move(cg), std::move(bindings)};
    })
    .and_then([&](GraphContext ctx) {
      return accumulate<Result<GraphContext>>(
        f.assignments,
        [&](auto acc, const Assignment& assignment) {
          return std::move(acc).and_then([&](GraphContext ctx) {
            return add_expr(ctx, assignment.expr, e, graphs)
              .and_then([&](std::vector<Term> terms) -> Result<GraphContext> {
                if(terms.size() != assignment.variables.size()) {
                  return err(
                    fmt::format("Assignment expects {} value(s), given {}", assignment.variables.size(), terms.size()));
                }

                std::vector<std::string> errors;
                for(int i = 0; i < terms.size(); i++) {
                  if(assignment.variables[i].type) {
                    const std::string& given_type = e.type_name(ctx.cg.type(terms[i]).type_id());

                    if(given_type != assignment.variables[i].type) {
                      errors.push_back(fmt::format("{} expects {}, given {}",
                                                   assignment.variables[i].name,
                                                   *assignment.variables[i].type,
                                                   given_type));
                    }
                  }
                  ctx.bindings.emplace(assignment.variables[i].name, terms[i]);
                }
                return errors.empty() ? Result<GraphContext>{std::move(ctx)} : tl::unexpected{std::move(errors)};
              });
          });
        },
        std::move(ctx));
    })
    .and_then([&](GraphContext ctx) {
      return accumulate_exprs(ctx, f.ret, e, graphs).and_then([&](std::vector<Term> terms) -> Result<FunctionGraph> {
        if(terms.size() != f.result.size()) {
          return err(fmt::format("{} returns {} value(s), given {}", f.name, f.result.size(), terms.size()));
        }

        std::vector<std::string> errors;
        for(int i = 0; i < terms.size(); i++) {
          const std::string& given_type = e.type_name(ctx.cg.type(terms[i]).type_id());

          if(given_type != f.result[i]) {
            errors.push_back(
              fmt::format("{} return element {} expects {}, given {}", f.name, i, f.result[i], given_type));
          }
        }
        return errors.empty()
                 ? std::move(ctx.cg).finalize(terms).map_error(to_graph_error(e, fmt::format("{} return", f.name)))
                 : tl::unexpected{std::move(errors)};
      });
    });
}

} // namespace

Result<Map<std::string, FunctionGraph>> create_graphs(const Env& e, const AST& ast) {
  Map<std::string, std::vector<std::string>> dependencies;

  for(const Function& function : ast) {
    dependencies.emplace(
      function.name,
      knot::preorder_accumulate<std::vector<std::string>>(function, [&](std::vector<std::string> v, const Call& c) {
        v.push_back(c.name);
        return v;
      }));
  }

  Map<std::string, FunctionGraph> graphs;
  std::vector<std::string> errors;
  Set<std::string> graph_errors;

  const auto satisfied = [&](const std::string& dep) {
    return e.contains_function(dep) || graphs.find(dep) != graphs.end();
  };

  auto it = ast.begin();
  while(it != ast.end()) {
    it = std::find_if(ast.begin(), ast.end(), [&](const Function& f) {
      const std::vector<std::string>& deps = dependencies.at(f.name);
      return graphs.find(f.name) == graphs.end() && std::all_of(deps.begin(), deps.end(), satisfied) &&
             graph_errors.find(f.name) == graph_errors.end();
    });

    if(it != ast.end()) {
      auto graph_result = create_graph(*it, e, graphs);

      if(graph_result) {
        graphs.emplace(it->name, std::move(*graph_result));
      } else {
        graph_errors.insert(it->name);
        errors.insert(errors.begin(), graph_result.error().begin(), graph_result.error().end());
      }
    }
  }

  for(const Function& f : ast) {
    if(graphs.find(f.name) == graphs.end() && graph_errors.find(f.name) == graph_errors.end()) {
      std::string dependency_error = fmt::format("Error generating {}()\n", f.name);
      for(const std::string& dep : dependencies.at(f.name)) {
        if(!satisfied(dep) && graph_errors.find(f.name) == graph_errors.end()) {
          dependency_error += fmt::format("  unfound dependency {}()\n", dep);
        }
      }
      errors.push_back(std::move(dependency_error));
    }
  }

  return errors.empty() ? Result<Map<std::string, FunctionGraph>>{std::move(graphs)}
                        : tl::unexpected{std::move(errors)};
}

} // namespace ooze
