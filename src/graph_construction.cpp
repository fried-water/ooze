#include "pch.h"

#include "graph_construction.h"
#include "overload_resolution.h"
#include "queries.h"

namespace ooze {

using namespace ast;

namespace {

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

Result<std::vector<Term>>
add_expr(GraphContext&, const Expr&, const Env&, std::function<Result<Any>(const Env&, const std::string&)>);

Result<std::vector<Term>> accumulate_exprs(GraphContext& ctx,
                                           const std::vector<Expr>& exprs,
                                           const Env& e,
                                           std::function<Result<Any>(const Env&, const std::string&)> load) {
  auto [terms, errors] =
    accumulate<std::pair<std::vector<Term>, std::vector<std::string>>>(exprs, [&](auto acc, const Expr& expr) {
      if(auto terms = add_expr(ctx, expr, e, load); terms) {
        acc.first.insert(acc.first.end(), terms->begin(), terms->end());
      } else {
        acc.second.insert(acc.second.end(), terms.error().begin(), terms.error().end());
      }
      return acc;
    });

  return errors.empty() ? Result<std::vector<Term>>{std::move(terms)} : tl::unexpected{std::move(errors)};
}

Result<std::vector<Term>> add_expr(GraphContext& ctx,
                                   const Expr& expr,
                                   const Env& e,
                                   std::function<Result<Any>(const Env&, const std::string&)> load) {
  return std::visit(
    Overloaded{
      [&](const Indirect<Call>& call) -> Result<std::vector<Term>> {
        if(call->name == "load") {
          if(call->parameters.size() != 1) {
            return err(fmt::format("load expects a string literal, given {} arg(s)", call->parameters.size()));
          } else if(auto literal_ptr = std::get_if<Literal>(&call->parameters.front().v); !literal_ptr) {
            return err(fmt::format("load expects a string literal"));
          } else if(auto string_ptr = std::get_if<std::string>(literal_ptr); !string_ptr) {
            return err(fmt::format("load expects a string literal"));
          } else {
            return load(e, *string_ptr).and_then([&](Any value) {
              return ctx.cg.add(AnyFunction(std::move(value)), {}).map_error(to_graph_error(e, *string_ptr));
            });
          }
        } else {
          return accumulate_exprs(ctx, call->parameters, e, load)
            .map_error([&](std::vector<std::string> errors) {
              if(e.functions.find(call->name) == e.functions.end()) {
                errors.push_back(fmt::format("use of undeclared function '{}'", call->name));
              }
              return errors;
            })
            .and_then([&](std::vector<Term> terms) -> Result<std::vector<Term>> {
              std::vector<TypeProperties> types;
              std::transform(
                terms.begin(), terms.end(), std::back_inserter(types), [&](Term term) { return ctx.cg.type(term); });
              return overload_resolution(e, call->name, types).and_then([&](EnvFunction f) {
                return std::visit(
                  [&](auto f) { return ctx.cg.add(std::move(f), terms).map_error(to_graph_error(e, call->name)); },
                  std::move(f));
              });
            });
        }
      },
      [&](const std::string& identifier) {
        const auto it = ctx.bindings.find(identifier);
        return it != ctx.bindings.end() ? Result<std::vector<Term>>{std::vector<Term>{it->second}}
                                        : err(fmt::format("Identifier {} not found", identifier));
      },
      [&](const Literal& literal) {
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

std::pair<Env, std::vector<std::string>>
create_graphs(Env e, const AST& ast, std::function<Result<Any>(const Env&, const std::string&)> load) {
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

  const auto satisfied = [&](const std::string& dep) { return e.functions.find(dep) != e.functions.end(); };

  auto it = ast.begin();
  while(it != ast.end()) {
    it = std::find_if(ast.begin(), ast.end(), [&](const Function& f) {
      const std::vector<std::string>& deps = dependencies.at(f.name);
      return e.functions.find(f.name) == e.functions.end() && std::all_of(deps.begin(), deps.end(), satisfied) &&
             graph_errors.find(f.name) == graph_errors.end();
    });

    if(it != ast.end()) {
      auto graph_result = create_graph(e, *it, load);

      if(graph_result) {
        e.add_graph(it->name, std::move(*graph_result));
      } else {
        graph_errors.insert(it->name);
        errors.insert(errors.begin(), graph_result.error().begin(), graph_result.error().end());
      }
    }
  }

  for(const Function& f : ast) {
    if(e.functions.find(f.name) == e.functions.end() && graph_errors.find(f.name) == graph_errors.end()) {
      std::string dependency_error = fmt::format("Error generating {}()\n", f.name);
      for(const std::string& dep : dependencies.at(f.name)) {
        if(!satisfied(dep) && graph_errors.find(f.name) == graph_errors.end()) {
          dependency_error += fmt::format("  unfound dependency {}()\n", dep);
        }
      }
      errors.push_back(std::move(dependency_error));
    }
  }

  return {std::move(e), std::move(errors)};
}

Result<FunctionGraph>
create_graph(const Env& e, const Function& f, std::function<Result<Any>(const Env&, const std::string&)> load) {
  std::vector<TypeProperties> input_types;
  std::vector<std::string> errors;

  for(const Parameter& p : f.parameters) {
    if(const auto it = e.type_ids.find(p.type); it != e.type_ids.end()) {
      input_types.push_back(TypeProperties{it->second, !p.borrow});
    } else {
      errors.push_back(fmt::format("type {} not found", p.type));
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
            return add_expr(ctx, assignment.expr, e, load)
              .and_then([&](std::vector<Term> terms) -> Result<GraphContext> {
                std::vector<TypeID> inputs(terms.size());
                std::transform(
                  terms.begin(), terms.end(), inputs.begin(), [&](const auto& t) { return ctx.cg.type(t).id; });

                return type_check(e, assignment.bindings, inputs).map([&]() {
                  for(int i = 0; i < terms.size(); i++) {
                    ctx.bindings.insert_or_assign(assignment.bindings[i].name, terms[i]);
                  }
                  return std::move(ctx);
                });
              });
          });
        },
        std::move(ctx));
    })
    .and_then([&](GraphContext ctx) {
      return accumulate_exprs(ctx, f.ret, e, load).and_then([&](std::vector<Term> terms) -> Result<FunctionGraph> {
        if(terms.size() != f.result.size()) {
          return err(fmt::format("{} returns {} value(s), given {}", f.name, f.result.size(), terms.size()));
        }

        std::vector<std::string> errors;
        for(int i = 0; i < terms.size(); i++) {
          const std::string given_type = type_name_or_id(e, ctx.cg.type(terms[i]).id);

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

Result<FunctionGraph> create_graph(const Env& e,
                                   const ast::Expr& expr,
                                   const std::vector<std::pair<std::string, TypeProperties>>& inputs,
                                   std::function<Result<Any>(const Env&, const std::string&)> load) {
  std::vector<TypeProperties> input_types;
  Map<std::string, Term> bindings;

  for(int i = 0; i < inputs.size(); i++) {
    input_types.push_back(inputs[i].second);
    bindings.emplace(inputs[i].first, Term{0, i});
  }

  GraphContext ctx{std::get<0>(anyf::make_graph(input_types)), std::move(bindings)};

  return add_expr(ctx, expr, e, load).and_then([&](std::vector<Term> terms) {
    return std::move(ctx.cg).finalize(terms).map_error(to_graph_error(e, "return"));
  });
}

std::vector<std::pair<std::string, TypeProperties>>
inputs_of(const Env& e, const ast::Expr& expr, std::function<std::optional<TypeID>(const std::string&)> type_of) {

  // Add inputs for all identifiers
  auto inputs = knot::preorder_accumulate<std::vector<std::pair<std::string, TypeProperties>>>(
    expr, [&](auto inputs, const ast::Expr& expr) {
      if(const std::string* binding = std::get_if<std::string>(&expr.v); binding != nullptr) {
        if(auto opt_type = type_of(*binding); opt_type) {
          inputs.emplace_back(*binding, TypeProperties{*opt_type});
        }
      }

      return inputs;
    });

  std::unordered_map<const ast::Expr*, anyf::TypeID> expr_types;

  // For all known function inputs, check if it takes its argument by value
  // To resolve function overloads we need to do a partial type check
  // If any fails we can abort since it's guarenteed to fail later when constructing the graph
  knot::postorder(expr, [&](const ast::Expr& expr) {
    std::optional<TypeID> type =
      std::visit(Overloaded{[](const Literal& literal) { return std::optional(ooze::type_of(literal)); },
                            [&](const std::string& binding) { return type_of(binding); },
                            [&](const Indirect<Call>& c) -> std::optional<TypeID> {
                              std::vector<TypeProperties> given_types;
                              for(const ast::Expr& e : c->parameters) {
                                const auto it = expr_types.find(&e);
                                if(it == expr_types.end()) {
                                  return std::nullopt;
                                }
                                given_types.push_back({it->second, true});
                              }

                              Result<EnvFunction> f = overload_resolution(e, c->name, given_types);

                              if(!f) {
                                return std::nullopt;
                              }

                              const auto function_inputs = input_types(f.value());

                              for(size_t i = 0; i < function_inputs.size(); i++) {
                                if(const std::string* binding = std::get_if<std::string>(&c->parameters[i].v);
                                   binding != nullptr) {
                                  const auto it = std::find_if(
                                    inputs.begin(), inputs.end(), [&](const auto& p) { return p.first == *binding; });
                                  if(it != inputs.end() && it->second.id == function_inputs[i].id) {
                                    it->second.value = function_inputs[i].value;
                                  }
                                }
                              }

                              assert(output_types(f.value()).size() == 1); // TODO decide how to handle this
                              return output_types(f.value()).front();
                            }},
                 expr.v);

    if(type) {
      expr_types.emplace(&expr, *type);
    }
  });

  return inputs;
}

} // namespace ooze
