#include "pch.h"

#include "graph_construction.h"
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

auto to_graph_error(const Env& e, std::string function) {
  return [&, f = std::move(function)](anyf::GraphError error) {
    return std::visit([&](auto err) { return std::vector{msg(e, f, err)}; }, error);
  };
}

struct GraphContext {
  anyf::ConstructingGraph cg;
  Map<std::string, Term> bindings;
};

Result<std::vector<Term>> add_expr(GraphContext&,
                                   const Expr&,
                                   const Env&,
                                   const Map<std::string, FunctionGraph>&,
                                   std::function<Result<Any>(const std::string&)>);

Result<std::vector<Term>> accumulate_exprs(GraphContext& ctx,
                                           const std::vector<Expr>& exprs,
                                           const Env& e,
                                           const Map<std::string, FunctionGraph>& graphs,
                                           std::function<Result<Any>(const std::string&)> load) {
  auto [terms, errors] =
    accumulate<std::pair<std::vector<Term>, std::vector<std::string>>>(exprs, [&](auto acc, const Expr& expr) {
      if(auto terms = add_expr(ctx, expr, e, graphs, load); terms) {
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
                                   const Map<std::string, FunctionGraph>& graphs,
                                   std::function<Result<Any>(const std::string&)> load) {
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
            return load(*string_ptr).and_then([&](Any value) {
              return ctx.cg.add(AnyFunction(std::move(value)), {}).map_error(to_graph_error(e, *string_ptr));
            });
          }
        } else {
          return accumulate_exprs(ctx, call->parameters, e, graphs, load)
            .map_error([&](std::vector<std::string> errors) {
              if(call->name != "clone" && graphs.find(call->name) == graphs.end() &&
                 e.functions.find(call->name) == e.functions.end()) {
                errors.push_back(fmt::format("Function {} not found", call->name));
              }
              return errors;
            })
            .and_then([&](std::vector<Term> terms) -> Result<std::vector<Term>> {
              if(call->name == "clone") {
                if(call->parameters.size() != 1) {
                  return err(fmt::format("clone expects 1 arg, given {} arg(s)", call->parameters.size()));
                } else if(const TypeID input_type = ctx.cg.type(terms.front()).id; !anyf::is_copyable(input_type)) {
                  return err(fmt::format("clone requires a copyable type, given {}", type_name_or_id(e, input_type)));
                } else {
                  AnyFunction clone(
                    {{input_type}}, {input_type}, [](Span<Any*> s) { return std::vector<Any>{*s.front()}; });
                  return ctx.cg.add(std::move(clone), terms).map_error(to_graph_error(e, call->name));
                }
              } else if(const auto it = graphs.find(call->name); it != graphs.end()) {
                return ctx.cg.add(it->second, terms).map_error(to_graph_error(e, call->name));
              } else if(const auto it = e.functions.find(call->name); it != e.functions.end()) {
                return ctx.cg.add(it->second, terms).map_error(to_graph_error(e, call->name));
              } else {
                return err(fmt::format("Function {} not found", call->name));
              }
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

Result<Map<std::string, FunctionGraph>>
create_graphs(const Env& e, const AST& ast, std::function<Result<Any>(const std::string&)> load) {
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
    return e.functions.find(dep) != e.functions.end() || graphs.find(dep) != graphs.end();
  };

  auto it = ast.begin();
  while(it != ast.end()) {
    it = std::find_if(ast.begin(), ast.end(), [&](const Function& f) {
      const std::vector<std::string>& deps = dependencies.at(f.name);
      return graphs.find(f.name) == graphs.end() && std::all_of(deps.begin(), deps.end(), satisfied) &&
             graph_errors.find(f.name) == graph_errors.end();
    });

    if(it != ast.end()) {
      auto graph_result = create_graph(e, *it, graphs, load);

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

Result<FunctionGraph> create_graph(const Env& e,
                                   const Function& f,
                                   const Map<std::string, FunctionGraph>& graphs,
                                   std::function<Result<Any>(const std::string&)> load) {
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
            return add_expr(ctx, assignment.expr, e, graphs, load)
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
      return accumulate_exprs(ctx, f.ret, e, graphs, load)
        .and_then([&](std::vector<Term> terms) -> Result<FunctionGraph> {
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
                                   const Map<std::string, FunctionGraph>& graphs,
                                   const std::vector<std::pair<std::string, TypeProperties>>& inputs,
                                   std::function<Result<Any>(const std::string&)> load) {
  std::vector<TypeProperties> input_types;
  Map<std::string, Term> bindings;

  for(int i = 0; i < inputs.size(); i++) {
    input_types.push_back(inputs[i].second);
    bindings.emplace(inputs[i].first, Term{0, i});
  }

  GraphContext ctx{std::get<0>(anyf::make_graph(input_types)), std::move(bindings)};

  return add_expr(ctx, expr, e, graphs, load).and_then([&](std::vector<Term> terms) {
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

  // For all known function inputs, check if it takes its argument by value
  return knot::preorder_accumulate<std::vector<std::pair<std::string, TypeProperties>>>(
    expr,
    [&](auto inputs, const ast::Call& c) {
      if(const auto it = e.functions.find(c.name); it != e.functions.end()) {
        const auto& input_types = it->second.input_types();
        const size_t s = std::min(input_types.size(), c.parameters.size());

        for(size_t i = 0; i < s; i++) {
          if(const std::string* binding = std::get_if<std::string>(&c.parameters[i].v); binding != nullptr) {
            const auto it =
              std::find_if(inputs.begin(), inputs.end(), [&](const auto& p) { return p.first == *binding; });
            if(it != inputs.end() && it->second.id == input_types[i].id) {
              it->second.value = input_types[i].value;
            }
          }
        }
      }

      return inputs;
    },
    std::move(inputs));
}

} // namespace ooze
