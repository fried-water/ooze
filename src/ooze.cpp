#include "pch.h"

#include "bindings.h"
#include "graph_construction.h"
#include "graph_execution.h"
#include "io.h"
#include "ooze/core.h"
#include "ooze/executor/task_executor.h"
#include "parser.h"
#include "parser_combinators.h"
#include "repl.h"
#include "type_check.h"
#include "user_msg.h"

namespace ooze {

namespace {
struct Command {
  bool run_main = false;
  std::vector<std::string> filenames;
};

std::optional<Command> parse_cmd_line(int argc, const char** argv) {
  if(argc <= 1) {
    return Command{};
  } else {
    const std::string_view cmd = argv[1];

    std::vector<std::string> filenames;
    for(int i = 2; i < argc; i++) {
      filenames.push_back(argv[i]);
    }

    if(cmd == "run") {
      return Command{true, std::move(filenames)};
    } else if(cmd == "repl") {
      return Command{false, std::move(filenames)};
    } else {
      return std::nullopt;
    }
  }
}

StringResult<void, Env> parse_scripts(Env e, const std::vector<std::string>& filenames) {
  return knot::accumulate(
    filenames, StringResult<void, Env>{std::move(e)}, [&](auto result, const std::string& filename) {
      return std::move(result)
        .and_then([&](Env e) { return read_text_file(filename).append_state(std::move(e)); })
        .and_then([&](const std::string& script, Env e) { return parse_script(std::move(e), script); });
    });
}

std::vector<std::string> gather_binding_strings(std::vector<Binding> bindings) {
  return transform_to_vec(std::move(bindings), [](Binding b) {
    return any_cast<std::string>(take(std::move(b)).wait());
  });
}

TypedFunction lift_only_borrowed_idents(TypedFunction f) {
  const auto count_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(f.expr, 0, [&](int acc, const TypedExpr& e) {
      return knot::accumulate(e.v, acc, [&](int acc, const ast::Ident& i) { return i.name == name ? acc + 1 : acc; });
    });
  };

  const auto get_borrowed_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(f.expr, std::vector<TypedExpr*>{}, [&](auto acc, TypedExpr& e) {
      knot::visit(e.v, [&](const TypedBorrowExpr& b) {
        knot::visit(b.expr->v, [&](const ast::Ident& i) {
          if(i.name == name) {
            acc.push_back(&e);
          }
        });
      });
      return acc;
    });
  };

  co_visit(f.header.pattern,
           *f.header.type.input,
           [&](const ast::Pattern&, CompoundType<TypeID>& t, const ast::Ident& i, const auto&) {
             const int uses = count_usages(i.name);
             auto borrows = get_borrowed_usages(i.name);
             if(uses == borrows.size()) {
               t = borrow_type(std::move(t));
               for(TypedExpr* e : borrows) {
                 *e = TypedExpr{ast::Ident{i.name}};
               }
             }
           });

  return f;
}

ContextualResult<TypedFunction>
infer_header_from_env(const Env& env, const Bindings& bindings, TypedExpr expr, CompoundType<TypeID> result_type) {
  Set<std::string> active;
  for(const auto& [name, fn] : env.functions) {
    if(bindings.find(name) == bindings.end()) {
      active.insert(name);
    }
  }

  TypedHeader h = inferred_header(expr, std::move(active));

  *h.type.output = std::move(result_type);

  std::vector<ContextualError> errors;

  co_visit(h.pattern,
           *h.type.input,
           Overloaded{[](ast::Pattern&, CompoundType<TypeID>&, const ast::WildCard& pattern, const auto& type) {
                        assert(false);
                      },
                      [&](ast::Pattern& p, CompoundType<TypeID>& t, const ast::Ident& ident, const auto&) {
                        if(const auto it = bindings.find(ident.name); it != bindings.end()) {
                          t = type(it->second);
                        } else {
                          errors.push_back({p.ref, fmt::format("use of undeclared binding '{}'", ident.name)});
                        }
                      }});

  return value_or_errors(TypedFunction{std::move(h), std::move(expr)}, std::move(errors));
}

bool is_instantiated(const Env& e, const EnvFunctionRef& call) {
  const EnvFunction& ef = e.functions.at(call.name)[call.overload_idx];
  return call.type == ef.type || any_of(ef.instatiations, [&](const auto& p) { return call.type == p.first; });
}

std::vector<EnvFunctionRef> uninstantiated_functions(const Env& e, const CheckedFunction& f) {
  return knot::preorder_accumulate(f, std::vector<EnvFunctionRef>{}, [&](auto acc, const EnvFunctionRef& call) {
    if(!is_instantiated(e, call)) {
      acc.push_back(call);
    }
    return acc;
  });
}

ContextualResult<FunctionGraph, Env> create_graph_and_instantiations(Env e, const CheckedFunction& f) {
  std::vector<ContextualError> errors;

  std::vector<EnvFunctionRef> typed_functions = uninstantiated_functions(e, f);
  std::vector<std::pair<EnvFunctionRef, CheckedFunction>> checked_functions;

  Set<EnvFunctionRef> visited;

  for(size_t i = 0; i < typed_functions.size(); i++) {
    const auto& call = typed_functions[i];
    if(visited.insert(call).second) {
      const EnvFunction& ef = e.functions.at(call.name)[call.overload_idx];
      auto result = overload_resolution_concrete(e, std::get<TypedFunction>(ef.f), call.type);

      if(result) {
        typed_functions = to_vec(uninstantiated_functions(e, *result), std::move(typed_functions));
        checked_functions.emplace_back(std::move(typed_functions[i]), std::move(*result));
      } else {
        errors = to_vec(std::move(result.error()), std::move(errors));
      }
    }
  }

  if(errors.empty()) {
    for(int i = checked_functions.size() - 1; i >= 0; i--) {
      const auto& [call, f] = checked_functions[i];
      e.functions.at(call.name)[call.overload_idx].instatiations.emplace_back(call.type, create_graph(e, f));
    }

    return ContextualResult<FunctionGraph>{create_graph(e, f)}.append_state(std::move(e));
  } else {
    return {Failure{std::move(errors)}, std::move(e)};
  }
}

ContextualResult<void, Env>
add_function(Env env, const std::string& name, std::variant<CheckedFunction, TypedFunction> var) {
  return success<std::vector<ContextualError>>(std::move(var), std::move(env))
    .and_then(visited(Overloaded{
      [&](TypedFunction f, Env env) {
        env.functions[name].push_back({f.header.type, std::move(f)});
        return ContextualResult<void, Env>{std::move(env)};
      },
      [&](CheckedFunction f, Env env) {
        return create_graph_and_instantiations(std::move(env), f).map([&](FunctionGraph g, Env env) {
          env.functions[name].push_back({std::move(f.header.type), std::move(g)});
          return env;
        });
      }}));
}

ContextualResult<Tree<Binding>, Env, Bindings>
run_function(ExecutorRef executor, Env e, Bindings bindings, const CheckedFunction& f) {
  return create_graph_and_instantiations(std::move(e), f)
    .append_state(std::move(bindings))
    .map([&](FunctionGraph g, Env e, Bindings bindings) {
      std::vector<Future> value_inputs;
      std::vector<BorrowedFuture> borrowed_inputs;

      co_visit(f.header.pattern,
               *f.header.type.input,
               Overloaded{[&](auto&, auto&, const ast::Ident& i, const auto&) {
                            value_inputs = to_vec(*take(bindings, i.name), std::move(value_inputs));
                          },
                          [&](auto&, auto&, const ast::Ident& i, const Borrow<TypeID>&) {
                            borrowed_inputs = to_vec(*borrow(bindings, i.name), std::move(borrowed_inputs));
                          }});

      std::vector<Future> futures = execute_graph(g, executor, std::move(value_inputs), std::move(borrowed_inputs));

      int idx = 0;
      const auto converter = Overloaded{
        // result vector should be in order of a preorder traversal of the leaves in the output type
        [&](TypeID t) {
          return Binding{{t}, std::move(futures[idx++])};
        },
        [&](const FunctionType<TypeID>& t) {
          return Binding{{t}, std::move(futures[idx++])};
        },

        // These can't be part of the output type of executed functions
        [](const Floating&) -> Binding {
          assert(false);
          exit(1);
        },
        [](const Borrow<TypeID>&) -> Binding {
          assert(false);
          exit(1);
        }};

      return std::tuple(
        knot::map<Tree<Binding>>(*f.header.type.output, std::cref(converter)), std::move(e), std::move(bindings));
    });
}

ContextualResult<Tree<Binding>, Env, Bindings>
run_expr(ExecutorRef executor, Env env, Bindings bindings, TypedExpr expr, CompoundType<TypeID> type) {
  return infer_header_from_env(env, bindings, std::move(expr), std::move(type))
    .map(lift_only_borrowed_idents)
    .append_state(std::move(env))
    .and_then([](TypedFunction f, Env env) {
      return overload_resolution_concrete(env, std::move(f)).append_state(std::move(env));
    })
    .append_state(std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    });
}

ContextualResult<std::string, Env, Bindings>
run_expr_to_string(ExecutorRef executor, Env env, Bindings bindings, TypedExpr expr) {
  return infer_header_from_env(env, bindings, std::move(expr), floating_type<TypeID>())
    .map(lift_only_borrowed_idents)
    .append_state(std::move(env))
    .and_then([](TypedFunction f, Env env) {
      auto result = overload_resolution_concrete(env, f);
      return result ? ContextualResult<TypedFunction, Env>{std::move(f), std::move(env)}
                    : ContextualResult<TypedFunction, Env>{Failure{std::move(result.error())}, std::move(env)};
    })
    .map([](TypedFunction f, Env env) {
      TypedScopeExpr scope;

      const int args = std::visit(Overloaded{[](const std::vector<CompoundType<TypeID>>& v) { return int(v.size()); },
                                             [](const auto&) { return 0; }},
                                  f.header.type.input->v);

      if(std::holds_alternative<ast::Ident>(f.expr.v) && args == 1) {
        knot::visit(f.header.type.input->v,
                    [](std::vector<CompoundType<TypeID>>& v) { v[0] = borrow_type(std::move(v[0])); });

        scope.assignments.push_back(
          {{ast::Ident{"x"}}, borrow_type(std::move(*f.header.type.output)), std::move(f.expr)});
        scope.result =
          TypedExpr{TypedCallExpr{{{ast::Ident{"to_string"}}}, {{std::vector{TypedExpr{ast::Ident{"x"}}}}}}};
      } else {
        scope.assignments.push_back({{ast::Ident{"x"}}, std::move(*f.header.type.output), std::move(f.expr)});
        scope.result = TypedExpr{TypedCallExpr{
          {{ast::Ident{"to_string"}}}, {{std::vector{TypedExpr{TypedBorrowExpr{TypedExpr{ast::Ident{"x"}}}}}}}}};
      }

      f.header.type.output = leaf_type(type_id<std::string>());
      f.expr.v = std::move(scope);

      return std::tuple(std::move(f), std::move(env));
    })
    .and_then([](TypedFunction f, Env env) {
      return overload_resolution_concrete(env, std::move(f)).append_state(std::move(env));
    })
    .append_state(std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    })
    .map([](Tree<Binding> t, Env env, Bindings b) {
      std::vector<Future> futures = take(std::move(t));
      return std::tuple(any_cast<std::string>(std::move(futures[0]).wait()), std::move(env), std::move(b));
    });
}

ContextualResult<void, Env, Bindings> run_assign(ExecutorRef executor, Env env, Bindings bindings, TypedAssignment a) {
  return infer_header_from_env(env, bindings, std::move(*a.expr), std::move(a.type))
    .map(lift_only_borrowed_idents)
    .append_state(std::move(env))
    .and_then([](TypedFunction f, Env env) {
      return overload_resolution_concrete(env, std::move(f)).append_state(std::move(env));
    })
    .and_then([&](CheckedFunction f, Env env) {
      auto result = type_check(env, a.pattern, *f.header.type.output);
      return result ? ContextualResult<CheckedFunction, Env>{std::move(f), std::move(env)}
                    : ContextualResult<CheckedFunction, Env>{Failure{std::move(result.error())}, std::move(env)};
    })
    .append_state(std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    })
    .map([&](Tree<Binding> results, Env env, Bindings bindings) {
      co_visit(a.pattern, results, [&](const ast::Pattern&, Tree<Binding>& tree, const ast::Ident& ident, const auto&) {
        bindings[ident.name] = std::move(tree);
      });
      return std::tuple(std::move(env), std::move(bindings));
    });
}

struct TypeOfBindingConverter {
  CompoundType<TypeID> operator()(const Tree<Binding>& tree) const {
    return std::visit(Overloaded{[&](const std::vector<Tree<Binding>>& v) {
                                   return CompoundType<TypeID>{knot::map<std::vector<CompoundType<TypeID>>>(v, *this)};
                                 },
                                 [&](const Binding& b) { return b.type; }},
                      tree.v);
  }
};

} // namespace

Tree<Any> await(Tree<Binding> tree) {
  return knot::map<Tree<Any>>(std::move(tree), [](Binding b) -> Any { return take(std::move(b)).wait(); });
}

CompoundType<TypeID> type(const Tree<Binding>& tree) { return TypeOfBindingConverter{}(tree); }

StringResult<void, Env> parse_script(Env env, std::string_view script) {
  return parse(script)
    .append_state(std::move(env))
    .and_then([](UnTypedAST ast, Env env) {
      return knot::accumulate(ast, ContextualResult<void, Env>{std::move(env)}, [&](auto result, const auto& tup) {
        const auto& name = std::get<0>(tup);
        return std::move(result)
          .and_then([&](Env env) { return type_name_resolution(env, std::get<1>(tup)).append_state(std::move(env)); })
          .and_then([](TypedFunction f, Env env) {
            return overload_resolution(env, std::move(f)).append_state(std::move(env));
          })
          .and_then([&](auto var, Env env) { return add_function(std::move(env), name, std::move(var)); });
      });
    })
    .map_error([&](auto errors) { return contextualize(script, std::move(errors)); });
}

StringResult<Tree<Binding>, Env, Bindings>
run(ExecutorRef executor, Env env, Bindings bindings, std::string_view expr) {
  return parse_repl(expr)
    .append_state(std::move(env), std::move(bindings))
    .and_then(visited(Overloaded{
      [&](UnTypedExpr expr, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(expr))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedExpr expr, Env env, Bindings bindings) {
            return run_expr(executor, std::move(env), std::move(bindings), std::move(expr), floating_type<TypeID>());
          });
      },
      [&](UnTypedAssignment assign, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(assign))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedAssignment assign, Env env, Bindings bindings) {
            return run_assign(executor, std::move(env), std::move(bindings), std::move(assign));
          })
          .map([](Env env, Bindings b) { return std::tuple(Tree<Binding>{}, std::move(env), std::move(b)); });
      }}))
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

StringResult<std::string, Env, Bindings>
run_to_string(ExecutorRef executor, Env env, Bindings bindings, std::string_view expr) {
  return parse_repl(expr)
    .append_state(std::move(env), std::move(bindings))
    .and_then(visited(Overloaded{
      [&](UnTypedExpr expr, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(expr))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedExpr expr, Env env, Bindings bindings) {
            return run_expr_to_string(executor, std::move(env), std::move(bindings), std::move(expr));
          });
      },
      [&](UnTypedAssignment assign, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(assign))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedAssignment assign, Env env, Bindings bindings) {
            return run_assign(executor, std::move(env), std::move(bindings), std::move(assign));
          })
          .map([](Env env, Bindings bindings) {
            return std::tuple(std::string(), std::move(env), std::move(bindings));
          });
      },
    }))
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

int main(int argc, const char** argv, Env e) {
  const std::optional<Command> cmd = parse_cmd_line(argc, argv);

  if(!cmd) {
    const char* msg =
      "Usage:\n"
      "  run [scripts...]\n"
      "  repl [scripts...]\n";

    fmt::print("{}", msg);
    return 1;
  }

  Executor executor = make_task_executor();

  const auto result =
    parse_scripts(std::move(e), cmd->filenames).append_state(Bindings{}).and_then([&](Env env, Bindings bindings) {
      if(cmd->run_main) {
        return run_to_string(executor, std::move(env), std::move(bindings), "main()")
          .map([](std::string s, Env e, Bindings b) {
            return std::tuple(make_vector(std::move(s)), std::move(e), std::move(b));
          });
      } else {
        std::tie(env, bindings) = run_repl(executor, std::move(env), std::move(bindings));
        return success<std::vector<std::string>>(std::vector<std::string>{}, std::move(env), std::move(bindings));
      }
    });

  for(const std::string& line : result ? result.value() : result.error()) {
    fmt::print("{}\n", line);
  }

  return !result.has_value();
}

} // namespace ooze
