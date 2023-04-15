#include "pch.h"

#include "bindings.h"
#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "parser.h"
#include "parser_combinators.h"
#include "queries.h"
#include "repl.h"
#include "type_check.h"
#include "user_msg.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

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

Result<void> parse_scripts(Env& e, const std::vector<std::string>& filenames) {
  return knot::accumulate(
    filenames, Result<void>{}, [&](Result<void> acc, const std::string& filename) -> Result<void> {
      auto result =
        read_text_file(filename).and_then([&](const std::string& script) { return parse_script(e, script); });

      if(acc) {
        return result;
      } else if(result) {
        return acc;
      } else {
        acc.error().insert(acc.error().end(),
                           std::make_move_iterator(result.error().begin()),
                           std::make_move_iterator(result.error().end()));
        return acc;
      }
    });
}

std::vector<std::string> gather_binding_strings(std::vector<Binding> bindings) {
  return transform_to_vec(std::move(bindings),
                          [](Binding b) { return anyf::any_cast<std::string>(take(std::move(b)).wait()); });
}

TypedFunction lift_only_borrowed_idents(TypedFunction f) {
  assert(f.scope.assignments.empty());

  const auto count_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(
      f.scope.result, 0, [&](int acc, const ast::IdentExpr& i) { return i.name == name ? acc + 1 : acc; });
  };

  const auto get_borrowed_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(f.scope.result, std::vector<TypedExpr*>{}, [&](auto acc, TypedExpr& e) {
      knot::visit(e.v, [&](const ast::BorrowExpr<NamedFunction>& b) {
        knot::visit(b.e->v, [&](const ast::IdentExpr& i) {
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
                 *e = TypedExpr{ast::IdentExpr{i.name}};
               }
             }
           });

  return f;
}

ContextualResult<TypedFunction>
infer_header_from_env(const RuntimeEnv& r, TypedScope s, CompoundType<TypeID> result_type) {
  TypedHeader h = inferred_header(s);

  *h.type.output = std::move(result_type);

  std::vector<ContextualError> errors;

  co_visit(h.pattern,
           *h.type.input,
           Overloaded{[](ast::Pattern&, CompoundType<TypeID>&, const ast::WildCard& pattern, const auto& type) {
                        assert(false);
                      },
                      [&](ast::Pattern& p, CompoundType<TypeID>& t, const ast::Ident& ident, const auto&) {
                        if(const auto it = r.bindings.find(ident.name); it != r.bindings.end()) {
                          t = type(it->second);
                        } else {
                          errors.push_back({p.ref, fmt::format("use of undeclared binding '{}'", ident.name)});
                        }
                      }});

  return value_or_errors(TypedFunction{std::move(h), std::move(s)}, std::move(errors));
}

ContextualResult<Tree<Binding>> run_function(RuntimeEnv& r, const CheckedFunction& f) {
  return create_graph(r.env, f).map([&](anyf::FunctionGraph g) {
    std::vector<anyf::Future> value_inputs;
    std::vector<anyf::BorrowedFuture> borrowed_inputs;

    co_visit(f.header.pattern,
             *f.header.type.input,
             Overloaded{[&](auto&, auto&, const ast::Ident& i, const auto&) {
                          auto values = take(r.bindings, i.name);
                          assert(values);
                          value_inputs.insert(value_inputs.end(),
                                              std::make_move_iterator(values->begin()),
                                              std::make_move_iterator(values->end()));
                        },
                        [&](auto&, auto&, const ast::Ident& i, const Borrow<TypeID>&) {
                          auto borrows = borrow(r.bindings, i.name);
                          assert(borrows);
                          borrowed_inputs.insert(borrowed_inputs.end(),
                                                 std::make_move_iterator(borrows->begin()),
                                                 std::make_move_iterator(borrows->end()));
                        }});

    std::vector<anyf::Future> results =
      anyf::execute_graph(g, r.executor, std::move(value_inputs), std::move(borrowed_inputs));

    int idx = 0;
    const auto converter =
      Overloaded{// result vector should be in order of a preorder traversal of the leaves in the output type
                 [&](TypeID t) -> Binding {
                   return {t, std::move(results[idx++])};
                 },

                 // These can't be part of the output type of executed functions
                 [](const FunctionType<TypeID>&) -> Binding { assert(false); },
                 [](const Floating&) -> Binding { assert(false); },
                 [](const Borrow<TypeID>&) -> Binding { assert(false); }};

    return knot::map<Tree<Binding>>(*f.header.type.output, std::cref(converter));
  });
}

ContextualResult<Tree<Binding>> run_expr(RuntimeEnv& r, TypedExpr expr) {
  return infer_header_from_env(r, TypedScope{{}, {std::move(expr)}}, floating_type<TypeID>())
    .map(lift_only_borrowed_idents)
    .and_then([&](TypedFunction f) { return overload_resolution(r.env, std::move(f)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); });
}

ContextualResult<std::string> run_expr_to_string(RuntimeEnv& r, TypedExpr expr) {
  return infer_header_from_env(r, TypedScope{{}, {std::move(expr)}}, floating_type<TypeID>())
    .map(lift_only_borrowed_idents)
    .and_then(
      [&](TypedFunction f) { return overload_resolution(r.env, f).map([&](CheckedFunction) { return std::move(f); }); })
    .map([&](TypedFunction f) {
      if(std::holds_alternative<ast::IdentExpr>(f.scope.result.v)) {
        knot::visit(f.header.type.input->v, [](std::vector<CompoundType<TypeID>>& v) {
          assert(v.size() == 1);
          v[0] = borrow_type(std::move(v[0]));
        });

        f.scope.assignments.push_back(
          {{ast::Ident{"x"}}, borrow_type(std::move(*f.header.type.output)), std::move(f.scope.result)});
        f.scope.result = TypedExpr{ast::Call<NamedFunction>{"to_string", {TypedExpr{ast::IdentExpr{"x"}}}}};
      } else {
        f.scope.assignments.push_back({{ast::Ident{"x"}}, std::move(*f.header.type.output), std::move(f.scope.result)});
        f.scope.result = TypedExpr{ast::Call<NamedFunction>{
          "to_string", {TypedExpr{ast::BorrowExpr<NamedFunction>{TypedExpr{ast::IdentExpr{"x"}}}}}}};
      }

      f.header.type.output = leaf_type(anyf::type_id<std::string>());

      return f;
    })
    .and_then([&](TypedFunction f) { return overload_resolution(r.env, std::move(f)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); })
    .map([&](Tree<Binding> t) {
      std::vector<anyf::Future> futures = take(std::move(t));
      return anyf::any_cast<std::string>(std::move(futures[0]).wait());
    });
}

ContextualResult<void> run_assign(RuntimeEnv& r, UnTypedAssignment a) {
  return type_name_resolution(r.env, std::move(a.type))
    .and_then([&](CompoundType<TypeID> type) {
      return infer_header_from_env(r, TypedScope{{}, {std::move(a.expr)}}, std::move(type));
    })
    .map(lift_only_borrowed_idents)
    .and_then([&](TypedFunction f) { return overload_resolution(r.env, std::move(f)); })
    .and_then([&](CheckedFunction f) {
      return type_check(r.env, a.pattern, *f.header.type.output).and_then([&](const auto&) {
        return run_function(r, f);
      });
    })
    .map([&](Tree<Binding> results) {
      co_visit(a.pattern, results, [&](const ast::Pattern&, Tree<Binding>& tree, const ast::Ident& ident, const auto&) {
        r.bindings[ident.name] = std::move(tree);
      });
    });
}

} // namespace

Tree<Any> await(Tree<Binding> tree) {
  return knot::map<Tree<Any>>(std::move(tree), [](Binding b) -> Any { return take(std::move(b)).wait(); });
}

CompoundType<TypeID> type(const Tree<Binding>& tree) {
  return knot::map<CompoundType<TypeID>>(tree, [](const Binding& b) { return b.type; });
}

RuntimeEnv make_default_runtime(Env env) { return {std::move(env), anyf::make_task_executor()}; }

Result<void> parse_script(Env& e, std::string_view script) {
  return parse(script)
    .map_error([&](auto errors) { return contextualize(script, std::move(errors)); })
    .and_then([&](AST ast) {
      std::vector<std::string> errors;

      for(const auto& [name, f] : ast) {
        auto graph_result = type_name_resolution(e, f)
                              .and_then([&](TypedFunction f) { return overload_resolution(e, std::move(f)); })
                              .and_then([&](CheckedFunction f) {
                                auto fg_result = create_graph(e, f);
                                return merge(std::move(f.header.type), std::move(fg_result));
                              })
                              .map_error([&](auto errors) { return contextualize(script, std::move(errors)); });

        if(graph_result) {
          auto&& [type, graph] = std::move(*graph_result);
          e.add_graph(name, {std::move(type)}, std::move(graph));
        } else {
          errors.insert(errors.begin(), graph_result.error().begin(), graph_result.error().end());
        }
      }

      return errors.empty() ? Result<void>{} : tl::unexpected{std::move(errors)};
    });
}

Result<Tree<Binding>> run(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .and_then([&](UnTypedExpr e) { return run_expr(r, std::move(e)); })
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

Result<Tree<Binding>> run_or_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .and_then([&](auto var) {
      return std::visit(
        Overloaded{
          [&](UnTypedExpr e) { return run_expr(r, std::move(e)); },
          [&](UnTypedAssignment a) { return run_assign(r, std::move(a)).map([]() { return Tree<Binding>{}; }); },
        },
        std::move(var));
    })
    .map_error([&](auto errors) { return contextualize(assignment_or_expr, std::move(errors)); });
}

Result<std::string> run_to_string(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .and_then([&](UnTypedExpr e) { return run_expr_to_string(r, std::move(e)); })
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

Result<std::string> run_to_string_or_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .and_then([&](auto var) {
      return std::visit(Overloaded{
                          [&](UnTypedExpr e) { return run_expr_to_string(r, std::move(e)); },
                          [&](UnTypedAssignment a) {
                            return run_assign(r, std::move(a)).map([]() { return std::string{}; });
                            ;
                          },
                        },
                        std::move(var));
    })
    .map_error([&](auto errors) { return contextualize(assignment_or_expr, std::move(errors)); });
}

int main(int argc, const char** argv, Env e) {
  const std::optional<Command> cmd = parse_cmd_line(argc, argv);

  if(!cmd) {
    const char* msg = "Usage:\n"
                      "  run [scripts...]\n"
                      "  repl [scripts...]\n";

    fmt::print("{}", msg);
    return 1;
  }

  const auto result = parse_scripts(e, cmd->filenames).and_then([&]() {
    if(cmd->run_main) {
      RuntimeEnv r = make_default_runtime(std::move(e));
      return run_to_string(r, "main()").map([](std::string s) { return make_vector(std::move(s)); });
    } else {
      run_repl(make_default_runtime(std::move(e)));
      return Result<std::vector<std::string>>{};
    }
  });

  if(result) {
    dump(result.value());
  } else {
    dump(result.error());
  }

  return !result.has_value();
}

} // namespace ooze
