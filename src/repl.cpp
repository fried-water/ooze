#include "pch.h"

#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "parser.h"
#include "repl.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

#include <iostream>

namespace ooze {

void run_repl(const Env& e, std::string_view script) {
  auto graph_result =
    parse(script)
      .map_error(
        [&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<script>", error)}; })
      .and_then([&](AST ast) {
        return create_graphs(e, ast, [&](const std::string& name) { return *load(e, read_binary_file(name), name); });
      })
      .or_else(dump_errors);

  if(!graph_result) {
    return;
  }

  if(!script.empty()) {
    fmt::print("loaded {}\n", script);
  }

  const Map<std::string, FunctionGraph> graphs = std::move(graph_result.value());
  Map<std::string, std::pair<anyf::Future, TypeID>> vars;

  anyf::TaskExecutor executor;

  const auto consume_var = [&](const auto& name) {
    if(const auto it = vars.find(name); it != vars.end()) {
      Any val = std::move(it->second);
      return val;
    } else {
      fmt::print("Binding {} not found\n", name);
      exit(1);
    }
  };

  std::string line;
  fmt::print("> ");

  while(std::getline(std::cin, line) && line != "exit") {
    if(!line.empty()) {

      if(line == ":env") {
        for(const auto& [v, f] : vars) {
          fmt::print("{} {}{}\n", type_name_or_id(e, f.second), v, f.first.ready() ? "" : "*");
        }
      } else {
        const auto res =
          parse_repl(line)
            .map_error(
              [&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<repl>", error)}; })
            .map([&](std::variant<ast::Expr, ast::Assignment> var) {
              return std::visit(Overloaded{[](ast::Assignment assignment) {
                                             return std::pair(std::optional(std::move(assignment.variables)),
                                                              std::move(assignment.expr));
                                           },
                                           [](ast::Expr expr) {
                                             return std::pair(std::optional<std::vector<ast::Binding>>(),
                                                              std::move(expr));
                                           }},
                                std::move(var));
            })
            .and_then([&](auto p) {
              auto [opt_bindings, expr] = std::move(p);

              std::vector<std::pair<std::string, TypeProperties>> inputs;
              if(std::holds_alternative<std::string>(expr.v)) {
                const std::string var = std::get<std::string>(expr.v);
                const auto it = vars.find(var);
                if(it != vars.end()) {
                  inputs.emplace_back(var, TypeProperties{it->second.second});
                }
              } else {
                inputs = inputs_of(e, expr);
              }

              return create_graph(e,
                                  expr,
                                  graphs,
                                  inputs,
                                  [&](const std::string& name) { return *load(e, read_binary_file(name), name); })
                .map([&, b = opt_bindings](FunctionGraph g) {
                  return std::tuple(std::move(g), std::move(inputs), std::move(b));
                });
            })
            .and_then([&](auto tup) {
              const auto& [g, inputs, opt_bindings] = tup;

              std::vector<anyf::Future> value_inputs;
              std::vector<anyf::BorrowedFuture> borrowed_inputs;

              std::vector<std::string> errors;

              for(const auto& [name, type] : inputs) {
                const auto it = vars.find(name);

                if(it == vars.end()) {
                  errors.push_back(fmt::format("{} not found", name));
                } else {
                  auto& [future, var_type] = it->second;

                  if(var_type != type.id) {
                    errors.push_back(fmt::format(
                      "{} expected {} but is {}", name, type_name_or_id(e, type.id), type_name_or_id(e, var_type)));
                  } else if(type.value) {
                    value_inputs.push_back(std::move(future));
                    vars.erase(it);
                  } else {
                    auto [bf, next_future] = borrow(std::move(future));
                    borrowed_inputs.push_back(std::move(bf));
                    future = std::move(next_future);
                  }
                }
              }

              auto results = anyf::execute_graph(g, executor, std::move(value_inputs), std::move(borrowed_inputs));
              if(opt_bindings) {
                check(opt_bindings->size() == results.size(), "abc");
                for(size_t i = 0; i < results.size(); i++) {
                  vars[(*opt_bindings)[i].name] = std::pair(std::move(results[i]), output_types(g)[i].id);
                }
              } else {
                for(size_t i = 0; i < results.size(); i++) {
                  Any any = std::move(results[i]).wait();

                  if(const auto it = e.to_string.find(any.type()); it != e.to_string.end()) {
                    fmt::print("{}\n", it->second(any));
                  } else {
                    fmt::print("[Object of {}]\n", type_name_or_id(e, any.type()));
                  }
                }
              }

              return errors.empty() ? success(std::tuple()) : tl::unexpected{std::move(errors)};
            })
            .or_else(dump_errors);
      }
    }

    fmt::print("> ");
  }
}

} // namespace ooze
