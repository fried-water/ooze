#include "pch.h"

#include "bindings.h"
#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "overload_resolution.h"
#include "parser.h"
#include "parser_combinators.h"
#include "queries.h"
#include "repl.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

#include <fstream>
#include <regex>
#include <sstream>

namespace ooze {

namespace {

template <typename T>
struct Option {
  std::string value;
};

struct Verbose {};

using ScriptOption = Option<struct SOpt>;
using OutputOption = Option<struct OOpt>;
using TakeOption = Option<struct TOpt>;
using ReturnOption = Option<struct ROpt>;
using ContainOption = Option<struct COpt>;

struct RunCommand {
  std::string script;
  std::string expr;
  std::string output_prefix;
  int verbosity = 0;
};

struct FunctionQueryCommand {
  std::string regex = ".*";
  std::vector<std::string> takes;
  std::vector<std::string> returns;
  std::vector<std::string> contains;
};

struct TypeQueryCommand {
  std::string regex = ".*";
};

auto o_opt_parser() { return pc::construct<OutputOption>(pc::seq(pc::constant("-o", "-o"), pc::any())); }
auto s_opt_parser() { return pc::construct<ScriptOption>(pc::seq(pc::constant("-s", "-s"), pc::any())); }
auto t_opt_parser() { return pc::construct<TakeOption>(pc::seq(pc::constant("-t", "-t"), pc::any())); }
auto r_opt_parser() { return pc::construct<ReturnOption>(pc::seq(pc::constant("-r", "-r"), pc::any())); }
auto c_opt_parser() { return pc::construct<ContainOption>(pc::seq(pc::constant("-c", "-c"), pc::any())); }

auto v_parser() {
  return pc::transform(pc::constant("-v", "-v"), []() { return Verbose{}; });
}

auto run_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("run", "run"), pc::n(pc::choose(v_parser(), s_opt_parser(), o_opt_parser(), pc::any()))),
    [](std::vector<std::variant<Verbose, ScriptOption, OutputOption, std::string>> args) {
      RunCommand cmd;

      for(auto&& arg : args) {
        std::visit(Overloaded{[&](ScriptOption o) { cmd.script = std::move(o.value); },
                              [&](OutputOption o) { cmd.output_prefix = std::move(o.value); },
                              [&](std::string expr) { cmd.expr = std::move(expr); },
                              [&](Verbose) { cmd.verbosity++; }},
                   std::move(arg));
      }

      return cmd;
    });
}

auto function_query_cmd_parser() {
  return pc::transform(pc::seq(pc::constant("functions", "functions"),
                               pc::n(pc::choose(t_opt_parser(), r_opt_parser(), c_opt_parser(), pc::any()))),
                       [](std::vector<std::variant<TakeOption, ReturnOption, ContainOption, std::string>> args) {
                         FunctionQueryCommand cmd;

                         for(auto&& arg : args) {
                           std::visit(Overloaded{[&](TakeOption o) { cmd.takes.push_back(std::move(o.value)); },
                                                 [&](ReturnOption o) { cmd.returns.push_back(std::move(o.value)); },
                                                 [&](ContainOption o) { cmd.contains.push_back(std::move(o.value)); },
                                                 [&](std::string r) { cmd.regex = std::move(r); }},
                                      std::move(arg));
                         }

                         return cmd;
                       });
}

auto type_query_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("types", "types"), pc::maybe(pc::any())),
    [](std::optional<std::string> regex) { return regex ? TypeQueryCommand{std::move(*regex)} : TypeQueryCommand{}; });
}

auto cmd_parser() {
  return pc::maybe(pc::choose(run_cmd_parser(), type_query_cmd_parser(), function_query_cmd_parser()));
}

void run(const RunCommand& cmd, Env e) {
  Result<std::string> script = cmd.script.empty() ? std::string() : read_text_file(cmd.script);

  anyf::TaskExecutor executor;

  script.map([&](const std::string& script) { return parse_script(std::move(e), script); })
    .and_then([&](auto pair) mutable {
      return pair.second.empty() ? Result<Env>(std::move(pair.first)) : tl::unexpected{std::move(pair.second)};
    })
    .and_then([&](Env e) {
      auto res = run(e, executor, cmd.expr, {}).second;
      return merge(std::move(e), std::move(res));
    })
    .map([&](auto p) {
      auto [e, outputs] = std::move(p);
      dump(to_string(e, executor, std::move(outputs)));
    })
    .or_else(dump);
}

void run(const FunctionQueryCommand& cmd, const Env& e) {
  const std::regex re(cmd.regex);

  std::vector<std::string> functions;
  std::transform(
    e.functions.begin(), e.functions.end(), std::back_inserter(functions), [](const auto& p) { return p.first; });

  std::sort(functions.begin(), functions.end());

  for(const std::string& fn_name : functions) {
    for(const auto& f : e.functions.at(fn_name)) {
      const auto find_type_id = [&](const std::string& type) {
        if(const auto it = e.type_ids.find(type); it == e.type_ids.end()) {
          fmt::print("type {} not found", type);
          std::exit(1);
        } else {
          return it->second;
        }
      };

      const auto doesnt_have =
        Overloaded{[&](const std::vector<TypeProperties>& types, const std::string& type) {
                     const TypeID exp = find_type_id(type);
                     return std::none_of(types.begin(), types.end(), [&](auto t) { return t.id == exp; });
                   },
                   [&](const std::vector<TypeID>& types, const std::string& type) {
                     return std::find(types.begin(), types.end(), find_type_id(type)) == types.end();
                   }};

      const auto doesnt_take = [&](const std::string& type) { return doesnt_have(input_types(f), type); };
      const auto doesnt_return = [&](const std::string& type) { return doesnt_have(output_types(f), type); };
      const auto doesnt_contain = [&](const std::string& type) { return doesnt_take(type) && doesnt_return(type); };

      if(!std::regex_match(fn_name, re)) continue;
      if(std::any_of(cmd.takes.begin(), cmd.takes.end(), doesnt_take)) continue;
      if(std::any_of(cmd.returns.begin(), cmd.returns.end(), doesnt_return)) continue;
      if(std::any_of(cmd.contains.begin(), cmd.contains.end(), doesnt_contain)) continue;

      fmt::print("{}\n", function_string(e, fn_name, f));
    }
  }
}

void run(const TypeQueryCommand& cmd, const Env& e) {
  const std::regex re(cmd.regex);

  std::vector<std::string> types;
  std::transform(
    e.type_names.begin(), e.type_names.end(), std::back_inserter(types), [](const auto& p) { return p.second; });

  std::sort(types.begin(), types.end());

  for(const std::string& type : types) {
    if(std::regex_match(type, re)) {
      fmt::print("{}\n", type);
    }
  }
}

} // namespace

std::pair<Env, std::vector<std::string>> parse_script(Env e, std::string_view script) {
  auto result = parse(script)
                  .map_error([&](const ParseError& error) {
                    return std::vector<std::string>{generate_error_msg("<script>", error)};
                  })
                  .map([&](AST ast) { return create_graphs(std::move(e), ast); });

  return result ? std::move(result.value()) : std::pair(std::move(e), std::move(result.error()));
}

Result<FunctionGraph> parse_expr(const Env& e, std::string_view expr) {
  return parse_expr(expr)
    .map_error(
      [&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<cmdline expr>", error)}; })
    .and_then([&](ast::Expr expr) { return create_graph(e, expr, {}); });
}

Result<std::vector<std::pair<std::string, TypeProperties>>>
expr_inputs(const Env& e, const Map<std::string, Binding>& bindings, const ast::Expr& expr) {
  const std::vector<std::pair<std::string, TypeProperties>> inputs =
    inputs_of(e, expr, [&](const std::string& binding) {
      const auto it = bindings.find(binding);
      return it != bindings.end() ? std::optional(it->second.type) : std::nullopt;
    });

  std::vector<std::string> errors;
  for(const auto& [name, type] : inputs) {
    if(const auto it = bindings.find(name); it == bindings.end()) {
      errors.push_back(fmt::format("Binding {} not found", name));
    } else if(it->second.type != type.id) {
      errors.push_back(fmt::format(
        "{} expected {} but is {}", name, type_name_or_id(e, type.id), type_name_or_id(e, it->second.type)));
    }
  }

  if(errors.empty()) {
    return inputs;
  } else {
    return tl::unexpected{std::move(errors)};
  }
}

std::pair<std::unordered_map<std::string, Binding>, Result<std::vector<Binding>>>
run_expr(const Env& e,
         anyf::TaskExecutor& executor,
         const ast::Expr& expr,
         std::unordered_map<std::string, Binding> bindings,
         std::optional<std::vector<ast::Binding>> expected_bindings = {}) {
  auto res = expr_inputs(e, bindings, expr)
               .and_then([&](auto inputs) {
                 auto g = create_graph(e, expr, inputs).and_then([&](auto g) {
                   return expected_bindings
                            ? type_check(e, *expected_bindings, output_types(g)).map([&]() { return std::move(g); })
                            : Result<FunctionGraph>{std::move(g)};
                 });

                 return merge(std::move(g), std::move(inputs));
               })
               .map([&](auto p) {
                 auto [g, inputs] = std::move(p);

                 std::vector<anyf::Future> value_inputs;
                 std::vector<anyf::BorrowedFuture> borrowed_inputs;

                 for(const auto& [name, type] : inputs) {
                   if(type.value) {
                     value_inputs.push_back(take(bindings, name).value().second);
                   } else {
                     borrowed_inputs.push_back(borrow(bindings, name).value().second);
                   }
                 }

                 auto futures = anyf::execute_graph(g, executor, std::move(value_inputs), std::move(borrowed_inputs));

                 std::vector<Binding> output_bindings;
                 output_bindings.reserve(futures.size());
                 for(size_t i = 0; i < futures.size(); i++) {
                   output_bindings.push_back({output_types(g)[i], std::move(futures[i])});
                 }

                 return output_bindings;
               });

  return {std::move(bindings), std::move(res)};
}

std::pair<std::unordered_map<std::string, Binding>, Result<std::vector<Binding>>>
run(const Env& e,
    anyf::TaskExecutor& executor,
    std::string_view assignment_or_expr,
    std::unordered_map<std::string, Binding> bindings) {
  auto res =
    parse_repl(assignment_or_expr)
      .map_error([&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<src>", error)}; })
      .map([&](std::variant<ast::Expr, ast::Assignment> var) {
        return std::visit(Overloaded{[&](ast::Assignment assignment) {
                                       Result<std::vector<Binding>> result;
                                       std::tie(bindings, result) = run_expr(
                                         e, executor, assignment.expr, std::move(bindings), assignment.bindings);

                                       result = std::move(result).map([&](std::vector<Binding> results) {
                                         for(size_t i = 0; i < results.size(); i++) {
                                           bindings[assignment.bindings[i].name] = std::move(results[i]);
                                         }
                                         return std::vector<Binding>{};
                                       });

                                       return std::pair(std::move(bindings), std::move(result));
                                     },
                                     [&](ast::Expr expr) { return run_expr(e, executor, expr, std::move(bindings)); }},
                          std::move(var));
      });

  return res ? std::move(res.value()) : std::pair(std::move(bindings), tl::unexpected{std::move(res.error())});
}

std::vector<std::string> to_string(const Env& e, anyf::TaskExecutor& executor, std::vector<Binding> bindings) {
  std::vector<std::pair<TypeID, Result<anyf::Future>>> results;
  results.reserve(bindings.size());

  for(Binding& b : bindings) {
    results.emplace_back(
      b.type,
      overload_resolution(e, "to_string", {{b.type, false}}, Span<TypeID>{anyf::type_id<std::string>()})
        .map([&](EnvFunction f) {
          return std::visit(
            Overloaded{
              [&](AnyFunction f) { return borrow(b).then([f = std::move(f)](Any any) { return f({&any}).front(); }); },
              [&](const FunctionGraph& g) {
                return std::move(anyf::execute_graph(g, executor, {}, {borrow(b)}).front());
              }},
            std::move(f));
        }));
  }

  std::vector<std::string> outputs;
  outputs.reserve(bindings.size());

  for(auto& [type, result] : results) {
    outputs.push_back(result ? anyf::any_cast<std::string>(std::move(result.value()).wait())
                             : fmt::format("[Object of {}]", type_name_or_id(e, type)));
  }

  return outputs;
}

int main(int argc, char* argv[], Env e) {

  std::vector<std::string> args;
  for(int i = 1; i < argc; i++) {
    args.push_back(argv[i]);
  }

  const auto cmd_result = pc::parse(cmd_parser(), Span<std::string>{args});

  if(!cmd_result) {
    const char* msg = "Usage:\n"
                      "  run [-s script] expr\n"
                      "  repl [-s script]\n"
                      "  types [regex]\n"
                      "  functions [-t takes] [-r returns] [-c contains] [regex]\n";

    fmt::print("{}", msg);
  } else if(*cmd_result) {
    std::visit([&](const auto& cmd) { run(cmd, std::move(e)); }, **cmd_result);
  } else {
    run_repl(e);
  }

  return 0;
}

} // namespace ooze
