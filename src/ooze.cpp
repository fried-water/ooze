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
  return pc::transform(pc::constant("-v", "-v"), [](const auto&) { return Verbose{}; });
}

auto run_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("run", "run"), pc::n(pc::choose(v_parser(), s_opt_parser(), o_opt_parser(), pc::any()))),
    [](const auto&, std::vector<std::variant<Verbose, ScriptOption, OutputOption, std::string>> args) {
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
  return pc::transform(
    pc::seq(pc::constant("functions", "functions"),
            pc::n(pc::choose(t_opt_parser(), r_opt_parser(), c_opt_parser(), pc::any()))),
    [](const auto&, std::vector<std::variant<TakeOption, ReturnOption, ContainOption, std::string>> args) {
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
  return pc::transform(pc::seq(pc::constant("types", "types"), pc::maybe(pc::any())),
                       [](const auto&, std::optional<std::string> regex) {
                         return regex ? TypeQueryCommand{std::move(*regex)} : TypeQueryCommand{};
                       });
}

auto cmd_parser() {
  return pc::maybe(pc::choose(run_cmd_parser(), type_query_cmd_parser(), function_query_cmd_parser()));
}

void run(const RunCommand& cmd, Env e) {
  Result<std::string> script = cmd.script.empty() ? std::string() : read_text_file(cmd.script);

  script.and_then([&](const std::string& script) { return parse_script(e, script); })
    .and_then([&]() {
      RuntimeEnv r{std::move(e)};
      return run_to_string(r, cmd.expr);
    })
    .map(dump)
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

std::vector<std::string> gather_binding_strings(std::vector<Binding> bindings) {
  return knot::map<std::vector<std::string>>(
    std::move(bindings), [](Binding b) { return anyf::any_cast<std::string>(take(std::move(b)).wait()); });
}

Result<TypedFunction> overload_resolution(const RuntimeEnv& r, const std::variant<UnTypedExpr, UnTypedAssignment>& v) {
  return overload_resolution(
    r.env, v, knot::map<std::unordered_map<std::string, TypeID>>(r.bindings, [](const Binding& b) { return b.type; }));
}

UnTypedExpr to_string_wrap(UnTypedExpr expr) {
  return {Indirect{ast::Call<NamedFunction>{{"to_string"}, {std::move(expr)}}}};
}

auto to_string_wrap(std::variant<UnTypedExpr, UnTypedAssignment> var) {
  if(auto expr = std::get_if<UnTypedExpr>(&var); expr) {
    var = to_string_wrap(std::move(*expr));
  }
  return var;
}

template <typename T>
Result<T> check_and_wrap(const RuntimeEnv& r, T t) {
  if(auto base_result = overload_resolution(r, t); base_result) {
    return to_string_wrap(std::move(t));
  } else {
    return tl::unexpected{std::move(base_result.error())};
  }
}

std::vector<Binding>
assign_bindings(RuntimeEnv& r, const std::variant<UnTypedExpr, UnTypedAssignment>& var, std::vector<Binding> results) {
  if(const auto assign = std::get_if<UnTypedAssignment>(&var); assign) {
    for(size_t i = 0; i < results.size(); i++) {
      r.bindings[assign->bindings[i].name] = std::move(results[i]);
    }

    results.clear();
  }

  return results;
}

} // namespace

Result<void> parse_script(Env& e, std::string_view script) {
  return parse(script).map_error(generate_error_msg).and_then([&](AST ast) {
    std::vector<std::string> errors;

    for(const auto& f : ast) {
      auto graph_result = overload_resolution(e, f).and_then(
        [&](const TypedFunction& typed_function) { return create_graph(e, typed_function); });

      if(graph_result) {
        e.add_graph(f.name, std::move(*graph_result));
      } else {
        errors.insert(errors.begin(), graph_result.error().begin(), graph_result.error().end());
      }
    }

    return errors.empty() ? Result<void>{} : tl::unexpected{std::move(errors)};
  });
}

Result<std::vector<Binding>> run_function(RuntimeEnv& r, const TypedFunction& f) {
  return create_graph(r.env, f).map([&](anyf::FunctionGraph g) {
    std::vector<anyf::Future> value_inputs;
    std::vector<anyf::BorrowedFuture> borrowed_inputs;

    for(const auto& [name, type, borrowed] : f.parameters) {
      if(borrowed) {
        borrowed_inputs.push_back(borrow(r.bindings, name).value().second);
      } else {
        value_inputs.push_back(take(r.bindings, name).value().second);
      }
    }

    auto futures = anyf::execute_graph(g, r.executor, std::move(value_inputs), std::move(borrowed_inputs));

    std::vector<Binding> result_bindings;
    result_bindings.reserve(futures.size());
    for(size_t i = 0; i < futures.size(); i++) {
      result_bindings.push_back({output_types(g)[i], std::move(futures[i])});
    }

    return result_bindings;
  });
}

Result<std::vector<Binding>> run_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .map_error(generate_error_msg)
    .and_then([&](auto var) { return merge(var, overload_resolution(r, var)); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); });
}

Result<std::vector<Binding>> run(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .map_error(generate_error_msg)
    .and_then([&](UnTypedExpr e) { return overload_resolution(r, e); })
    .and_then([&](TypedFunction f) { return run_function(r, f); });
}

Result<std::vector<std::string>> run_to_string(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .map_error(generate_error_msg)
    .and_then([&](UnTypedExpr e) { return check_and_wrap(r, e); })
    .and_then([&](UnTypedExpr e) { return overload_resolution(r, e); })
    .and_then([&](TypedFunction f) { return run_function(r, f); })
    .map(gather_binding_strings);
}

Result<std::vector<std::string>> run_to_string_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .map_error(generate_error_msg)
    .and_then([&](auto var) { return check_and_wrap(r, var); })
    .and_then([&](auto var) { return merge(var, overload_resolution(r, var)); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); })
    .map(gather_binding_strings);
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
