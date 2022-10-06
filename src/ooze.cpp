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

struct RunCommand {
  std::string script;
  std::string expr;
  std::string output_prefix;
  int verbosity = 0;
};

auto o_opt_parser() { return pc::construct<OutputOption>(pc::seq(pc::constant("-o", "-o"), pc::any())); }
auto s_opt_parser() { return pc::construct<ScriptOption>(pc::seq(pc::constant("-s", "-s"), pc::any())); }
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

std::vector<std::string> gather_binding_strings(std::vector<Binding> bindings) {
  return knot::map<std::vector<std::string>>(
    std::move(bindings), [](Binding b) { return anyf::any_cast<std::string>(take(std::move(b)).wait()); });
}

Result<CheckedFunction> overload_resolution(const RuntimeEnv& r, TypedBody b) {
  return overload_resolution(
    r.env, std::move(b), knot::map<std::unordered_map<std::string, TypeID>>(r.bindings, [](const Binding& b) {
      return b.type;
    }));
}

Result<TypedBody> check_and_wrap(const RuntimeEnv& r, TypedBody b) {
  if(auto base_result = overload_resolution(r, b); base_result) {
    for(UnTypedExpr& expr : b.result) {
      expr = UnTypedExpr{Indirect{ast::Call<NamedFunction>{{"to_string"}, {std::move(expr)}}}};
    }
    return b;
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

Result<std::vector<Binding>> run_function(RuntimeEnv& r, const CheckedFunction& f) {
  return create_graph(r.env, f).map([&](anyf::FunctionGraph g) {
    std::vector<anyf::Future> value_inputs;
    std::vector<anyf::BorrowedFuture> borrowed_inputs;

    for(const auto& [name, type, borrowed] : f.header.parameters) {
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

} // namespace

Result<void> parse_script(Env& e, std::string_view script) {
  return parse(script).map_error(generate_error_msg).and_then([&](AST ast) {
    std::vector<std::string> errors;

    for(const auto& f : ast) {
      auto graph_result = type_name_resolution(e, f)
                            .and_then([&](TypedFunction f) { return overload_resolution(e, std::move(f)); })
                            .and_then([&](const CheckedFunction& f) { return create_graph(e, f); });

      if(graph_result) {
        e.add_graph(f.name, std::move(*graph_result));
      } else {
        errors.insert(errors.begin(), graph_result.error().begin(), graph_result.error().end());
      }
    }

    return errors.empty() ? Result<void>{} : tl::unexpected{std::move(errors)};
  });
}

Result<std::vector<Binding>> run(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .map_error(generate_error_msg)
    .and_then([&](UnTypedExpr e) { return type_name_resolution(r.env, convert_to_function_body(std::move(e))); })
    .and_then([&](TypedBody b) { return overload_resolution(r, std::move(b)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); });
}

Result<std::vector<Binding>> run_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .map_error(generate_error_msg)
    .and_then([&](auto var) { return merge(var, type_name_resolution(r.env, convert_to_function_body(var))); })
    .and_then(
      [&](auto tup) { return merge(std::move(std::get<0>(tup)), overload_resolution(r, std::move(std::get<1>(tup)))); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); });
}

Result<std::vector<std::string>> run_to_string(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .map_error(generate_error_msg)
    .and_then([&](UnTypedExpr e) { return type_name_resolution(r.env, convert_to_function_body(std::move(e))); })
    .and_then([&](TypedBody b) { return check_and_wrap(r, std::move(b)); })
    .and_then([&](TypedBody b) { return overload_resolution(r, std::move(b)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); })
    .map(gather_binding_strings);
}

Result<std::vector<std::string>> run_to_string_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .map_error(generate_error_msg)
    .and_then([&](auto var) { return merge(var, type_name_resolution(r.env, convert_to_function_body(var))); })
    .and_then([&](auto tup) {
      return std::holds_alternative<UnTypedAssignment>(std::get<0>(tup))
               ? merge(std::move(std::get<0>(tup)), std::move(std::get<1>(tup)))
               : merge(std::move(std::get<0>(tup)), check_and_wrap(r, std::move(std::get<1>(tup))));
    })
    .and_then(
      [&](auto tup) { return merge(std::move(std::get<0>(tup)), overload_resolution(r, std::move(std::get<1>(tup)))); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); })
    .map(gather_binding_strings);
}

int main(int argc, char* argv[], Env e) {

  std::vector<std::string> args;
  for(int i = 1; i < argc; i++) {
    args.push_back(argv[i]);
  }

  const auto cmd_result = pc::parse(pc::maybe(run_cmd_parser()), Span<std::string>{args});

  if(!cmd_result) {
    const char* msg = "Usage:\n"
                      "  run [-s script] expr\n"
                      "  repl [-s script]\n";

    fmt::print("{}", msg);
  } else if(*cmd_result) {
    run(**cmd_result, std::move(e));
  } else {
    run_repl(e);
  }

  return 0;
}

} // namespace ooze
