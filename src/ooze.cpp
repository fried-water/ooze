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

#include <fstream>
#include <regex>
#include <sstream>

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
  return knot::accumulate<Result<void>>(filenames, [&](Result<void> acc, const std::string& filename) -> Result<void> {
    auto result = read_text_file(filename).and_then([&](const std::string& script) { return parse_script(e, script); });

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
  return knot::map<std::vector<std::string>>(
    std::move(bindings), [](Binding b) { return anyf::any_cast<std::string>(take(std::move(b)).wait()); });
}

ContextualResult<CheckedFunction> overload_resolution(const RuntimeEnv& r, TypedScope b) {
  return overload_resolution(
    r.env, std::move(b), knot::map<std::unordered_map<std::string, TypeID>>(r.bindings, [](const Binding& b) {
      return b.type;
    }));
}

ContextualResult<TypedScope> check_and_wrap(const RuntimeEnv& r, TypedScope b) {
  const auto to_string_wrap = [](TypedExpr e, TypeID t) {
    return t == anyf::type_id<std::string>() ? std::move(e)
                                             : TypedExpr{ast::Call<NamedFunction>{"to_string", {{std::move(e)}}}};
  };

  return overload_resolution(r, b).map([&](CheckedFunction f) {
    assert(b.assignments.size() == 0);
    assert(b.result.size() == 1);

    if(f.header.result.size() == 1) {
      return TypedScope{{}, {to_string_wrap(std::move(b.result.front()), f.header.result.front())}};
    } else {
      TypedScope new_scope{{{{}, std::move(b.result.front())}}, {}};

      int i = 0;
      for(TypeID type : f.header.result) {
        const std::string var = std::to_string(i++);
        new_scope.assignments.back().bindings.push_back({var, type});
        new_scope.result.push_back(to_string_wrap({var}, type));
      }

      return new_scope;
    }
  });
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

ContextualResult<std::vector<Binding>> run_function(RuntimeEnv& r, const CheckedFunction& f) {
  return create_graph(r.env, f).map([&](anyf::FunctionGraph g) {
    std::vector<anyf::Future> value_inputs;
    std::vector<anyf::BorrowedFuture> borrowed_inputs;

    for(const auto& [name, type, borrowed, ref] : f.header.parameters) {
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

RuntimeEnv make_default_runtime(Env env) { return {std::move(env), anyf::make_task_executor()}; }

Result<void> parse_script(Env& e, std::string_view script) {
  return parse(script)
    .map_error([&](auto errors) { return contextualize(script, std::move(errors)); })
    .and_then([&](AST ast) {
      std::vector<std::string> errors;

      for(const auto& f : ast) {
        auto graph_result = type_name_resolution(e, f)
                              .and_then([&](TypedFunction f) { return overload_resolution(e, std::move(f)); })
                              .and_then([&](const CheckedFunction& f) { return create_graph(e, f); })
                              .map_error([&](auto errors) { return contextualize(script, std::move(errors)); });

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
    .and_then([&](UnTypedExpr e) { return type_name_resolution(r.env, convert_to_scope(std::move(e))); })
    .and_then([&](TypedScope b) { return overload_resolution(r, std::move(b)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); })
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

Result<std::vector<Binding>> run_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .and_then([&](auto var) { return merge(var, type_name_resolution(r.env, convert_to_scope(var))); })
    .and_then(
      [&](auto tup) { return merge(std::move(std::get<0>(tup)), overload_resolution(r, std::move(std::get<1>(tup)))); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); })
    .map_error([&](auto errors) { return contextualize(assignment_or_expr, std::move(errors)); });
}

Result<std::vector<std::string>> run_to_string(RuntimeEnv& r, std::string_view expr) {
  return parse_expr(expr)
    .and_then([&](UnTypedExpr e) { return type_name_resolution(r.env, convert_to_scope(std::move(e))); })
    .and_then([&](TypedScope b) { return check_and_wrap(r, std::move(b)); })
    .and_then([&](TypedScope b) { return overload_resolution(r, std::move(b)); })
    .and_then([&](CheckedFunction f) { return run_function(r, f); })
    .map(gather_binding_strings)
    .map_error([&](auto errors) { return contextualize(expr, std::move(errors)); });
}

Result<std::vector<std::string>> run_to_string_assign(RuntimeEnv& r, std::string_view assignment_or_expr) {
  return parse_repl(assignment_or_expr)
    .and_then([&](auto var) { return merge(var, type_name_resolution(r.env, convert_to_scope(var))); })
    .and_then([&](auto tup) {
      return std::holds_alternative<UnTypedAssignment>(std::get<0>(tup))
               ? success<ContextualError>(std::move(tup))
               : merge(std::move(std::get<0>(tup)), check_and_wrap(r, std::move(std::get<1>(tup))));
    })
    .and_then(
      [&](auto tup) { return merge(std::move(std::get<0>(tup)), overload_resolution(r, std::move(std::get<1>(tup)))); })
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), run_function(r, std::get<1>(tup))); })
    .map([&](auto tup) { return assign_bindings(r, std::get<0>(tup), std::move(std::get<1>(tup))); })
    .map(gather_binding_strings)
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

  const auto& result = parse_scripts(e, cmd->filenames).and_then([&]() {
    if(cmd->run_main) {
      RuntimeEnv r = make_default_runtime(std::move(e));
      return run_to_string(r, "main()");
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
