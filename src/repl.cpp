#include "pch.h"

#include "bindings.h"
#include "io.h"
#include "parser_combinators.h"
#include "repl.h"
#include "src_map.h"

#include "ooze/core.h"

#include <CLI/CLI.hpp>

namespace ooze {

namespace {

struct CLIState {
  CLI::App app;

  CLI::App* run_cmd = nullptr;
  CLI::App* repl_cmd = nullptr;

  std::vector<std::string> scripts;
  std::vector<std::string> run_args;

  CLIState() : app{"", "ooze"} {
    run_cmd = app.add_subcommand("run", "");
    run_cmd->add_option("script", scripts, "")->required()->expected(1, -1);
    run_cmd->add_option("--args", run_args, "arguments to main()");

    repl_cmd = app.add_subcommand("repl", "");
    repl_cmd->add_option("script", scripts, "");
  }
};

StringResult<void, Env> parse_scripts(Env e, const std::vector<std::string>& filenames) {
  std::vector<StringResult<std::string>> srcs = transform_to_vec(filenames, read_text_file);

  std::vector<std::string> errors = knot::accumulate(srcs, std::vector<std::string>{}, [](auto acc, const auto& r) {
    return r ? std::move(acc) : to_vec(std::move(r.error()), std::move(acc));
  });

  return errors.empty() ? std::move(e).parse_scripts(
                            transform_to_vec(srcs, [](const auto& r) { return std::string_view{r.value()}; }))
                        : StringResult<void, Env>{Failure{std::move(errors)}, std::move(e)};
}

struct HelpCmd {};
struct EvalCmd {
  std::string file;
};
struct BindingsCmd {};
struct FunctionsCmd {};
struct TypesCmd {};
struct DropCmd {
  std::string var;
};

auto help_parser() { return pc::construct<HelpCmd>(pc::constant("h", "h")); }

auto eval_parser() { return pc::construct<EvalCmd>(pc::seq(pc::constant("e", "e"), pc::any())); }

auto bindings_parser() { return pc::construct<BindingsCmd>(pc::constant("b", "b")); }

auto functions_parser() { return pc::construct<FunctionsCmd>(pc::constant("f", "f")); }

auto types_parser() { return pc::construct<TypesCmd>(pc::constant("t", "t")); }

auto drop_parser() { return pc::construct<DropCmd>(pc::seq(pc::constant("d", "d"), pc::any())); }

auto cmd_parser() {
  return pc::choose(help_parser(), eval_parser(), bindings_parser(), functions_parser(), types_parser(), drop_parser());
}

auto parse_command(std::string_view line) {
  std::vector<std::string> split;

  auto pos = line.begin();
  while(pos != line.end()) {
    auto next_pos = std::find_if(pos, line.end(), [](char c) { return c == ' '; });
    split.emplace_back(pos, next_pos);
    pos = next_pos == line.end() ? line.end() : next_pos + 1;
  }

  return pc::parse(cmd_parser(), Span<std::string>{split}).map_error([](const auto&) {
    return std::vector<std::string>{"Error parsing command"};
  });
}

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, const HelpCmd&) {
  return std::tuple(
    std::vector<std::string>{{":h - This message"},
                             {":b - List all bindings (* means they are not ready, & means they are borrowed)"},
                             {":f - List all native and script functions"},
                             {":t - List all registered types and their capabilities"},
                             {":d binding - Drop the given binding"}},
    std::move(env));
}

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, BindingsCmd) {
  const auto bindings = env.bindings();

  std::vector<std::string> output;
  output.reserve(bindings.size() + 1);
  output.push_back(fmt::format("{} binding(s)", bindings.size()));

  for(const auto& [name, type, state] : env.bindings()) {
    output.push_back(fmt::format("  {}: {}{}",
                                 name,
                                 state == BindingState::Ready ? "" : (state == BindingState::Borrowed ? "&" : "*"),
                                 env.pretty_print(type)));
  }

  return {std::move(output), std::move(env)};
}

constexpr auto convert_errors = [](std::vector<std::string> errors, auto&&... ts) {
  return success(knot::Type<std::vector<std::string>>{}, std::move(errors), std::forward<decltype(ts)>(ts)...);
};

constexpr auto convert_errors_futures = [](std::vector<std::string> errors, auto&&... ts) {
  return success(
    knot::Type<std::vector<std::string>>{}, Future(Any(std::move(errors))), std::forward<decltype(ts)>(ts)...);
};

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .append_state(std::move(env))
    .and_then([](std::string script, Env env) { return std::move(env).parse_scripts(make_sv_array(script)); })
    .map([](Env env) { return std::tuple(std::vector<std::string>{}, std::move(env)); })
    .or_else(convert_errors)
    .value_and_state();
}

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, const FunctionsCmd&) {
  const auto functions = sorted(transform_to_vec(env.globals(), flattened([&](std::string name, Type type) {
                                                   return std::pair(std::move(name), env.pretty_print(type));
                                                 })));

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};

  auto it = functions.begin();
  while(it != functions.end()) {
    const auto& [name, type] = *it;
    const auto next_it = std::find_if(it, functions.end(), [&](const auto& p) { return p.first != it->first; });

    if(const auto count = std::distance(it, next_it); count == 1) {
      output.push_back(fmt::format("  {}: {}", name, type));
    } else if(count <= 5) {
      std::for_each(it, next_it, [&](const auto& p) { output.push_back(fmt::format("  {}{}", p.first, p.second)); });
    } else {
      output.push_back(fmt::format("  {} [{} overloads]", name, count));
    }

    it = next_it;
  }

  return {std::move(output), std::move(env)};
}

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, const TypesCmd&) {
  const auto types = sorted(transform_to_vec(env.native_types().names, [&](const auto& p) {
    return std::pair(p.first, env.type_check_fn(fmt::format("(x: &{}) -> string = to_string(x)", p.first)).has_value());
  }));

  std::vector<std::string> output{fmt::format("{} type(s)", types.size())};
  for(const auto& [name, info] : types) {
    output.push_back(fmt::format("  {:20} [to_string: {}]", name, info ? "Y" : "N"));
  }

  return {std::move(output), std::move(env)};
}

std::tuple<std::vector<std::string>, Env> run(Executor&, Env env, const DropCmd& cmd) {
  return env.drop(cmd.var) ? std::tuple(std::vector<std::string>{}, std::move(env))
                           : std::tuple(make_vector(fmt::format("Binding {} not found", cmd.var)), std::move(env));
}

} // namespace

std::tuple<Future, Env> step_repl(Executor& executor, Env env, std::string_view line) {
  if(line.empty()) {
    return {Future(Any(std::vector<std::string>{})), std::move(env)};
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .append_state(std::move(env))
      .map(visited([&](const auto& cmd, Env env) { return run(executor, std::move(env), cmd); }))
      .map([](auto output, Env env) { return std::tuple(Future(Any(std::move(output))), std::move(env)); })
      .or_else(convert_errors_futures)
      .value_and_state();
  } else {
    return std::move(env)
      .run_to_string(executor, line)
      .map([](Future out, Env env) {
        return std::tuple(std::move(out).then([](Any any) {
          std::string output = any_cast<std::string>(std::move(any));
          return Any(output.empty() ? std::vector<std::string>{} : make_vector(std::move(output)));
        }),
                          std::move(env));
      })
      .or_else(convert_errors_futures)
      .value_and_state();
  }
}

void run_repl_recursive(Executor& executor, Env env) {
  std::string line;
  if(std::getline(std::cin, line)) {
    Future output;
    std::tie(output, env) = step_repl(executor, std::move(env), line);

    // Move-only fn please
    auto sp = std::make_shared<Env>(std::move(env));
    return std::move(output).then([&executor, sp = std::move(sp)](Any output) mutable {
      for(const auto& line : any_cast<std::vector<std::string>>(output)) {
        fmt::print("{}\n", line);
      }

      fmt::print("> ");
      return run_repl_recursive(executor, std::move(*sp));
    });
  }
}

void run_repl(Executor& executor, Env env) {
  fmt::print("Welcome to the ooze repl!\n");
  fmt::print("Try :h for help. Use Ctrl^D to exit.\n");
  fmt::print("> ");

  run_repl_recursive(executor, std::move(env));
}

int repl_main(int argc, const char** argv, Env env) {
  CLIState cli;

  try {
    cli.app.parse(argc, argv);
  } catch(const CLI::ParseError& e) {
    return cli.app.exit(e);
  }

  auto result = parse_scripts(std::move(env), cli.scripts);

  if(!result) {
    for(const std::string& line : result.error()) {
      fmt::print("{}\n", line);
    }
    return 1;
  }

  std::tie(env) = std::move(result.state());

  int ret = 0;

  {
    Executor executor = make_tbb_executor(4);

    if(cli.run_cmd->parsed()) {
      const auto extract_return = [&ret](Binding b, Env env) {
        take(std::move(b.values[0])).then([&](Any a) { ret = any_cast<i32>(a); });
        return env;
      };

      const auto return_success = [](Binding, Env env) { return env; };

      const auto dump_error = [&ret](auto errors, Env env) {
        for(const std::string& line : errors) {
          fmt::print("{}\n", line);
        }

        ret = 1;
        return env;
      };

      env.insert("args", std::move(cli.run_args));

      if(auto tc_result = env.type_check("main", "fn(string_vector) -> i32"); tc_result) {
        std::move(env).run(executor, "main(args)").map(extract_return).map_error(dump_error);
      } else if(env.type_check("main", "fn(string_vector) -> ()")) {
        std::move(env).run(executor, "main(args)").map(return_success).map_error(dump_error);
      } else if(env.type_check("main", "fn() -> i32")) {
        std::move(env).run(executor, "main()").map(extract_return).map_error(dump_error);
      } else if(env.type_check("main", "fn() -> ()")) {
        std::move(env).run(executor, "main()").map(return_success).map_error(dump_error);
      } else {
        std::move(tc_result).append_state(std::move(env)).map_error(dump_error);
      }
    } else {
      run_repl(executor, std::move(env));
    }
  } // Executor goes out of scope

  return ret;
}

} // namespace ooze
