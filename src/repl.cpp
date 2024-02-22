#include "pch.h"

#include "bindings.h"
#include "io.h"
#include "parser_combinators.h"
#include "pretty_print.h"
#include "repl.h"

#include "ooze/core.h"
#include "ooze/executor/sequential_executor.h"
#include "ooze/executor/tbb_executor.h"

#include <CLI/CLI.hpp>

#include <iostream>
#include <map>

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

  return errors.empty()
           ? parse_scripts(std::move(e),
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
struct ReleaseCmd {
  std::string var;
};
struct AwaitCmd {
  std::vector<std::string> bindings;
};

auto help_parser() { return pc::construct<HelpCmd>(pc::constant("h", "h")); }

auto eval_parser() { return pc::construct<EvalCmd>(pc::seq(pc::constant("e", "e"), pc::any())); }

auto bindings_parser() { return pc::construct<BindingsCmd>(pc::constant("b", "b")); }

auto functions_parser() { return pc::construct<FunctionsCmd>(pc::constant("f", "f")); }

auto types_parser() { return pc::construct<TypesCmd>(pc::constant("t", "t")); }

auto release_parser() { return pc::construct<ReleaseCmd>(pc::seq(pc::constant("r", "r"), pc::any())); }

auto await_parser() { return pc::construct<AwaitCmd>(pc::seq(pc::constant("a", "a"), pc::n(pc::any()))); }

auto cmd_parser() {
  return pc::choose(
    help_parser(),
    eval_parser(),
    bindings_parser(),
    functions_parser(),
    types_parser(),
    release_parser(),
    await_parser());
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

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const HelpCmd&) {
  return std::tuple(
    std::vector<std::string>{
      {":h - This message"},
      {":b - List all bindings (* means they are not ready, & means they are borrowed)"},
      {":f - List all environment and script functions"},
      {":t - List all registered types and their capabilities"},
      {":r binding - Release the given binding"},
      {":a bindings... - Await the given bindings or everything if unspecified"}},
    std::move(env),
    std::move(bindings));
}

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, BindingsCmd) {
  const auto ordered = sorted(transform_to_vec(bindings, Get<0>{}));

  std::vector<std::string> output;
  output.reserve(bindings.size() + 1);
  output.push_back(fmt::format("{} binding(s)", bindings.size()));

  for(const std::string& binding_name : ordered) {
    std::stringstream tree_ss;

    const auto& binding = bindings.at(binding_name);
    const BindingState max_state = std::accumulate(
      binding.values.begin(), binding.values.end(), BindingState::Ready, [](BindingState acc, const AsyncValue& ele) {
        const BindingState ele_state = find_binding_state(ele);
        return i32(acc) > i32(ele_state) ? acc : ele_state;
      });

    tree_ss << pretty_print(env.ast.tg, env.native_types.names, binding.type);
    output.push_back(fmt::format(
      "  {}: {}{}",
      binding_name,
      max_state == BindingState::Ready ? "" : (max_state == BindingState::Borrowed ? "&" : "*"),
      std::move(tree_ss).str()));
  }

  return {std::move(output), std::move(env), std::move(bindings)};
}

constexpr auto convert_errors = [](std::vector<std::string> errors, auto&&... ts) {
  return success(knot::Type<std::vector<std::string>>{}, std::move(errors), std::forward<decltype(ts)>(ts)...);
};

constexpr auto convert_errors_futures = [](std::vector<std::string> errors, auto&&... ts) {
  return success(
    knot::Type<std::vector<std::string>>{}, Future(Any(std::move(errors))), std::forward<decltype(ts)>(ts)...);
};

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .append_state(std::move(env))
    .and_then([](std::string script, Env env) { return parse_scripts(std::move(env), make_sv_array(script)); })
    .map([](Env env) { return std::tuple(std::vector<std::string>{}, std::move(env)); })
    .or_else(convert_errors)
    .append_state(std::move(bindings))
    .value_and_state();
}

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const FunctionsCmd&) {
  std::vector<std::pair<std::string_view, std::string>> functions;

  const auto srcs = make_sv_array(env.src);

  for(const ASTID id : env.ast.forest.root_ids()) {
    const ASTID ident = *env.ast.forest.first_child(id);
    const Type type = env.ast.types[ident.get()];
    if(env.ast.tg.get<TypeTag>(type) == TypeTag::Fn) {
      const std::string_view name = sv(srcs, env.ast.srcs[ident.get()]);
      functions.emplace_back(name, pretty_print_fn_type(env.ast.tg, env.native_types.names, type));
    }
  }

  functions = sorted(std::move(functions));

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};

  auto it = functions.begin();
  while(it != functions.end()) {
    const auto& [name, type] = *it;
    const auto next_it = std::find_if(it, functions.end(), [&](const auto& p) { return p.first != it->first; });

    if(const auto count = std::distance(it, next_it); count == 1) {
      output.push_back(fmt::format("  {}{}", name, type));
    } else if(count <= 5) {
      std::for_each(it, next_it, [&](const auto& p) { output.push_back(fmt::format("  {}{}", p.first, p.second)); });
    } else {
      output.push_back(fmt::format("  {} [{} overloads]", name, count));
    }

    it = next_it;
  }

  return {std::move(output), std::move(env), std::move(bindings)};
}

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const TypesCmd&) {
  const auto types = sorted(transform_to_vec(env.native_types.names, [&](const auto& p) {
    return std::pair(p.first,
                     type_check_fn(env, fmt::format("(x: &{}) -> string = to_string(x)", p.first)).has_value());
  }));

  std::vector<std::string> output{fmt::format("{} type(s)", types.size())};
  for(const auto& [name, info] : types) {
    output.push_back(fmt::format("  {:20} [to_string: {}]", name, info ? "Y" : "N"));
  }

  return {std::move(output), std::move(env), std::move(bindings)};
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(ExecutorRef, Env env, Bindings bindings, const ReleaseCmd& cmd) {
  if(const auto it = bindings.find(cmd.var); it != bindings.end()) {
    bindings.erase(it);
    return {std::vector<std::string>{}, std::move(env), std::move(bindings)};
  } else {
    return {make_vector(fmt::format("Binding {} not found", cmd.var)), std::move(env), std::move(bindings)};
  }
}

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const AwaitCmd& cmd) {
  std::vector<std::string> output;
  if(cmd.bindings.empty()) {
    for(auto& [name, binding] : bindings) {
      for(AsyncValue& v : binding.values) {
        v = await(std::move(v));
      }
    }
  } else {
    for(const std::string& binding : cmd.bindings) {
      if(const auto it = bindings.find(binding); it != bindings.end()) {
        for(AsyncValue& v : it->second.values) {
          v = await(std::move(v));
        }
      } else {
        output.push_back(fmt::format("Binding {} not found", binding));
      }
    }
  }

  return {std::move(output), std::move(env), std::move(bindings)};
}

} // namespace

std::tuple<Future, Env, Bindings> step_repl(ExecutorRef executor, Env env, Bindings bindings, std::string_view line) {
  if(line.empty()) {
    return {Future(Any(std::vector<std::string>{})), std::move(env), std::move(bindings)};
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .append_state(std::move(env), std::move(bindings))
      .map(visited([&](const auto& cmd, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), cmd);
      }))
      .map([](auto output, Env env, Bindings bindings) {
        return std::tuple(Future(Any(std::move(output))), std::move(env), std::move(bindings));
      })
      .or_else(convert_errors_futures)
      .value_and_state();
  } else {
    return run_to_string(executor, std::move(env), std::move(bindings), line)
      .map([](Future out, Env env, Bindings bindings) {
        return std::tuple(std::move(out).then([](Any any) {
          std::string output = any_cast<std::string>(std::move(any));
          return Any(output.empty() ? std::vector<std::string>{} : make_vector(std::move(output)));
        }),
                          std::move(env),
                          std::move(bindings));
      })
      .or_else(convert_errors_futures)
      .value_and_state();
  }
}

void run_repl_recursive(ExecutorRef executor, Env env, Bindings bindings) {
  std::string line;
  if(std::getline(std::cin, line)) {
    Future output;
    std::tie(output, env, bindings) = step_repl(executor, std::move(env), std::move(bindings), line);

    // Move-only fn please
    auto sp = std::make_shared<std::pair<Env, Bindings>>(std::move(env), std::move(bindings));
    return std::move(output).then([executor, sp = std::move(sp)](Any output) mutable {
      for(const auto& line : any_cast<std::vector<std::string>>(output)) {
        fmt::print("{}\n", line);
      }

      fmt::print("> ");
      return run_repl_recursive(executor, std::move(sp->first), std::move(sp->second));
    });
  }
}

void run_repl(ExecutorRef executor, Env env, Bindings bindings) {
  fmt::print("Welcome to the ooze repl!\n");
  fmt::print("Try :h for help. Use Ctrl^D to exit.\n");
  fmt::print("> ");

  run_repl_recursive(executor, std::move(env), std::move(bindings));
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
      const auto extract_return = [&ret](Binding b, Env env, Bindings bindings) {
        take(std::move(b.values[0])).then([&](Any a) { ret = any_cast<i32>(a); });
        return std::tuple(std::move(env), std::move(bindings));
      };

      const auto return_success = [](Binding, Env env, Bindings bindings) {
        return std::tuple(std::move(env), std::move(bindings));
      };

      const auto dump_error = [&ret](auto errors, Env e, Bindings b) {
        for(const std::string& line : errors) {
          fmt::print("{}\n", line);
        }

        ret = 1;
        return std::tuple(std::move(e), std::move(b));
      };

      const Type arg_type = env.ast.tg.add_node(TypeTag::Leaf, type_id(knot::Type<std::vector<std::string>>{}));
      Bindings bindings;
      bindings.emplace("args", Binding{arg_type, make_vector(AsyncValue{Future{Any{std::move(cli.run_args)}}})});

      if(auto tc_result = type_check_binding(env, "main: fn(string_vector) -> i32"); tc_result) {
        run(executor, std::move(env), std::move(bindings), "main(args)").map(extract_return).map_error(dump_error);
      } else if(type_check_binding(env, "main: fn(string_vector) -> ()")) {
        run(executor, std::move(env), std::move(bindings), "main(args)").map(return_success).map_error(dump_error);
      } else if(type_check_binding(env, "main: fn() -> i32")) {
        run(executor, std::move(env), std::move(bindings), "main()").map(extract_return).map_error(dump_error);
      } else if(type_check_binding(env, "main: fn() -> ()")) {
        run(executor, std::move(env), std::move(bindings), "main()").map(return_success).map_error(dump_error);
      } else {
        std::move(tc_result).append_state(std::move(env), std::move(bindings)).map_error(dump_error);
      }
    } else {
      run_repl(executor, std::move(env), {});
    }
  } // Executor goes out of scope

  return ret;
}

} // namespace ooze
