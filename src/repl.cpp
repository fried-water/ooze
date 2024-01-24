#include "pch.h"

#include "bindings.h"
#include "io.h"
#include "parser_combinators.h"
#include "pretty_print.h"
#include "repl.h"

#include "ooze/core.h"
#include "ooze/executor/task_executor.h"

#include <CLI/CLI.hpp>

#include <iostream>
#include <map>

namespace ooze {

namespace {

struct CLIState {
  CLI::App app;

  CLI::App* run_cmd = nullptr;
  CLI::App* repl_cmd = nullptr;

  std::vector<std::string> run_scripts;
  std::vector<std::string> run_args;

  std::vector<std::string> repl_scripts;

  CLIState() : app{"", "ooze"} {
    run_cmd = app.add_subcommand("run", "");
    run_cmd->add_option("script", run_scripts, "")->required()->expected(1, -1);

    repl_cmd = app.add_subcommand("repl", "");
    repl_cmd->add_option("script", repl_scripts, "");
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

std::tuple<std::vector<std::string>, Env, Bindings> run(ExecutorRef, Env env, Bindings bindings, const HelpCmd& help) {
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
  std::vector<std::string> ordered = sorted(transform_to_vec(bindings, [](const auto& p) { return p.first; }));

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

    tree_ss << pretty_print(make_sv_array(env.src), env.tg, env.native_types.names, binding.type);
    output.push_back(fmt::format(
      "  {}: {}{}",
      binding_name,
      max_state == BindingState::Ready ? "" : (max_state == BindingState::Borrowed ? "&" : "*"),
      std::move(tree_ss).str()));
  }

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

constexpr auto convert_errors = [](std::vector<std::string> errors, auto&&... ts) {
  return success(knot::Type<std::vector<std::string>>{}, std::move(errors), std::forward<decltype(ts)>(ts)...);
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
    if(env.tg.get<TypeTag>(type) == TypeTag::Fn) {
      const std::string_view name = sv(srcs, env.ast.srcs[ident.get()]);
      functions.emplace_back(name, pretty_print_fn_type(srcs, env.tg, env.native_types.names, type));
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

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
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

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(ExecutorRef, Env env, Bindings bindings, const ReleaseCmd& cmd) {
  if(const auto it = bindings.find(cmd.var); it != bindings.end()) {
    bindings.erase(it);
    return std::tuple(std::vector<std::string>{}, std::move(env), std::move(bindings));
  } else {
    return std::tuple(make_vector(fmt::format("Binding {} not found", cmd.var)), std::move(env), std::move(bindings));
  }
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(ExecutorRef executor, Env env, Bindings bindings, const AwaitCmd& cmd) {
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

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

} // namespace

std::tuple<std::vector<std::string>, Env, Bindings>
step_repl(ExecutorRef executor, Env env, Bindings bindings, std::string_view line) {
  if(line.empty()) {
    return std::tuple(std::vector<std::string>{}, std::move(env), std::move(bindings));
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .append_state(std::move(env), std::move(bindings))
      .map(visited([&](const auto& cmd, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), cmd);
      }))
      .or_else(convert_errors)
      .value_and_state();
  } else {
    return run_to_string(executor, std::move(env), std::move(bindings), line)
      .map([](std::string out, Env env, Bindings bindings) {
        return std::tuple(
          out.empty() ? std::vector<std::string>{} : make_vector(std::move(out)), std::move(env), std::move(bindings));
      })
      .or_else(convert_errors)
      .value_and_state();
  }
}

std::tuple<Env, Bindings> run_repl(ExecutorRef executor, Env env, Bindings bindings) {
  fmt::print("Welcome to the ooze repl!\n");
  fmt::print("Try :h for help. Use Ctrl^D to exit.\n");
  fmt::print("> ");

  std::string line;
  std::vector<std::string> output;

  while(std::getline(std::cin, line)) {
    std::tie(output, env, bindings) = step_repl(executor, std::move(env), std::move(bindings), line);

    for(const auto& line : output) {
      fmt::print("{}\n", line);
    }

    fmt::print("> ");
  }

  return {std::move(env), std::move(bindings)};
}

int repl_main(int argc, const char** argv, Env e) {
  CLIState cli;

  try {
    cli.app.parse(argc, argv);
  } catch(const CLI::ParseError& e) {
    return cli.app.exit(e);
  }

  Executor executor = make_task_executor();

  const auto result =
    cli.run_cmd->parsed()
      ? parse_scripts(std::move(e), cli.run_scripts).append_state(Bindings{}).and_then([&](Env env, Bindings bindings) {
          return run_to_string(executor, std::move(env), std::move(bindings), "main()")
            .map([](std::string s, Env e, Bindings b) {
              return std::tuple(make_vector(std::move(s)), std::move(e), std::move(b));
            });
        })
      : parse_scripts(std::move(e), cli.repl_scripts).append_state(Bindings{}).map([&](Env env, Bindings bindings) {
          std::tie(env, bindings) = run_repl(executor, std::move(env), std::move(bindings));
          return std::tuple(std::vector<std::string>{}, std::move(env), std::move(bindings));
        });

  for(const std::string& line : result ? result.value() : result.error()) {
    fmt::print("{}\n", line);
  }

  return !result.has_value();
}

} // namespace ooze
