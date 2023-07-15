#include "pch.h"

#include "bindings.h"
#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "parser.h"
#include "parser_combinators.h"
#include "pretty_print.h"
#include "repl.h"
#include "type_check.h"

#include <iostream>
#include <map>

namespace ooze {

namespace {

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

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef, Env env, Bindings bindings, const HelpCmd& help) {
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

std::tuple<std::vector<std::string>, Env, Bindings> run(anyf::ExecutorRef, Env env, Bindings bindings, BindingsCmd) {
  std::vector<std::string> ordered = sorted(transform_to_vec(bindings, [](const auto& p) { return p.first; }));

  std::vector<std::string> output;
  output.reserve(bindings.size() + 1);
  output.push_back(fmt::format("{} binding(s)", bindings.size()));

  for(const std::string& binding : ordered) {
    std::stringstream tree_ss;
    tree_to_string(tree_ss, bindings.at(binding), [&](std::ostream& os, const Binding& binding) {
      const auto& [type, f, b] = binding;
      os << fmt::format(
        "{}{}", f.ready() || (b.valid() && b.unique()) ? "" : (b.valid() ? "&" : "*"), pretty_print(env, type));
    });
    output.push_back(fmt::format("  {}: {}", binding, std::move(tree_ss).str()));
  }

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

constexpr auto convert_errors = [](std::vector<std::string> errors, auto&&... ts) {
  return success<std::vector<std::string>>(std::move(errors), std::forward<decltype(ts)>(ts)...);
};

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef, Env env, Bindings bindings, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .append_state(std::move(env))
    .and_then([](std::string script, Env env) { return parse_script(std::move(env), script); })
    .map([](Env env) { return std::tuple(std::vector<std::string>{}, std::move(env)); })
    .or_else(convert_errors)
    .append_state(std::move(bindings))
    .value_and_state();
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef, Env env, Bindings bindings, const FunctionsCmd&) {
  std::vector<std::pair<std::string, std::string>> functions;

  const std::array<std::string, 4> COLLAPSE{{"clone", "to_string", "serialize", "deserialize"}};

  for(const auto& [name, fs] : env.functions) {
    if(std::find(COLLAPSE.begin(), COLLAPSE.end(), name) == COLLAPSE.end()) {
      for(const auto& f : fs) {
        functions.emplace_back(name, fmt::format("{}{}", name, pretty_print(env, f.type)));
      }
    }
  }

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};

  for(const std::string& name : COLLAPSE) {
    if(const auto it = env.functions.find(name); it != env.functions.end()) {
      output.push_back(fmt::format("  {} [{} overloads]", name, it->second.size()));
    }
  }

  for(const auto& [name, str] : sorted(std::move(functions))) {
    output.push_back(fmt::format("  {}", str));
  }

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef, Env env, Bindings bindings, const TypesCmd&) {
  std::map<std::string, bool> types;

  for(const auto& [id, name] : env.type_names) {
    TypedFunction to_string_wrap{
      {{make_vector(ast::Pattern{ast::Ident{"x"}})},
       {tuple_type<TypeID>(make_vector(borrow_type(leaf_type(id)))), leaf_type(anyf::type_id<std::string>())}},
      {TypedCallExpr{{"to_string"}, {{ast::IdentExpr{"x"}}}}}};

    types[pretty_print(env, id)] = overload_resolution(env, std::move(to_string_wrap)).has_value();
  }

  std::vector<std::string> output{fmt::format("{} type(s)", types.size())};
  for(const auto& [name, info] : types) {
    output.push_back(fmt::format("  {:20} [to_string: {}]", name, info ? "Y" : "N"));
  }

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef, Env env, Bindings bindings, const ReleaseCmd& cmd) {
  return take(bindings, cmd.var)
    .map([](const auto&) { return std::vector<std::string>{}; })
    .or_else(convert_errors)
    .append_state(std::move(env), std::move(bindings))
    .value_and_state();
}

std::tuple<std::vector<std::string>, Env, Bindings>
run(anyf::ExecutorRef executor, Env env, Bindings bindings, const AwaitCmd& cmd) {
  std::vector<std::string> output;
  if(cmd.bindings.empty()) {
    for(auto& [binding, entry] : bindings) {
      knot::preorder(entry, [&](Binding& b) { b.future = anyf::Future(executor, std::move(b.future).wait()); });
    }
  } else {
    for(const std::string& binding : cmd.bindings) {
      if(const auto it = bindings.find(binding); it != bindings.end()) {
        knot::preorder(it->second, [&](Binding& b) { b.future = anyf::Future(executor, std::move(b.future).wait()); });
      } else {
        output.push_back(fmt::format("Binding {} not found", binding));
      }
    }
  }

  return std::tuple(std::move(output), std::move(env), std::move(bindings));
}

} // namespace

std::tuple<std::vector<std::string>, Env, Bindings>
step_repl(anyf::ExecutorRef executor, Env env, Bindings bindings, std::string_view line) {
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

std::tuple<Env, Bindings> run_repl(anyf::ExecutorRef executor, Env env, Bindings bindings) {
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

} // namespace ooze
