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

#include <anyf/graph_execution.h>

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
  return pc::choose(help_parser(),
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

std::vector<std::string> run(RuntimeEnv&, const HelpCmd& help) {
  return {{":h - This message"},
          {":b - List all bindings (* means they are not ready, & means they are borrowed)"},
          {":f - List all environment and script functions"},
          {":t - List all registered types and their capabilities"},
          {":r binding - Release the given binding"},
          {":a bindings... - Await the given bindings or everything if unspecified"}};
}

std::vector<std::string> run(RuntimeEnv& repl, BindingsCmd) {
  std::vector<std::string> output{fmt::format("{} binding(s)", repl.bindings.size())};

  std::vector<std::string> ordered;
  for(const auto& [binding, entry] : repl.bindings) {
    ordered.push_back(binding);
  }
  std::sort(ordered.begin(), ordered.end());

  for(const auto& binding : ordered) {
    const auto& [type, f, b] = repl.bindings.at(binding);
    output.push_back(fmt::format("  {}: {}{}",
                                 binding,
                                 type_name_or_id(repl.env, type),
                                 f.ready() || (b.valid() && b.unique()) ? "" : (b.valid() ? "&" : "*")));
  }

  return output;
}

std::vector<std::string> run(RuntimeEnv& repl, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .and_then([&](std::string script) { return parse_script(repl.env, script); })
    .map([]() { return std::vector<std::string>{}; })
    .or_else(convert_errors)
    .value();
}

std::vector<std::string> run(RuntimeEnv& repl, const FunctionsCmd&) {
  const Env& e = repl.env;

  std::vector<std::pair<std::string, std::string>> functions;

  const std::array<std::string, 4> COLLAPSE{{"clone", "to_string", "serialize", "deserialize"}};

  for(const auto& [name, fs] : e.functions) {
    if(std::find(COLLAPSE.begin(), COLLAPSE.end(), name) == COLLAPSE.end()) {
      for(const auto& f : fs) {
        functions.emplace_back(name, function_string(e, name, f));
      }
    }
  }

  std::sort(functions.begin(), functions.end());

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};

  for(const std::string& name : COLLAPSE) {
    if(const auto it = e.functions.find(name); it != e.functions.end()) {
      output.push_back(fmt::format("  {} [{} overloads]", name, it->second.size()));
    }
  }

  for(const auto& [name, str] : functions) {
    output.push_back(fmt::format("  {}", str));
  }

  return output;
}

std::vector<std::string> run(RuntimeEnv& repl, const TypesCmd&) {
  const Env& e = repl.env;

  std::map<std::string, bool> types;

  for(const auto& [id, name] : e.type_names) {
    types[type_name_or_id(e, id)] = {};
  }

  const auto to_string_it = e.functions.find("to_string");
  if(to_string_it != e.functions.end()) {
    for(const auto& f : to_string_it->second) {
      if(input_types(f).size() == 1) {
        types[type_name_or_id(e, input_types(f).front().id)] = true;
      }
    }
  }

  std::vector<std::string> output{fmt::format("{} type(s)", types.size())};
  for(const auto& [name, info] : types) {
    output.push_back(fmt::format("  {:20} [to_string: {}]", name, info ? "Y" : "N"));
  }

  return output;
}

std::vector<std::string> run(RuntimeEnv& repl, const ReleaseCmd& cmd) {
  return take(repl.bindings, cmd.var)
    .map([](const auto&) { return std::vector<std::string>{}; })
    .map_error(convert_errors)
    .value();
}

std::vector<std::string> run(RuntimeEnv& repl, const AwaitCmd& cmd) {
  std::vector<std::string> output;
  if(cmd.bindings.empty()) {
    for(auto& [binding, entry] : repl.bindings) {
      entry.future = anyf::Future(repl.executor, std::move(entry.future).wait());
    }
  } else {
    for(const std::string& binding : cmd.bindings) {
      if(const auto it = repl.bindings.find(binding); it != repl.bindings.end()) {
        it->second.future = anyf::Future(repl.executor, std::move(it->second.future).wait());
      } else {
        output.push_back(fmt::format("Binding {} not found", binding));
      }
    }
  }
  return output;
}

} // namespace

std::vector<std::string> step_repl(RuntimeEnv& repl, std::string_view line) {
  if(line.empty()) {
    return {};
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .map([&](const auto& cmd) { return std::visit([&](const auto& cmd) { return run(repl, cmd); }, cmd); })
      .or_else(convert_errors)
      .value();
  } else {
    return run_to_string_assign(repl, line).or_else(convert_errors).value();
  }
}

void run_repl(Env e) {
  RuntimeEnv repl{std::move(e)};

  std::string line;
  fmt::print("Welcome to the ooze repl!\n");
  fmt::print("Try :h for help. Use Ctrl^D to exit.\n");
  fmt::print("> ");

  while(std::getline(std::cin, line)) {
    for(const auto& line : step_repl(repl, line)) {
      fmt::print("{}\n", line);
    }

    fmt::print("> ");
  }
}

} // namespace ooze
