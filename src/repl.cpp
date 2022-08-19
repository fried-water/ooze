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
struct SaveCmd {
  std::string var;
  std::string file;
};
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

auto save_parser() { return pc::construct<SaveCmd>(pc::seq(pc::constant("s", "s"), pc::any(), pc::any())); }

auto release_parser() { return pc::construct<ReleaseCmd>(pc::seq(pc::constant("r", "r"), pc::any())); }

auto await_parser() { return pc::construct<AwaitCmd>(pc::seq(pc::constant("a", "a"), pc::n(pc::any()))); }

auto cmd_parser() {
  return pc::choose(help_parser(),
                    eval_parser(),
                    bindings_parser(),
                    functions_parser(),
                    types_parser(),
                    save_parser(),
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

std::vector<std::string> run(Repl&, const HelpCmd& help) {
  return {{":h - This message"},
          {":b - List all bindings (* means they are not ready)"},
          {":f - List all environment and script functions"},
          {":t - List all registered types and their capabilities"},
          {":s binding file - Serialize the given binding asynchronously"},
          {":r binding - Release the given binding"},
          {":a bindings... - Await the given bindings or everything if unspecified"}};
}

std::vector<std::string> run(Repl& repl, BindingsCmd) {
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

std::vector<std::string> run(Repl& repl, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .map([&](std::string script) {
      std::vector<std::string> errors;
      std::tie(repl.env, errors) = parse_script(std::move(repl.env), script, [](const Env& e, const std::string& name) {
        return read_binary_file(name).and_then([&](const auto& bytes) { return load(e, bytes, name); });
      });

      return errors;
    })
    .or_else(convert_errors)
    .value();
}

std::vector<std::string> run(Repl& repl, const FunctionsCmd&) {
  const Env& e = repl.env;

  std::vector<std::pair<std::string, std::string>> functions;

  for(const auto& [name, fs] : e.functions) {
    if(name != "clone" && name != "to_string") {
      for(const auto& f : fs) {
        functions.emplace_back(name, function_string(e, name, f));
      }
    }
  }

  std::sort(functions.begin(), functions.end());

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};

  if(const auto it = e.functions.find("clone"); it != e.functions.end()) {
    output.push_back(fmt::format("  clone [{} overloads]", it->second.size()));
  }

  if(const auto it = e.functions.find("to_string"); it != e.functions.end()) {
    output.push_back(fmt::format("  to_string [{} overloads]", it->second.size()));
  }

  for(const auto& [name, str] : functions) {
    output.push_back(fmt::format("  {}", str));
  }

  return output;
}

std::vector<std::string> run(Repl& repl, const TypesCmd&) {
  const Env& e = repl.env;

  std::map<std::string, std::tuple<bool, bool, bool>> types;

  for(const auto& [id, name] : e.type_names) {
    types[type_name_or_id(e, id)] = {};
  }

  const auto to_string_it = e.functions.find("to_string");
  if(to_string_it != e.functions.end()) {
    for(const auto& f : to_string_it->second) {
      if(input_types(f).size() == 1) {
        std::get<0>(types[type_name_or_id(e, input_types(f).front().id)]) = true;
      }
    }
  }

  for(const auto& [id, _] : e.serialize) {
    std::get<1>(types[type_name_or_id(e, id)]) = true;
  }

  for(const auto& [id, _] : e.deserialize) {
    std::get<2>(types[type_name_or_id(e, id)]) = true;
  }

  std::vector<std::string> output{fmt::format("{} type(s)", types.size())};
  for(const auto& [name, info] : types) {
    output.push_back(fmt::format(
      "  {:20} [to_string: {}, serialize: {}{}]",
      name,
      std::get<0>(info) ? "Y" : "N",
      std::get<1>(info) ? "Y" : "N",
      std::get<1>(info) == std::get<2>(info) ? "" : fmt::format(", deserialize: {}", std::get<2>(info) ? "Y" : "N")));
  }

  return output;
}

std::vector<std::string> run(Repl& repl, const SaveCmd& cmd) {
  auto res = borrow(repl.bindings, cmd.var);

  if(!res) {
    return std::move(res.error());
  }

  auto& [type, bf] = res.value();

  const auto serialize_it = repl.env.serialize.find(type);

  if(serialize_it == repl.env.serialize.end()) {
    err(fmt::format("{} ({}) does not support serialization", cmd.var, type_name_or_id(repl.env, type)));
  }

  repl.outstanding_writes.emplace_back(cmd.var, type, bf.then([&repl, file = cmd.file](const Any& v) {
    return save(repl.env, v).and_then([&](const auto& bytes) { return write_binary_file(file, bytes); });
  }));

  return {};
}

std::vector<std::string> run(Repl& repl, const ReleaseCmd& cmd) {
  return take(repl.bindings, cmd.var)
    .map([](const auto&) { return std::vector<std::string>{}; })
    .map_error(convert_errors)
    .value();
}

std::vector<std::string> run(Repl& repl, const AwaitCmd& cmd) {
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

std::vector<std::string> step_repl(Repl& repl, std::string_view line) {
  for(auto it = repl.outstanding_writes.begin(); it != repl.outstanding_writes.end();) {
    auto& [binding, type, future] = *it;

    if(future.ready()) {
      const Result<void> result = anyf::any_cast<Result<void>>(std::move(future).wait());
      if(!result) {
        fmt::print("Error serializing {} ({})", binding, type_name_or_id(repl.env, type));
        if(result.error().size() == 1) {
          fmt::print(": {}\n", result.error().front());
        } else {
          fmt::print("\n");
          for(const std::string& msg : result.error()) {
            fmt::print("  {}\n", msg);
          }
        }
      }

      it = repl.outstanding_writes.erase(it);
    } else {
      ++it;
    }
  }

  if(line.empty()) {
    return {};
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .map([&](const auto& cmd) { return std::visit([&](const auto& cmd) { return run(repl, cmd); }, cmd); })
      .or_else(convert_errors)
      .value();
  } else {
    Result<std::vector<Binding>> results;
    std::tie(repl.bindings, results) =
      run(repl.env, repl.executor, line, std::move(repl.bindings), [&](const Env& e, const std::string& name) {
        return read_binary_file(name).and_then([&](const auto& bytes) { return load(e, bytes, name); });
      });

    return std::move(results)
      .map([&](std::vector<Binding> bindings) { return to_string(repl.env, repl.executor, std::move(bindings)); })
      .or_else(convert_errors)
      .value();
  }
}

void run_repl(Env e) {
  Repl repl{std::move(e)};

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

  for(auto& [binding, type, future] : repl.outstanding_writes) {
    const Result<void> result = anyf::any_cast<Result<void>>(std::move(future).wait());
    if(!result) {
      fmt::print("Error serializing {} ({})", binding, type_name_or_id(e, type));
      if(result.error().size() == 1) {
        fmt::print(": {}\n", result.error().front());
      } else {
        fmt::print("\n");
        for(const std::string& msg : result.error()) {
          fmt::print("  {}\n", msg);
        }
      }
    }
  }
}

} // namespace ooze
