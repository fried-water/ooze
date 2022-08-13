#include "pch.h"

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

Result<std::pair<TypeID, anyf::Future>> take(Repl& repl, const std::string& name) {
  if(const auto var_it = repl.bindings.find(name); var_it != repl.bindings.end()) {
    BindingEntry e = std::move(var_it->second);
    repl.bindings.erase(var_it);
    return std::pair(e.type, std::move(e.future));
  } else {
    return err(fmt::format("Binding {} not found", name));
  }
}

Result<std::pair<TypeID, anyf::BorrowedFuture>> borrow(Repl& repl, const std::string& name) {
  if(const auto var_it = repl.bindings.find(name); var_it == repl.bindings.end()) {
    return err(fmt::format("Binding {} not found", name));
  } else if(BindingEntry& e = var_it->second; e.borrowed_future.valid()) {
    return std::pair(e.type, e.borrowed_future);
  } else {
    std::tie(e.borrowed_future, e.future) = borrow(std::move(e.future));
    return std::pair(e.type, e.borrowed_future);
  }
}

std::vector<std::string> run(const Env& e, Repl&, const HelpCmd& help) {
  return {{":h - This message"},
          {":b - List all bindings (* means they are not ready)"},
          {":f - List all environment and script functions"},
          {":t - List all registered types and their capabilities"},
          {":s binding file - Serialize the given binding asynchronously"},
          {":r binding - Release the given binding"},
          {":a bindings... - Await the given bindings or everything if unspecified"}};
}

std::vector<std::string> run(const Env& e, Repl& repl, BindingsCmd) {
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
                                 type_name_or_id(e, type),
                                 f.ready() || (b.valid() && b.unique()) ? "" : (b.valid() ? "&" : "*")));
  }

  return output;
}

std::vector<std::string> run(const Env& e, Repl& repl, const EvalCmd& eval) {
  return read_text_file(eval.file)
    .and_then([&](std::string script) {
      return parse_script(e, script, [&](const std::string& name) {
        return read_binary_file(name).and_then([&](const auto& bytes) { return load(e, bytes, name); });
      });
    })
    .map([&](auto graphs) {
      std::vector<std::string> output{fmt::format("Loaded {} function(s) from {}", graphs.size(), eval.file)};
      for(auto& [name, graph] : graphs) {
        output.push_back(fmt::format("  {} {}",
                                     repl.graphs.find(name) == repl.graphs.end() ? "inserted" : "replaced",
                                     function_string(e, name, graph)));

        repl.graphs[name] = std::move(graph);
      }
      return output;
    })
    .or_else(convert_errors)
    .value();
}

std::vector<std::string> run(const Env& e, Repl& repl, const FunctionsCmd&) {
  std::vector<std::pair<std::string, std::string>> functions;

  for(const auto& [name, graph] : repl.graphs) {
    functions.emplace_back(name, function_string(e, name, graph));
  }

  for(const auto& [name, fs] : e.functions) {
    if(name != "clone" && name != "to_string") {
      for(const AnyFunction& f : fs) {
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

std::vector<std::string> run(const Env& e, Repl&, const TypesCmd&) {
  std::map<std::string, std::tuple<bool, bool, bool>> types;

  for(const auto& [id, name] : e.type_names) {
    types[type_name_or_id(e, id)] = {};
  }

  const auto to_string_it = e.functions.find("to_string");
  if(to_string_it != e.functions.end()) {
    for(const auto& f : to_string_it->second) {
      if(f.input_types().size() == 1) {
        std::get<0>(types[type_name_or_id(e, f.input_types().front().id)]) = true;
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

std::vector<std::string> run(const Env& e, Repl& repl, const SaveCmd& cmd) {
  auto res = borrow(repl, cmd.var);

  if(!res) {
    return std::move(res.error());
  }

  auto& [type, bf] = res.value();

  const auto serialize_it = e.serialize.find(type);

  if(serialize_it == e.serialize.end()) {
    err(fmt::format("{} ({}) does not support serialization", cmd.var, type_name_or_id(e, type)));
  }

  repl.outstanding_writes.emplace_back(cmd.var, type, bf.then([&e, file = cmd.file](const Any& v) {
    return save(e, v).and_then([&](const auto& bytes) { return write_binary_file(file, bytes); });
  }));

  return {};
}

std::vector<std::string> run(const Env&, Repl& repl, const ReleaseCmd& cmd) {
  return take(repl, cmd.var)
    .map([](const auto&) { return std::vector<std::string>{}; })
    .map_error(convert_errors)
    .value();
}

std::vector<std::string> run(const Env&, Repl& repl, const AwaitCmd& cmd) {
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

Result<std::vector<std::pair<std::string, TypeProperties>>>
expr_inputs(const Env& e, const Map<std::string, BindingEntry>& bindings, const ast::Expr& expr) {
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

Result<std::tuple<std::vector<anyf::Future>, std::vector<TypeID>>>
run_expr(const Env& e, Repl& repl, const ast::Expr& expr) {
  return expr_inputs(e, repl.bindings, expr)
    .and_then([&](auto inputs) {
      auto g = create_graph(e, expr, repl.graphs, inputs, [&](const std::string& name) {
        return read_binary_file(name).and_then([&](const auto& bytes) { return load(e, bytes, name); });
      });

      return merge(std::move(g), std::move(inputs));
    })
    .map([&](auto p) {
      auto [g, inputs] = std::move(p);

      std::vector<anyf::Future> value_inputs;
      std::vector<anyf::BorrowedFuture> borrowed_inputs;

      for(const auto& [name, type] : inputs) {
        if(type.value) {
          value_inputs.push_back(take(repl, name).value().second);
        } else {
          borrowed_inputs.push_back(borrow(repl, name).value().second);
        }
      }

      return std::tuple(anyf::execute_graph(g, repl.executor, std::move(value_inputs), std::move(borrowed_inputs)),
                        output_types(g));
    });
}

std::vector<std::string> bind_results(const Env& e,
                                      Repl& repl,
                                      const std::vector<TypeID>& types,
                                      const std::vector<ast::Binding>& bindings,
                                      std::vector<anyf::Future> results) {
  return type_check(e, bindings, types)
    .map([&]() {
      for(size_t i = 0; i < results.size(); i++) {
        repl.bindings[bindings[i].name] = BindingEntry{types[i], std::move(results[i])};
      }
      return std::vector<std::string>{};
    })
    .map_error(convert_errors)
    .value();
}

std::vector<std::string> wait_and_dump_results(const Env& e, std::vector<anyf::Future> results) {
  std::vector<std::string> output;

  for(auto& future : results) {
    Any any = std::move(future).wait();

    if(const auto to_string_function = overload_resolution(e, "to_string", {any.type()}); to_string_function) {
      const auto& f = to_string_function.value();
      if(f.output_types() == std::vector<TypeID>{anyf::type_id<std::string>()}) {
        output.push_back(anyf::any_cast<std::string>(to_string_function.value()({&any}).front()));
      } else {
        output.push_back(fmt::format("[Object of {}]", type_name_or_id(e, any.type())));
      }
    } else {
      output.push_back(fmt::format("[Object of {}]", type_name_or_id(e, any.type())));
    }
  }

  return output;
}

} // namespace

std::vector<std::string> step_repl(const Env& e, Repl& repl, std::string_view line) {
  for(auto it = repl.outstanding_writes.begin(); it != repl.outstanding_writes.end();) {
    auto& [binding, type, future] = *it;

    if(future.ready()) {
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

      it = repl.outstanding_writes.erase(it);
    } else {
      ++it;
    }
  }

  if(line.empty()) {
    return {};
  } else if(line[0] == ':') {
    return parse_command({line.data() + 1, line.size() - 1})
      .map([&](const auto& cmd) { return std::visit([&](const auto& cmd) { return run(e, repl, cmd); }, cmd); })
      .or_else(convert_errors)
      .value();
  } else {
    return parse_repl(line)
      .map_error([&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<repl>", error)}; })
      .and_then([&](std::variant<ast::Expr, ast::Assignment> var) {
        return std::visit(Overloaded{[&](ast::Assignment assignment) {
                                       return run_expr(e, repl, assignment.expr).map([&](auto tup) {
                                         return bind_results(
                                           e, repl, std::get<1>(tup), assignment.bindings, std::move(std::get<0>(tup)));
                                       });
                                     },
                                     [&](ast::Expr expr) {
                                       return run_expr(e, repl, expr).map([&](auto tup) {
                                         return wait_and_dump_results(e, std::move(std::get<0>(tup)));
                                       });
                                     }},
                          std::move(var));
      })
      .or_else(convert_errors)
      .value();
  }
}

void run_repl(const Env& e) {
  Repl repl;

  std::string line;
  fmt::print("Welcome to the ooze repl!\n");
  fmt::print("Try :h for help. Use Ctrl^D to exit.\n");
  fmt::print("> ");

  while(std::getline(std::cin, line)) {
    for(const auto& line : step_repl(e, repl, line)) {
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
