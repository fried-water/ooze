#include "pch.h"

#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
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
  for(const auto& [v, f] : repl.bindings) {
    ordered.push_back(v);
  }
  std::sort(ordered.begin(), ordered.end());

  for(const auto& v : ordered) {
    const auto& [f, type] = repl.bindings.at(v);
    output.push_back(fmt::format("  {}: {}{}", v, type_name_or_id(e, type), f.ready() ? "" : "*"));
  }

  return output;
}

std::vector<std::string> run(const Env& e, Repl& repl, const EvalCmd& eval) {
  return parse(read_text_file(eval.file))
    .map_error([&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<script>", error)}; })
    .and_then([&](AST ast) {
      return create_graphs(e, ast, [&](const std::string& name) { return *load(e, read_binary_file(name), name); });
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
  std::map<std::string, std::string> functions;

  for(const auto& [name, graph] : repl.graphs) {
    functions.emplace(name, function_string(e, name, graph));
  }

  for(const auto& [name, f] : e.functions) {
    functions.emplace(name, function_string(e, name, f));
  }

  std::vector<std::string> output{fmt::format("{} function(s)", functions.size())};
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

  for(const auto& [id, _] : e.to_string) {
    std::get<0>(types[type_name_or_id(e, id)]) = true;
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
  const auto var_it = repl.bindings.find(cmd.var);

  if(var_it == repl.bindings.end()) {
    return {fmt::format("Binding {} not found", cmd.var)};
  }

  auto& [future, type] = var_it->second;

  const auto serialize_it = e.serialize.find(type);

  if(serialize_it == e.serialize.end()) {
    return {fmt::format("{} ({}) does not support serialization", cmd.var, type_name_or_id(e, type))};
  }

  auto [bf, f2] = borrow(std::move(future));

  repl.bindings[cmd.var].first = std::move(f2);

  bf.then([&e, file = cmd.file](const Any& value) { write_binary_file(file, save(e, value)); });

  return {};
}

std::vector<std::string> run(const Env&, Repl& repl, const ReleaseCmd& cmd) {
  if(const auto it = repl.bindings.find(cmd.var); it != repl.bindings.end()) {
    repl.bindings.erase(it);
    return {};
  } else {
    return {fmt::format("Binding {} not found", cmd.var)};
  }
}

std::vector<std::string> run(const Env&, Repl& repl, const AwaitCmd& cmd) {
  std::vector<std::string> output;
  if(cmd.bindings.empty()) {
    for(auto& [name, pair] : repl.bindings) {
      pair.first = anyf::Future(repl.executor, std::move(pair.first).wait());
    }
  } else {
    for(const std::string& binding : cmd.bindings) {
      if(const auto it = repl.bindings.find(binding); it != repl.bindings.end()) {
        it->second.first = anyf::Future(repl.executor, std::move(it->second.first).wait());
      } else {
        output.push_back(fmt::format("Binding {} not found", binding));
      }
    }
  }
  return output;
}

Result<std::vector<std::pair<std::string, TypeProperties>>>
expr_inputs(const Env& e, const Map<std::string, std::pair<anyf::Future, TypeID>>& bindings, const ast::Expr& expr) {
  std::vector<std::pair<std::string, TypeProperties>> inputs;
  if(std::holds_alternative<std::string>(expr.v)) {
    const std::string var = std::get<std::string>(expr.v);
    const auto it = bindings.find(var);
    if(it != bindings.end()) {
      inputs.emplace_back(var, TypeProperties{it->second.second});
    }
  } else {
    inputs = inputs_of(e, expr);
  }

  std::vector<std::string> errors;
  for(const auto& [name, type] : inputs) {
    const auto it = bindings.find(name);

    if(it == bindings.end()) {
      errors.push_back(fmt::format("{} not found", name));
    } else {
      auto& [future, var_type] = it->second;

      if(var_type != type.id) {
        errors.push_back(
          fmt::format("{} expected {} but is {}", name, type_name_or_id(e, type.id), type_name_or_id(e, var_type)));
      }
    }
  }

  if(errors.empty()) {
    return inputs;
  } else {
    return tl::unexpected{std::move(errors)};
    ;
  }
}

Result<std::tuple<std::vector<anyf::Future>, std::vector<TypeProperties>>>
run_expr(const Env& e, Repl& repl, const ast::Expr& expr) {
  return expr_inputs(e, repl.bindings, expr)
    .and_then([&](auto inputs) {
      auto g = create_graph(
        e, expr, repl.graphs, inputs, [&](const std::string& name) { return *load(e, read_binary_file(name), name); });

      return merge(std::move(g), std::move(inputs));
    })
    .map([&](auto p) {
      auto [g, inputs] = std::move(p);

      std::vector<anyf::Future> value_inputs;
      std::vector<anyf::BorrowedFuture> borrowed_inputs;

      for(const auto& [name, type] : inputs) {
        auto& [future, var_type] = repl.bindings.at(name);

        if(type.value) {
          value_inputs.push_back(std::move(future));
          repl.bindings.erase(name);
        } else {
          auto [bf, next_future] = borrow(std::move(future));
          borrowed_inputs.push_back(std::move(bf));
          future = std::move(next_future);
        }
      }

      return std::tuple(anyf::execute_graph(g, repl.executor, std::move(value_inputs), std::move(borrowed_inputs)),
                        output_types(g));
    });
}

void bind_results(const Env& e,
                  Repl& repl,
                  const std::vector<TypeProperties>& types,
                  const std::vector<ast::Binding>& bindings,
                  std::vector<anyf::Future> results) {
  check(bindings.size() == results.size(), "TODO");
  for(size_t i = 0; i < results.size(); i++) {
    repl.bindings[bindings[i].name] = std::pair(std::move(results[i]), types[i].id);
  }
}

std::vector<std::string> wait_and_dump_results(const Env& e, std::vector<anyf::Future> results) {
  std::vector<std::string> output;

  for(auto& future : results) {
    Any any = std::move(future).wait();

    const auto it = e.to_string.find(any.type());

    output.push_back(it != e.to_string.end() ? it->second(any)
                                             : fmt::format("[Object of {}]", type_name_or_id(e, any.type())));
  }

  return output;
}

} // namespace

std::vector<std::string> step_repl(const Env& e, Repl& repl, std::string_view line) {
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
        return std::visit(
          Overloaded{[&](ast::Assignment assignment) {
                       return run_expr(e, repl, assignment.expr).map([&](auto tup) {
                         bind_results(e, repl, std::get<1>(tup), assignment.variables, std::move(std::get<0>(tup)));
                         return std::vector<std::string>{};
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
  fmt::print("Try :h for help. Use Ctrl^C to exit.\n");
  fmt::print("> ");

  while(std::getline(std::cin, line)) {
    for(const auto& line : step_repl(e, repl, line)) {
      fmt::print("{}\n", line);
    }

    fmt::print("> ");
  }
}

} // namespace ooze
