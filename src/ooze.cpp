#include "pch.h"

#include "graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "parser.h"
#include "parser_combinators.h"
#include "queries.h"
#include "repl.h"

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
using TakeOption = Option<struct TOpt>;
using ReturnOption = Option<struct ROpt>;
using ContainOption = Option<struct COpt>;

struct RunCommand {
  std::string script;
  std::string expr;
  std::string output_prefix;
  int verbosity = 0;
};

struct FunctionQueryCommand {
  std::string regex = ".*";
  std::vector<std::string> takes;
  std::vector<std::string> returns;
  std::vector<std::string> contains;
};

struct TypeQueryCommand {
  std::string regex = ".*";
};

auto o_opt_parser() { return pc::construct<OutputOption>(pc::seq(pc::constant("-o", "-o"), pc::any())); }
auto s_opt_parser() { return pc::construct<ScriptOption>(pc::seq(pc::constant("-s", "-s"), pc::any())); }
auto t_opt_parser() { return pc::construct<TakeOption>(pc::seq(pc::constant("-t", "-t"), pc::any())); }
auto r_opt_parser() { return pc::construct<ReturnOption>(pc::seq(pc::constant("-r", "-r"), pc::any())); }
auto c_opt_parser() { return pc::construct<ContainOption>(pc::seq(pc::constant("-c", "-c"), pc::any())); }

auto v_parser() {
  return pc::transform(pc::constant("-v", "-v"), []() { return Verbose{}; });
}

auto run_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("run", "run"), pc::n(pc::choose(v_parser(), s_opt_parser(), o_opt_parser(), pc::any()))),
    [](std::vector<std::variant<Verbose, ScriptOption, OutputOption, std::string>> args) {
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

auto function_query_cmd_parser() {
  return pc::transform(pc::seq(pc::constant("functions", "functions"),
                               pc::n(pc::choose(t_opt_parser(), r_opt_parser(), c_opt_parser(), pc::any()))),
                       [](std::vector<std::variant<TakeOption, ReturnOption, ContainOption, std::string>> args) {
                         FunctionQueryCommand cmd;

                         for(auto&& arg : args) {
                           std::visit(Overloaded{[&](TakeOption o) { cmd.takes.push_back(std::move(o.value)); },
                                                 [&](ReturnOption o) { cmd.returns.push_back(std::move(o.value)); },
                                                 [&](ContainOption o) { cmd.contains.push_back(std::move(o.value)); },
                                                 [&](std::string r) { cmd.regex = std::move(r); }},
                                      std::move(arg));
                         }

                         return cmd;
                       });
}

auto type_query_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("types", "types"), pc::maybe(pc::any())),
    [](std::optional<std::string> regex) { return regex ? TypeQueryCommand{std::move(*regex)} : TypeQueryCommand{}; });
}

auto cmd_parser() {
  return pc::maybe(pc::choose(run_cmd_parser(), type_query_cmd_parser(), function_query_cmd_parser()));
}

void run(const RunCommand& cmd, const Env& e) {
  run(e,
      read_text_file(cmd.script),
      cmd.expr,
      [&](const std::string& name) { return *load(e, read_binary_file(name), name); })
    .map([&](std::vector<Any> outputs) {
      if(cmd.output_prefix != "") {
        for(size_t i = 0; i < outputs.size(); i++) {
          write_binary_file(outputs.size() == 1 ? fmt::format("{}", cmd.output_prefix)
                                                : fmt::format("{}_{}", cmd.output_prefix, i),
                            save(e, outputs[i]));
        }
      }

      if(cmd.output_prefix == "" || cmd.verbosity != 0) {
        for(const Any& any : outputs) {
          if(const auto it = e.to_string.find(any.type()); it != e.to_string.end()) {
            fmt::print("{}\n", it->second(any));
          } else {
            fmt::print("[Object of {}]\n", type_name_or_id(e, any.type()));
          }
        }
      }
    })
    .or_else(dump_errors);
}

void run(const FunctionQueryCommand& cmd, const Env& e) {
  const std::regex re(cmd.regex);

  std::vector<std::string> functions;
  std::transform(
    e.functions.begin(), e.functions.end(), std::back_inserter(functions), [](const auto& p) { return p.first; });

  std::sort(functions.begin(), functions.end());

  for(const std::string& fn_name : functions) {
    const auto& f = e.functions.at(fn_name);

    const auto doesnt_have = [&](const std::vector<TypeProperties>& types, const std::string& type) {
      const auto it = e.type_ids.find(type);
      check(it != e.type_ids.end(), "type {} not found", type);
      return std::none_of(types.begin(), types.end(), [&](auto t) { return t.id == it->second; });
    };

    const auto doesnt_take = [&](const std::string& type) { return doesnt_have(f.input_types(), type); };
    const auto doesnt_return = [&](const std::string& type) { return doesnt_have(f.output_types(), type); };
    const auto doesnt_contain = [&](const std::string& type) { return doesnt_take(type) && doesnt_return(type); };

    if(!std::regex_match(fn_name, re)) continue;
    if(std::any_of(cmd.takes.begin(), cmd.takes.end(), doesnt_take)) continue;
    if(std::any_of(cmd.returns.begin(), cmd.returns.end(), doesnt_return)) continue;
    if(std::any_of(cmd.contains.begin(), cmd.contains.end(), doesnt_contain)) continue;

    fmt::print("{}\n", function_string(e, fn_name, f));
  }
}

void run(const TypeQueryCommand& cmd, const Env& e) {
  const std::regex re(cmd.regex);

  std::vector<std::string> types;
  std::transform(
    e.type_names.begin(), e.type_names.end(), std::back_inserter(types), [](const auto& p) { return p.second; });

  std::sort(types.begin(), types.end());

  for(const std::string& type : types) {
    if(std::regex_match(type, re)) {
      fmt::print("{}\n", type);
    }
  }
}

} // namespace

std::optional<Any> load(const Env& e, Span<std::byte> bytes, const std::string& name) {
  auto opt_type = knot::deserialize_partial<std::string>(bytes.begin(), bytes.end());

  check(opt_type.has_value(), "deserializing file {}", name);

  const auto id_it = e.type_ids.find(opt_type->first);
  check(id_it != e.type_ids.end(), "cannot find type {}", opt_type->first);

  const auto ser_it = e.deserialize.find(id_it->second);
  check(ser_it != e.deserialize.end(), "type {} is not deserializable", opt_type->first);

  return ser_it->second(Span<std::byte>(opt_type->second, bytes.end()));
}

std::vector<std::byte> save(const Env& e, const Any& v) {
  const auto name_it = e.type_names.find(v.type());
  check(name_it != e.type_names.end(), "cannot find type 0x{}", v.type());

  const auto ser_it = e.serialize.find(v.type());
  check(ser_it != e.serialize.end(), "type {} is not serializable", name_it->second);

  return ser_it->second(v, knot::serialize(name_it->second));
}

Result<std::vector<Any>>
run(const Env& e, std::string_view script, std::string_view expr, std::function<Any(const std::string&)> load) {
  return merge(parse_expr(expr).map_error([&](const ParseError& error) {
           return std::vector<std::string>{generate_error_msg("<cmdline expr>", error)};
         }),
               parse(script)
                 .map_error([&](const ParseError& error) {
                   return std::vector<std::string>{generate_error_msg("<script>", error)};
                 })
                 .and_then([&](AST ast) { return create_graphs(e, ast, load); }))
    .and_then([&](auto tup) { return create_graph(e, std::get<0>(tup), std::get<1>(tup), {}, load); })
    .map([&](FunctionGraph g) { return anyf::execute_graph(g, anyf::TaskExecutor{}, {}); });
}

int main(int argc, char* argv[], const Env& e) {

  std::vector<std::string> args;
  for(int i = 1; i < argc; i++) {
    args.push_back(argv[i]);
  }

  const auto cmd_result = pc::parse(cmd_parser(), Span<std::string>{args});

  if(!cmd_result) {
    const char* msg = "Usage:\n"
                      "  run [-s script] expr\n"
                      "  repl [-s script]\n"
                      "  types [regex]\n"
                      "  functions [-t takes] [-r returns] [-c contains] [regex]\n";

    fmt::print("{}", msg);
  } else if(*cmd_result) {
    std::visit([&](const auto& cmd) { run(cmd, e); }, **cmd_result);
  } else {
    run_repl(e);
  }

  return 0;
}

} // namespace ooze
