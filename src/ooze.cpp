#include "pch.h"

#include "graph_construction.h"
#include "ooze/core.h"
#include "parser.h"
#include "parser_combinators.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

#include <fstream>
#include <regex>
#include <sstream>

namespace ooze {

namespace {

template <typename Range, typename F>
std::string join(const Range& range, F f) {
  if(range.begin() == range.end()) {
    return "()";
  } else {
    std::string r = "(" + f(*range.begin());
    std::for_each(++range.begin(), range.end(), [&](const auto& ele) { r += ", " + f(ele); });
    return r + ")";
  }
}

void write_binary(const std::string& filename, Span<std::byte> bytes) {
  std::basic_ofstream<char> file(filename, std::ios::binary);
  file.write((const char*)bytes.begin(), bytes.size());
}

std::vector<std::byte> read_binary(const std::string& filename) {
  std::basic_ifstream<char> file(filename.c_str(), std::ios::binary);
  std::vector<std::byte> bytes;
  std::transform(std::istreambuf_iterator<char>(file), {}, std::back_inserter(bytes), [](char c) {
    return static_cast<std::byte>(c);
  });

  return bytes;
}

std::string load_file(const std::string& filename) {
  std::ifstream file(filename);

  std::stringstream sstr;
  sstr << file.rdbuf();
  return std::move(sstr).str();
}

std::string type_name_or_id(const Env& e, TypeID type) {
  return e.contains_type(type) ? e.type_name(type) : fmt::format("0x{}", type);
}

std::optional<Any> load(const Env& e, Span<std::byte> bytes, const std::string& name) {
  auto opt_type = knot::deserialize_partial<std::string>(bytes.begin(), bytes.end());

  check(opt_type.has_value(), "deserializing @{}", name);
  check(e.contains_type(opt_type->first), "cannot find type {}", opt_type->first);
  check(e.contains_serialize(e.type_id(opt_type->first)), "type {} is not serializable", opt_type->first);

  return e.deserialize(opt_type->first, Span<std::byte>(opt_type->second, bytes.end()));
}

std::vector<std::byte> save(const Env& e, const Any& v) {
  check(e.contains_type(v.type()), "cannot find type 0x{}", v.type());
  check(e.contains_serialize(v.type()), "type {} is not serializable", e.type_name(v.type()));

  return e.serialize(v, knot::serialize(e.type_name(v.type())));
}

std::string generate_error_msg(std::string_view src, const ParseError& error) {
  std::string highlight = "";
  for(int i = 0; i < error.pos; i++) highlight += " ";
  highlight += '^';
  for(int i = 0; i < static_cast<int>(error.error_token.size()) - 1; i++) highlight += "~";

  return fmt::format("{}:{}:{} {}\n  {}\n  {}", src, error.line, error.pos, error.msg, error.error_line, highlight);
}

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

auto cmd_parser() { return pc::choose(run_cmd_parser(), type_query_cmd_parser(), function_query_cmd_parser()); }

auto parse_script_to_graphs(const Env& e, std::string_view script, std::function<Any(const std::string&)> load) {
  return parse(script)
    .map_error([&](const ParseError& error) { return std::vector<std::string>{generate_error_msg("<script>", error)}; })
    .and_then([&](AST ast) { return create_graphs(e, ast, load); });
}

void run(const RunCommand& cmd, const Env& e) {
  run(e, load_file(cmd.script), cmd.expr, [&](const std::string& name) { return *load(e, read_binary(name), name); })
    .map([&](std::vector<Any> outputs) {
      if(cmd.output_prefix != "") {
        for(size_t i = 0; i < outputs.size(); i++) {
          write_binary(outputs.size() == 1 ? fmt::format("{}", cmd.output_prefix)
                                           : fmt::format("{}_{}", cmd.output_prefix, i),
                       save(e, outputs[i]));
        }
      }

      if(cmd.output_prefix == "" || cmd.verbosity != 0) {
        for(const Any& any : outputs) {
          if(e.contains_type(any.type()) && e.contains_to_string(any.type())) {
            fmt::print("{}\n", e.to_string(any));
          } else {
            fmt::print("[Object of type {}]\n", type_name_or_id(e, any.type()));
          }
        }
      }
    })
    .or_else(dump_errors);
}

void run(const FunctionQueryCommand& cmd, const Env& e) {
  const auto type_name = [&](auto t) { return type_name_or_id(e, t.type_id()); };

  const std::regex re(cmd.regex);

  for(const std::string& fn_name : e.functions()) {
    const AnyFunction& f = e.function(fn_name);

    const auto doesnt_have = [&](const std::vector<TypeProperties>& types, const std::string& type) {
      check(e.contains_type(type), "type {} not found", type);
      const TypeID type_id = e.type_properties(type).type_id();
      return std::none_of(types.begin(), types.end(), [&](auto t) { return t.type_id() == type_id; });
    };

    const auto doesnt_take = [&](const std::string& type) { return doesnt_have(f.input_types(), type); };
    const auto doesnt_return = [&](const std::string& type) { return doesnt_have(f.output_types(), type); };

    const auto doesnt_contain = [&](const std::string& type) { return doesnt_take(type) && doesnt_return(type); };

    if(!std::regex_match(fn_name, re)) continue;
    if(std::any_of(cmd.takes.begin(), cmd.takes.end(), doesnt_take)) continue;
    if(std::any_of(cmd.returns.begin(), cmd.returns.end(), doesnt_return)) continue;
    if(std::any_of(cmd.contains.begin(), cmd.contains.end(), doesnt_contain)) continue;

    fmt::print("{}{} -> {}\n",
               fn_name,
               join(f.input_types(), type_name),
               f.output_types().size() == 1 ? type_name(f.output_types().front()) : join(f.output_types(), type_name));
  }
}

void run(const TypeQueryCommand& cmd, const Env& e) {
  const std::regex re(cmd.regex);

  for(const std::string& t : e.types()) {
    if(std::regex_match(t, re)) {
      fmt::print("{}\n", t);
    }
  }
}

} // namespace

Result<std::vector<Any>>
run(const Env& e, std::string_view script, std::string_view expr, std::function<Any(const std::string&)> load) {
  return merge(parse_expr(expr).map_error([&](const ParseError& error) {
           return std::vector<std::string>{generate_error_msg("<cmdline expr>", error)};
         }),
               parse_script_to_graphs(e, script, load),
               [&](const auto& expr, const auto& graphs) { return create_graph(e, expr, graphs, load); })
    .map([&](FunctionGraph g) { return anyf::execute_graph(g, anyf::TaskExecutor{}, {}); });
}

int main(int argc, char* argv[], const Env& e) {

  std::vector<std::string> args;
  for(int i = 1; i < argc; i++) {
    args.push_back(argv[i]);
  }

  const auto opt_cmd = pc::parse(cmd_parser(), Span<std::string>{args});

  if(!opt_cmd) {
    const char* msg = "Usage:\n"
                      "  run [-s script] expr\n"
                      "  types [regex]\n"
                      "  functions [-t takes] [-r returns] [-c contains] [regex]\n";

    fmt::print("{}", msg);
  } else {
    std::visit([&](const auto& cmd) { run(cmd, e); }, *opt_cmd);
  }

  return 0;
}

} // namespace ooze
