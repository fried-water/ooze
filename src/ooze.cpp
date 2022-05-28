#include "pch.h"

#include "graph_construction.h"
#include "ooze/core.h"
#include "parser.h"
#include "parser_combinators.h"

#include <executor/task_executor.h>
#include <graph_execution.h>

#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>

namespace ooze {

namespace {

template <typename T>
T&& identity(T&& t) {
  return std::forward<T>(t);
}

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

std::vector<Any> gather_args(const Env& e, const std::vector<Literal>& args) {
  std::vector<Any> inputs;
  inputs.reserve(args.size());

  std::transform(args.begin(), args.end(), std::back_inserter(inputs), [&](const Literal& l) {
    return std::visit(
      Overloaded{[&](const FileLiteral& f) { return *load(e, read_binary(f.name)); }, [&](auto v) { return Any{v}; }},
      l);
  });

  return inputs;
}

void dump_values(const Env& e, const std::string& output_prefix, const std::vector<Any>& values) {
  for(size_t i = 0; i < values.size(); i++) {
    write_binary(values.size() == 1 ? fmt::format("{}", output_prefix) : fmt::format("{}_{}", output_prefix, i),
                 save(e, values[i]));
  }
}

template <typename T>
struct Option {
  std::string value;
};

struct Verbose {};

using FunctionOption = Option<struct FOpt>;
using ScriptOption = Option<struct SOpt>;
using OutputOption = Option<struct OOpt>;
using TakeOption = Option<struct TOpt>;
using ReturnOption = Option<struct ROpt>;
using ContainOption = Option<struct COpt>;

struct DumpComand {
  std::string name;
};

struct ScriptCommand {
  std::string script;
  std::string function = "main";
  std::vector<Literal> args;
  std::string output_prefix = "result";
  int verbosity = 0;
};

struct FunctionCommand {
  std::string function;
  std::vector<Literal> args;
  std::string output_prefix = "result";
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

auto literal_parser() { return pc::transform_if(parse_literal); }

auto o_opt_parser() { return pc::construct<OutputOption>(pc::seq(pc::constant("-o"), pc::any())); }
auto s_opt_parser() { return pc::construct<ScriptOption>(pc::seq(pc::constant("-s"), pc::any())); }
auto f_opt_parser() { return pc::construct<FunctionOption>(pc::seq(pc::constant("-f"), pc::any())); }
auto t_opt_parser() { return pc::construct<TakeOption>(pc::seq(pc::constant("-t"), pc::any())); }
auto r_opt_parser() { return pc::construct<ReturnOption>(pc::seq(pc::constant("-r"), pc::any())); }
auto c_opt_parser() { return pc::construct<ContainOption>(pc::seq(pc::constant("-c"), pc::any())); }

auto v_parser() {
  return pc::transform(pc::constant("-v"), []() { return Verbose{}; });
}

auto fn_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("fn"), pc::any(), pc::n(choose(v_parser(), literal_parser(), o_opt_parser()))),
    [](std::string fn, std::vector<std::variant<Verbose, Literal, OutputOption>> args) {
      FunctionCommand cmd{std::move(fn)};

      for(auto&& arg : args) {
        std::visit(Overloaded{[&](Literal l) { cmd.args.push_back(std::move(l)); },
                              [&](OutputOption o) { cmd.output_prefix = o.value; },
                              [&](Verbose) { cmd.verbosity++; }},
                   std::move(arg));
      }

      return cmd;
    });
}

auto script_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("run"),
            pc::any(),
            pc::n(pc::choose(v_parser(), f_opt_parser(), s_opt_parser(), o_opt_parser(), literal_parser()))),
    [](std::string script,
       std::vector<std::variant<Verbose, FunctionOption, ScriptOption, OutputOption, Literal>> args) {
      ScriptCommand cmd{std::move(script)};

      for(auto&& arg : args) {
        std::visit(Overloaded{[&](FunctionOption o) { cmd.function = std::move(o.value); },
                              [&](ScriptOption o) { cmd.script = std::move(o.value); },
                              [&](OutputOption o) { cmd.output_prefix = o.value; },
                              [&](Literal l) { cmd.args.push_back(std::move(l)); },
                              [&](Verbose) { cmd.verbosity++; }},
                   std::move(arg));
      }

      return cmd;
    });
}

auto dump_cmd_parser() { return pc::construct<DumpComand>(pc::seq(pc::constant("dump"), pc::any())); }

auto function_query_cmd_parser() {
  return pc::transform(
    pc::seq(pc::constant("functions"), pc::n(pc::choose(t_opt_parser(), r_opt_parser(), c_opt_parser(), pc::any()))),
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
  return pc::transform(pc::seq(pc::constant("types"), pc::maybe(pc::any())), [](std::optional<std::string> regex) {
    return regex ? TypeQueryCommand{std::move(*regex)} : TypeQueryCommand{};
  });
}

auto cmd_parser() {
  return pc::choose(
    fn_cmd_parser(), script_cmd_parser(), dump_cmd_parser(), type_query_cmd_parser(), function_query_cmd_parser());
}

void run(const DumpComand& cmd, const Env& e) {
  const std::optional<Any> any = load(e, read_binary(cmd.name));

  check(any.has_value(), "reading file {}", cmd.name);

  const std::string& type_name = e.type_name(any->type());
  const TypeEntry& t = e.type(type_name);

  fmt::print("{} hash({})\n", type_name, t.hash(*any));
  fmt::print("{}\n", t.to_string(*any));
}

void run(const ScriptCommand& cmd, const Env& e) {
  std::vector<Any> inputs = gather_args(e, cmd.args);

  if(cmd.verbosity != 0) {
    int i = 0;
    for(const Any& any : inputs) {
      const std::string& type_name = e.type_name(any.type());
      fmt::print("Input {}: {} {}\n", i++, type_name, e.type(type_name).to_string(any));
    }
  }

  auto outputs = run(e, load_file(cmd.script), std::move(inputs), cmd.function);

  dump_values(e, cmd.output_prefix, outputs);

  if(cmd.verbosity != 0) {
    int i = 0;
    for(const Any& any : outputs) {
      const std::string& type_name = e.type_name(any.type());
      fmt::print("Output {}: {} {}\n", i++, type_name, e.type(type_name).to_string(any));
    }
  }
}

void run(const FunctionCommand& cmd, const Env& e) {
  dump_values(e, cmd.output_prefix, run(e, cmd.function, gather_args(e, cmd.args)));
}

void run(const FunctionQueryCommand& cmd, const Env& e) {
  const auto type_name = [&](auto t) { return e.type_name(t.type_id()); };

  const std::regex re(cmd.regex);

  for(const std::string& fn_name : e.functions()) {
    const AnyFunction& f = e.function(fn_name);

    const auto doesnt_have = [&](const std::vector<anyf::Type>& types, const std::string& type) {
      check(e.contains_type(type), "type {} not found", type);
      const TypeID type_id = e.type(type).type.type_id();
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

std::optional<Any> load(const Env& e, Span<std::byte> bytes) {
  auto opt_type = knot::deserialize_partial<std::string>(bytes.begin(), bytes.end());

  check(opt_type.has_value(), "deserializing");
  check(e.contains_type(opt_type->first), "cannot find type {}", opt_type->first);

  const TypeEntry& entry = e.type(opt_type->first);

  return entry.deserialize(Span<std::byte>(opt_type->second, bytes.end()));
}

std::vector<std::byte> save(const Env& e, const Any& v) {
  return e.type(v.type()).serialize(v, knot::serialize(e.type_name(v.type())));
}

std::vector<Any> run(const Env& e, const std::string& fn, std::vector<Any> args) {
  check(e.contains_function(fn), "No function named {}", fn);

  const auto& function = e.function(fn);
  const auto& expected_types = function.input_types();

  check(expected_types.size() == args.size(), "expected {} args, given {}\n", expected_types.size(), args.size());

  for(size_t i = 0; i < args.size(); i++) {
    check(expected_types[i].type_id() == args[i].type(),
          "expected {}, given {} for arg {}",
          e.type_name(expected_types[i].type_id()),
          e.type_name(args[i].type()),
          i);
  }

  return function(anyf::any_ptrs(args));
}

std::vector<Any> run(const Env& e, std::string_view script, std::vector<Any> inputs, const std::string& fn) {
  const std::optional<AST> ast = parse(script);

  check(ast.has_value(), "parsing failed");
  check(validate(*ast), "AST validation failed");

  const std::vector<std::string> type_errors = typecheck(*ast);

  if(!type_errors.empty()) {
    fmt::print("Error: type checking failed:\n");
    for(const std::string& error : type_errors) {
      fmt::print("  {}\n", error);
    }
    exit(1);
  }

  const Map<std::string, FunctionGraph> graphs = create_graphs(e, *ast);

  check(graphs.find(fn) != graphs.end(), "cannot find {}()", fn);

  return anyf::execute_graph(graphs.at(fn), anyf::TaskExecutor{}, std::move(inputs));
}

int main(int argc, char* argv[], const Env& e) {

  std::vector<std::string> args;
  for(int i = 1; i < argc; i++) {
    args.push_back(argv[i]);
  }

  const auto opt_cmd = pc::parse(cmd_parser(), Span<std::string>{args});

  if(!opt_cmd) {
    const char* msg = "Usage:\n"
                      "  dump file\n"
                      "  run <script> [-f fn] [-v] args...\n"
                      "  fn <fn> [-v] args...\n"
                      "  types [regex]\n"
                      "  functions [-t takes] [-r returns] [-c contains] [regex]\n";

    fmt::print("{}", msg);
  } else {
    std::visit([&](const auto& cmd) { run(cmd, e); }, *opt_cmd);
  }

  return 0;
}

} // namespace ooze
