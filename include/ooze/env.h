#pragma once

#include "ooze/type.h"

#include <anyf/any_function.h>
#include <anyf/graph.h>
#include <anyf/traits.h>
#include <anyf/type.h>
#include <knot/core.h>

#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

namespace ooze {

using anyf::Any;
using anyf::AnyFunction;
using anyf::FunctionGraph;
using anyf::Span;
using anyf::TypeID;
using anyf::TypeProperties;

struct EnvFunction {
  CompoundType<TypeID> type;
  std::variant<AnyFunction, FunctionGraph> f;
};

struct Env {
  std::unordered_map<TypeID, std::string> type_names;
  std::unordered_map<std::string, TypeID> type_ids;
  std::unordered_map<std::string, std::vector<EnvFunction>> functions;

  template <typename F>
  void add_function(const std::string& name, F&& f) {
    AnyFunction anyf{std::forward<F>(f)};

    std::vector<CompoundType<TypeID>> input;
    input.reserve(anyf.input_types().size());
    std::transform(anyf.input_types().begin(),
                   anyf.input_types().end(),
                   std::back_inserter(input),
                   [](TypeProperties p) { return p.value ? leaf_type(p.id) : borrow_type(leaf_type(p.id)); });

    if(anyf.output_types().size() == 1) {
      functions[name].push_back(
        {function_type(tuple_type(std::move(input)), leaf_type(anyf.output_types()[0])), std::move(anyf)});
    } else {
      std::vector<CompoundType<TypeID>> output;
      output.reserve(anyf.output_types().size());
      std::transform(anyf.output_types().begin(), anyf.output_types().end(), std::back_inserter(output), [](TypeID t) {
        return leaf_type(t);
      });

      functions[name].push_back(
        {function_type(tuple_type(std::move(input)), tuple_type(std::move(output))), std::move(anyf)});
    }
  }

  void add_graph(const std::string& name, CompoundType<TypeID> t, FunctionGraph f) {
    functions[name].push_back({std::move(t), std::move(f)});
  }

  template <typename T>
  void add_type(const std::string& name) {
    type_ids.emplace(name, anyf::type_id<T>());
    type_names.emplace(anyf::type_id<T>(), name);

    if constexpr(std::is_copy_constructible_v<T>) {
      add_function("clone", [](const T& t) { return t; });
    }
  }
};

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(knot::TypeList<Ts...>) {
  return
    [](std::conditional_t<std::is_trivially_destructible_v<Ts>, const Ts&, Ts>... ts) { return T{std::move(ts)...}; };
}

} // namespace details

template <typename T>
void add_tieable_type(Env& e, const std::string& name) {
  const TypeID type = anyf::type_id<T>();

  e.add_type<T>(name);

  e.add_function("to_string", [](const T& t) { return knot::debug(t); });
  e.add_function("serialize", [](const T& t) { return knot::serialize(t); });
  e.add_function("deserialize",
                 [](const std::vector<std::byte>& bytes) { return *knot::deserialize<T>(bytes.begin(), bytes.end()); });

  if constexpr(std::is_aggregate_v<T>) {
    std::string create_name = "create_";
    std::transform(name.begin(), name.end(), std::back_inserter(create_name), [](char c) { return std::tolower(c); });

    using knot::as_tie;

    e.add_function(create_name, details::generate_constructor<T>(as_typelist(tie_type(knot::Type<T>{}))));
  }
}

Env create_primative_env();

} // namespace ooze
