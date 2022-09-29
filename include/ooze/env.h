#pragma once

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

using EnvFunction = std::variant<AnyFunction, FunctionGraph>;

inline const std::vector<TypeProperties>& input_types(const EnvFunction& f) {
  return std::holds_alternative<AnyFunction>(f) ? std::get<AnyFunction>(f).input_types()
                                                : input_types(std::get<FunctionGraph>(f));
}

inline const std::vector<TypeID>& output_types(const EnvFunction& f) {
  return std::holds_alternative<AnyFunction>(f) ? std::get<AnyFunction>(f).output_types()
                                                : output_types(std::get<FunctionGraph>(f));
}

struct Env {
  std::unordered_map<TypeID, std::string> type_names;
  std::unordered_map<std::string, TypeID> type_ids;
  std::unordered_map<std::string, std::vector<EnvFunction>> functions;

  template <typename F>
  void add_function(const std::string& name, F&& f) {
    functions[name].emplace_back(AnyFunction{std::forward<F>(f)});
  }

  void add_graph(const std::string& name, FunctionGraph f) { functions[name].emplace_back(std::move(f)); }

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
