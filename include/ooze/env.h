#pragma once

#include "ooze/any_function.h"
#include "ooze/ast.h"
#include "ooze/graph.h"
#include "ooze/traits.h"
#include "ooze/type.h"

#include <knot/core.h>

#include <algorithm>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ooze {

struct EnvFunction {
  FunctionType<TypeID> type;
  std::variant<AnyFunction, FunctionGraph, TypedFunction> f;
  std::vector<std::pair<FunctionType<TypeID>, FunctionGraph>> instatiations;
};

FunctionType<TypeID> type_of(const AnyFunction&);

struct Env {
  std::unordered_map<TypeID, std::string> type_names;
  std::unordered_map<std::string, TypeID> type_ids;
  std::unordered_map<std::string, std::vector<EnvFunction>> functions;
  std::unordered_set<TypeID> copy_types;

  template <typename F>
  void add_function(const std::string& name, F&& f) {
    AnyFunction anyf{std::forward<F>(f)};
    functions[name].push_back({type_of(anyf), std::move(anyf)});
  }

  void add_graph(const std::string& name, FunctionType<TypeID> t, FunctionGraph f) {
    functions[name].push_back({std::move(t), std::move(f)});
  }

  template <typename T>
  void add_type(const std::string& name, std::optional<bool> copy_override = {}) {
    const TypeID type = type_id<T>();

    type_ids.emplace(name, type);
    type_names.emplace(type, name);

    constexpr bool is_default_copy =
      std::is_copy_constructible_v<T> && std::is_trivially_destructible_v<T> && sizeof(T) <= 64;

    const bool is_copy_type =
      (copy_override.has_value() && *copy_override) || (!copy_override.has_value() && is_default_copy);

    if(is_copy_type) {
      assert(std::is_copy_constructible_v<T>);
      copy_types.insert(type);
    }

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
  const TypeID type = type_id<T>();

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
