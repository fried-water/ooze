#pragma once

#include <anyf/any_function.h>
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
using anyf::Span;
using anyf::TypeID;
using anyf::TypeProperties;

struct Env {
  std::unordered_map<TypeID, std::string> type_names;
  std::unordered_map<std::string, TypeID> type_ids;
  std::unordered_map<TypeID, std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)>> serialize;
  std::unordered_map<TypeID, std::function<std::optional<Any>(Span<std::byte>)>> deserialize;
  std::unordered_map<std::string, std::vector<AnyFunction>> functions;

  template <typename F>
  void add_function(const std::string& name, F&& f) {
    functions[name].push_back(AnyFunction{std::forward<F>(f)});
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

  e.serialize.emplace(type, [](const Any& t, std::vector<std::byte> bytes) {
    knot::serialize(anyf::any_cast<T>(t), std::back_inserter(bytes));
    return bytes;
  });

  e.deserialize.emplace(type, [](Span<std::byte> bytes) {
    std::optional<T> opt = knot::deserialize<T>(bytes.begin(), bytes.end());
    return opt ? std::optional(Any(std::move(*opt))) : std::nullopt;
  });

  if constexpr(std::is_aggregate_v<T>) {
    std::string create_name = "create_";
    std::transform(name.begin(), name.end(), std::back_inserter(create_name), [](char c) { return std::tolower(c); });

    using knot::as_tie;

    e.add_function(create_name, details::generate_constructor<T>(as_typelist(tie_type(knot::Type<T>{}))));
  }
}

Env create_primative_env();

} // namespace ooze
