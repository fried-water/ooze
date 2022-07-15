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
  std::unordered_map<TypeID, std::function<std::string(const Any&)>> to_string;
  std::unordered_map<TypeID, std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)>> serialize;
  std::unordered_map<TypeID, std::function<std::optional<Any>(Span<std::byte>)>> deserialize;
  std::unordered_map<std::string, AnyFunction> functions;

  template <typename T>
  void add_name(const std::string& name) {
    type_ids.emplace(name, anyf::type_id(knot::Type<T>{}));
    type_names.emplace(anyf::type_id(knot::Type<T>{}), name);
  }
};

inline std::string type_name_or_id(const Env& e, TypeID type) {
  const auto it = e.type_names.find(type);
  return it != e.type_names.end() ? it->second : fmt::format("type 0x{}", type);
}

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(knot::TypeList<Ts...>) {
  return [](Ts... ts) { return T{std::move(ts)...}; };
}

} // namespace details

template <typename T>
void add_tieable_type(Env& e, const std::string& name) {
  const TypeID type = anyf::type_id(knot::Type<T>{});

  e.add_name<T>(name);

  e.to_string.emplace(type, [](const Any& t) { return knot::debug(anyf::any_cast<T>(t)); });

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

    e.functions.emplace(create_name, details::generate_constructor<T>(as_typelist(tie_type(knot::Type<T>{}))));
  }
}

Env create_primative_env();

} // namespace ooze
