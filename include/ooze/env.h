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
using anyf::type_id;
using anyf::TypeID;
using anyf::TypeProperties;
using knot::Type;
using knot::TypeList;

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(TypeList<Ts...>) {
  return [](Ts... ts) { return T{std::move(ts)...}; };
}

} // namespace details

struct TypeEntry {
  TypeProperties type;
  TypeProperties borrowed_type;

  std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)> serialize;
  std::function<std::optional<Any>(const Span<std::byte>&)> deserialize;
  std::function<std::size_t(const Any&)> hash;
  std::function<std::string(const Any&)> to_string;
};

class Env {
  std::unordered_map<std::string, TypeEntry> _types;
  std::unordered_map<TypeID, std::string> _type_names;
  std::unordered_map<std::string, AnyFunction> _functions;

public:
  template <typename T>
  void add(const std::string& name) {
    static_assert(is_decayed(Type<T>{}), "Can only add decayed types");
    _types.emplace(name,
                   TypeEntry{TypeProperties(Type<T>{}),
                             TypeProperties(Type<const T&>{}),
                             [](const Any& t, std::vector<std::byte> bytes) {
                               knot::serialize(anyf::any_cast<T>(t), std::back_inserter(bytes));
                               return bytes;
                             },
                             [=](const Span<std::byte>& bytes) {
                               std::optional<T> opt = knot::deserialize<T>(bytes.begin(), bytes.end());
                               return opt ? std::optional(Any(std::move(*opt))) : std::nullopt;
                             },
                             [](const Any& t) { return knot::hash_value(anyf::any_cast<T>(t)); },
                             [](const Any& t) { return knot::debug(anyf::any_cast<T>(t)); }});

    _type_names.emplace(type_id(Type<T>{}), name);

    if constexpr(std::is_aggregate_v<T>) {
      std::string lower_name = name;
      std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(), [](char c) { return std::tolower(c); });

      using knot::as_tie;

      add("create_" + lower_name, details::generate_constructor<T>(as_typelist(tie_type(Type<T>{}))));
    }
  }

  template <typename F>
  void add(const std::string& name, F&& f) {
    AnyFunction anyf(std::forward<F>(f));
    _functions.emplace(name, AnyFunction(std::move(f)));
  }

  const TypeEntry& type(const std::string& name) const { return _types.at(name); }
  const TypeEntry& type(TypeID id) const { return type(_type_names.at(id)); }
  std::string type_name(TypeID id) const {
    const auto it = _type_names.find(id);
    return it != _type_names.end() ? it->second : fmt::format("0x{:x}", id);
  }
  const AnyFunction& function(const std::string& name) const { return _functions.at(name); }

  bool contains_function(const std::string& name) const { return _functions.find(name) != _functions.end(); }
  bool contains_type(const std::string& name) const { return _types.find(name) != _types.end(); }
  bool contains_type(TypeID id) const { return _type_names.find(id) != _type_names.end(); }

  std::vector<std::string> functions() const {
    std::vector<std::string> functions;

    std::transform(
      _functions.begin(), _functions.end(), std::back_inserter(functions), [](const auto& pair) { return pair.first; });

    std::sort(functions.begin(), functions.end());

    return functions;
  }

  std::vector<std::string> types() const {
    std::vector<std::string> types;

    std::transform(
      _type_names.begin(), _type_names.end(), std::back_inserter(types), [](const auto& pair) { return pair.second; });

    std::sort(types.begin(), types.end());

    return types;
  }
};

Env create_primative_env();

} // namespace ooze
