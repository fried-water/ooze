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

class Env {
  std::unordered_map<TypeID, std::string> _type_names;
  std::unordered_map<std::string, std::pair<TypeProperties, TypeProperties>> _properties;
  std::unordered_map<TypeID, std::function<std::string(const Any&)>> _to_strings;

  std::unordered_map<TypeID, std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)>> _serialize;

  std::unordered_map<TypeID, std::function<std::optional<Any>(Span<std::byte>)>> _deserialize;

  std::unordered_map<std::string, AnyFunction> _functions;

public:
  template <typename T>
  void add(const std::string& name) {
    static_assert(is_decayed(knot::Type<T>{}), "Can only add decayed types");
    _properties.emplace(name, std::pair(TypeProperties(knot::Type<T>{}), TypeProperties(knot::Type<const T&>{})));
    _type_names.emplace(anyf::type_id(knot::Type<T>{}), name);
  }

  void add_to_string(const std::string& name, std::function<std::string(const Any&)> f) {
    assert(_properties.find(name) != _properties.end());
    _to_strings.emplace(type_id(name), std::move(f));
  }

  void add_serialize(const std::string& name,
                     std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)> ser,
                     std::function<std::optional<Any>(const Span<std::byte>&)> deser) {
    assert(_properties.find(name) != _properties.end());
    _serialize.emplace(type_id(name), std::move(ser));
    _deserialize.emplace(type_id(name), std::move(deser));
  }

  template <typename F>
  void add(const std::string& name, F&& f) {
    AnyFunction anyf(std::forward<F>(f));
    _functions.emplace(name, AnyFunction(std::move(f)));
  }

  const AnyFunction& function(const std::string& name) const { return _functions.at(name); }

  bool contains_function(const std::string& name) const { return _functions.find(name) != _functions.end(); }

  bool contains_type(const std::string& name) const { return _properties.find(name) != _properties.end(); }
  bool contains_type(TypeID id) const { return _type_names.find(id) != _type_names.end(); }
  bool contains_to_string(TypeID id) const { return _to_strings.find(id) != _to_strings.end(); }
  bool contains_serialize(TypeID id) const { return _serialize.find(id) != _serialize.end(); }

  TypeID type_id(const std::string& name) const { return _properties.at(name).first.type_id(); }
  TypeProperties type_properties(const std::string& name) const { return _properties.at(name).first; }
  TypeProperties type_borrowed_properties(const std::string& name) const { return _properties.at(name).second; }

  std::string type_name(TypeID id) const {
    const auto it = _type_names.find(id);
    assert(it != _type_names.end());
    return it->second;
  }

  std::string to_string(const Any& any) const {
    const auto it = _to_strings.find(any.type());
    assert(it != _to_strings.end());
    return it->second(any);
  }

  std::vector<std::byte> serialize(const Any& any, std::vector<std::byte> bytes) const {
    const auto it = _serialize.find(any.type());
    assert(it != _serialize.end());
    return it->second(any, std::move(bytes));
  }

  std::optional<Any> deserialize(const std::string& name, Span<std::byte> bytes) const {
    const auto prop_it = _properties.find(name);
    assert(prop_it != _properties.end());
    const auto it = _deserialize.find(prop_it->second.first.type_id());
    assert(it != _deserialize.end());
    return it->second(bytes);
  }

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

inline std::string type_name_or_id(const Env& e, TypeID type) {
  return e.contains_type(type) ? e.type_name(type) : fmt::format("0x{}", type);
}

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(knot::TypeList<Ts...>) {
  return [](Ts... ts) { return T{std::move(ts)...}; };
}

} // namespace details

template <typename T>
void add_tieable_type(Env& e, const std::string& name) {
  e.add<T>(name);
  e.add_to_string(name, [](const Any& t) { return knot::debug(anyf::any_cast<T>(t)); });
  e.add_serialize(
    name,
    [](const Any& t, std::vector<std::byte> bytes) {
      knot::serialize(anyf::any_cast<T>(t), std::back_inserter(bytes));
      return bytes;
    },
    [](Span<std::byte> bytes) {
      std::optional<T> opt = knot::deserialize<T>(bytes.begin(), bytes.end());
      return opt ? std::optional(Any(std::move(*opt))) : std::nullopt;
    });

  if constexpr(std::is_aggregate_v<T>) {
    std::string lower_name = name;
    std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(), [](char c) { return std::tolower(c); });

    using knot::as_tie;

    e.add("create_" + lower_name, details::generate_constructor<T>(as_typelist(tie_type(knot::Type<T>{}))));
  }
}

Env create_primative_env();

} // namespace ooze
