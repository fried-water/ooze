#pragma once

#include <any_function.h>
#include <traits.h>

namespace ooze {

using anyf::Any;
using anyf::any_cast;
using anyf::AnyFunction;
using anyf::type_id;
using anyf::TypeID;

using anyf::TL;
using anyf::Ty;

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(TL<Ts...>) {
  return [](Ts... ts) { return T{std::move(ts)...}; };
}

} // namespace details

struct TypeEntry {
  anyf::Type type;
  anyf::Type borrowed_type;

  std::function<std::vector<std::byte>(const Any&, std::vector<std::byte>)> serialize;
  std::function<Any(const Span<std::byte>&)> deserialize;
  std::function<std::size_t(const Any&)> hash;
  std::function<std::string(const Any&)> to_string;
};

class Env {
  Map<std::string, TypeEntry> _types;
  Map<TypeID, std::string> _type_names;
  Map<std::string, AnyFunction> _functions;

public:
  template <typename T>
  void add(const std::string& name) {
    anyf::check(anyf::is_decayed(Ty<T>{}), "Can only add decayed types");
    _types.emplace(name,
                   TypeEntry{anyf::Type(anyf::Ty<T>{}),
                             anyf::Type(anyf::Ty<const T&>{}),
                             [](const Any& t, std::vector<std::byte> bytes) {
                               knot::serialize(any_cast<T>(t), std::back_inserter(bytes));
                               return bytes;
                             },
                             [=](const Span<std::byte>& bytes) {
                               std::optional<T> opt = knot::deserialize<T>(bytes.begin(), bytes.end());
                               check(opt.has_value(), "Error: deserializing: {}\n", name);
                               return Any(std::move(*opt));
                             },
                             [](const Any& t) { return knot::hash_value(any_cast<T>(t)); },
                             [](const Any& t) { return knot::debug(any_cast<T>(t)); }});

    _type_names.emplace(type_id<T>(), name);

    if constexpr(std::is_aggregate_v<T>) {
      std::string lower_name = name;
      std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(), [](char c) { return std::tolower(c); });

      using knot::as_tie;

      const auto types = anyf::decay(as_tl(Ty<decltype(as_tie(std::declval<T>()))>{}));

      add(fmt::format("create_{}", lower_name), details::generate_constructor<T>(types));
    }
  }

  template <typename F>
  void add(const std::string& name, F&& f) {
    AnyFunction anyf(std::forward<F>(f));
    _functions.emplace(name, AnyFunction(std::move(f)));
  }

  const TypeEntry& type(const std::string& name) const { return _types.at(name); }
  const TypeEntry& type(TypeID id) const { return type(_type_names.at(id)); }
  const std::string& type_name(TypeID id) const { return _type_names.at(id); }
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
