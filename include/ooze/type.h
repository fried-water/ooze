#pragma once

#include "ooze/graph.h"
#include "ooze/strong_id.h"
#include "ooze/traits.h"

#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ooze {

struct TypeID {
  uintptr_t id = {};

  static constexpr TypeID Invalid() { return TypeID{}; }

  KNOT_ORDERED(TypeID);
};

template <typename T>
constexpr TypeID type_id(knot::Type<T> t) {
  static_assert(t == decay(t));
  static_assert(std::is_move_constructible_v<T>);
  return {reinterpret_cast<uintptr_t>(&type_id<T>)};
}

} // namespace ooze

template <>
struct std::hash<ooze::TypeID> {
  size_t operator()(ooze::TypeID t) const noexcept { return std::hash<uintptr_t>{}(t.id); }
};

namespace ooze {

using Type = StrongID<struct TypeSpace>;

enum class TypeTag { Leaf, Floating, Borrow, Fn, Tuple };

constexpr auto names(knot::Type<TypeTag>) { return knot::Names("Type", {"Leaf", "Floating", "Borrow", "Fn", "Tuple"}); }

using TypeGraph = Graph<Type, TypeTag, TypeID>;

using TypeNames = std::vector<std::pair<std::string, TypeID>>;

inline void insert(TypeNames& names, std::string name, TypeID tid) {
  const auto it = std::lower_bound(names.begin(), names.end(), name, [&](const auto& p, std::string_view name) {
    return p.first < name;
  });
  names.insert(it, std::pair(std::move(name), tid));
}

inline std::string_view find_name(const TypeNames& names, TypeID tid) {
  const auto it = std::find_if(names.begin(), names.end(), [&](const auto& p) { return p.second == tid; });
  return it != names.end() ? std::string_view{it->first} : std::string_view{};
}

inline std::optional<TypeID> find_id(const TypeNames& names, std::string_view name) {
  const auto it = std::lower_bound(names.begin(), names.end(), name, [](const auto& p, std::string_view name) {
    return std::string_view{p.first} < name;
  });
  return it != names.end() && it->first == name ? std::optional(it->second) : std::nullopt;
}

struct NativeTypeInfo {
  TypeNames names;
  std::unordered_set<TypeID> copyable;
};

struct TypeCache {
  Type floating;
  Type borrow_floating;
  Type fn_floating;
  Type unit;
  Type boolean;
};

template <typename T>
Type add_type(TypeGraph& g, knot::Type<T> t) {
  const TypeID id = type_id(decay(t));
  const Type type = g.add_node(TypeTag::Leaf, id);

  return is_const_ref(t) ? g.add_node(std::array{type}, TypeTag::Borrow, TypeID{}) : type;
}

template <typename F>
Type add_fn(TypeGraph& g, knot::Type<F> f) {
  std::vector<Type> types;
  types.reserve(size(args(f)));
  visit(args(f), [&](auto t) { types.push_back(add_type(g, t)); });
  const Type args = g.add_node(types, TypeTag::Tuple, TypeID{});

  if constexpr(const auto fn_ret = return_types(f); size(fn_ret) == 1) {
    const Type result = add_type(g, head(fn_ret));
    return g.add_node(std::array{args, result}, TypeTag::Fn, TypeID{});
  } else {
    types.clear();
    types.reserve(size(fn_ret));
    visit(fn_ret, [&](auto t) { types.push_back(add_type(g, t)); });
    const Type result = g.add_node(types, TypeTag::Tuple, TypeID{});
    return g.add_node(std::array{args, result}, TypeTag::Fn, TypeID{});
  }
}

inline TypeCache create_type_cache(TypeGraph& g) {
  TypeCache tc;

  tc.floating = g.add_node(TypeTag::Floating, TypeID{});
  tc.borrow_floating = g.add_node(std::array{tc.floating}, TypeTag::Borrow, TypeID{});
  tc.fn_floating = g.add_node(std::array{tc.floating, tc.floating}, TypeTag::Fn, TypeID{});
  tc.unit = g.add_node(TypeTag::Tuple, TypeID{});
  tc.boolean = g.add_node(TypeTag::Leaf, type_id(knot::Type<bool>{}));

  return tc;
}

inline int size_of(const TypeGraph& g, const Type& t) {
  int s = 0;

  preorder(g, t, [&](Type t) {
    switch(g.get<TypeTag>(t)) {
    case TypeTag::Leaf:
    case TypeTag::Fn: s += 1; return false;
    case TypeTag::Floating: assert(false);
    case TypeTag::Borrow:
    case TypeTag::Tuple: break;
    }
    return true;
  });

  return s;
}

} // namespace ooze
