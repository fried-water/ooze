#pragma once

#include "ooze/src_map.h"
#include "ooze/type.h"

#include <unordered_map>
#include <utility>

namespace ooze {

using TypeRef = StrongID<struct TypeSpace>;

enum class TypeTag { Leaf, Floating, Borrow, Fn, Tuple };

constexpr auto names(knot::Type<TypeTag>) { return knot::Names("Type", {"Leaf", "Floating", "Borrow", "Fn", "Tuple"}); }

using TypeGraph = Graph<TypeRef, TypeTag, SrcRef, TypeID>;

struct TypeCache {
  TypeRef floating;
  TypeRef borrow_floating;
  TypeRef fn_floating;
  TypeRef unit;
  TypeRef boolean;

  // leaf and borrowed type
  std::unordered_map<TypeID, std::pair<TypeRef, TypeRef>> native;
};

template <typename T>
TypeRef add_or_get_type(TypeGraph& g, std::unordered_map<TypeID, std::pair<TypeRef, TypeRef>>& types, knot::Type<T> t) {
  const TypeID id = type_id(decay(t));

  auto it = types.find(id);
  if(it == types.end()) {
    const TypeRef type = g.add_node(TypeTag::Leaf, SrcRef{}, id);
    const TypeRef borrow = g.add_node(std::array{type}, TypeTag::Borrow, SrcRef{}, TypeID{});
    it = types.emplace(id, std::pair(type, borrow)).first;
  }

  return is_const_ref(t) ? it->second.second : it->second.first;
}

template <typename F>
TypeRef add_fn(TypeGraph& g, std::unordered_map<TypeID, std::pair<TypeRef, TypeRef>>& existing_types, knot::Type<F> f) {
  std::vector<TypeRef> types;
  types.reserve(size(args(f)));
  visit(args(f), [&](auto t) { types.push_back(add_or_get_type(g, existing_types, t)); });
  const TypeRef args = g.add_node(types, TypeTag::Tuple, SrcRef{}, TypeID{});

  if constexpr(const auto fn_ret = return_types(f); size(fn_ret) == 1) {
    const TypeRef result = add_or_get_type(g, existing_types, head(fn_ret));
    return g.add_node(std::array{args, result}, TypeTag::Fn, SrcRef{}, TypeID{});
  } else {
    types.clear();
    types.reserve(size(fn_ret));
    visit(fn_ret, [&](auto t) { types.push_back(add_or_get_type(g, existing_types, t)); });
    const TypeRef result = g.add_node(types, TypeTag::Tuple, SrcRef{}, TypeID{});
    return g.add_node(std::array{args, result}, TypeTag::Fn, SrcRef{}, TypeID{});
  }
}

inline TypeCache create_type_cache(TypeGraph& g) {
  TypeCache tc;

  tc.floating = g.add_node(TypeTag::Floating, {}, TypeID{});
  tc.borrow_floating = g.add_node(std::array{tc.floating}, TypeTag::Borrow, {}, TypeID{});
  tc.fn_floating = g.add_node(std::array{tc.floating, tc.floating}, TypeTag::Fn, {}, TypeID{});
  tc.unit = g.add_node(TypeTag::Tuple, {}, TypeID{});
  tc.boolean = g.add_node(TypeTag::Leaf, {}, type_id(knot::Type<bool>{}));

  tc.native.emplace(type_id(knot::Type<bool>{}),
                    std::pair(tc.boolean, g.add_node(std::array{tc.boolean}, TypeTag::Borrow, {}, TypeID{})));

  return tc;
}

inline int size_of(const TypeGraph& g, const TypeRef& t) {
  int s = 0;

  preorder(g, t, [&](TypeRef t) {
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
