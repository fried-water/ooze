#pragma once

#include "ooze/indirect.h"
#include "ooze/slice.h"
#include "ooze/traits.h"

#include <knot/core.h>

#include <vector>

namespace ooze {

struct TypeID {
  uintptr_t id = {};
  KNOT_ORDERED(TypeID);
};

template <typename T>
constexpr TypeID type_id(Type<T> t) {
  static_assert(t == decay(t));
  static_assert(std::is_move_constructible_v<T>);

  return {reinterpret_cast<uintptr_t>(&type_id<T>) | uintptr_t(std::is_copy_constructible_v<T>)};
}

template <typename T>
constexpr TypeID type_id() {
  return type_id(Type<T>{});
}

constexpr bool is_copyable(TypeID t) { return t.id & 1; }

struct Floating {
  KNOT_ORDERED(Floating);
};

template <typename T>
struct CompoundType;

template <typename T>
struct Borrow {
  Indirect<CompoundType<T>> type;
  KNOT_ORDERED(Borrow);
};

template <typename T>
struct FunctionType {
  Indirect<CompoundType<T>> input;
  Indirect<CompoundType<T>> output;
  KNOT_ORDERED(FunctionType);
};

template <typename T>
struct CompoundType {
  std::variant<std::vector<CompoundType<T>>, FunctionType<T>, Floating, Borrow<T>, T> v;
  Slice ref;
  KNOT_ORDERED(CompoundType);
};

template <typename T>
CompoundType<T> leaf_type(T t, Slice ref = {}) {
  return {std::move(t), ref};
}

template <typename T>
CompoundType<T> floating_type(Slice ref = {}) {
  return {Floating{}, ref};
}

template <typename T>
CompoundType<T> borrow_type(CompoundType<T> t, Slice ref = {}) {
  return {Borrow<T>{std::move(t)}, ref};
}

template <typename T>
CompoundType<T> function_type(CompoundType<T> input, CompoundType<T> output, Slice ref = {}) {
  return {FunctionType<T>{std::move(input), std::move(output)}, ref};
}

template <typename T>
CompoundType<T> tuple_type(std::vector<CompoundType<T>> v, Slice ref = {}) {
  return {std::move(v), ref};
}

template <typename T>
CompoundType<TypeID> type_of(knot::Type<T> t) {
  return is_const_ref(t) ? borrow_type(leaf_type(type_id(decay(t)))) : leaf_type(type_id(decay(t)));
}

template <typename... Ts>
CompoundType<TypeID> type_of(knot::TypeList<Ts...> tl) {
  std::vector<CompoundType<TypeID>> types;
  types.reserve(size(tl));
  visit(tl, [&](auto t) { types.push_back(type_of(t)); });
  return tuple_type(std::move(types));
}

} // namespace ooze

template <>
struct std::hash<ooze::TypeID> {
  size_t operator()(ooze::TypeID t) const noexcept { return std::hash<uintptr_t>{}(t.id); }
};
