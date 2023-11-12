#pragma once

#include "ooze/indirect.h"
#include "ooze/slice.h"
#include "ooze/traits.h"

#include <knot/core.h>

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

struct FloatingType {
  KNOT_ORDERED(FloatingType);
};

template <typename T>
struct Type;

template <typename T>
struct BorrowType {
  Indirect<Type<T>> type;
  KNOT_ORDERED(BorrowType);
};

template <typename T>
struct FunctionType {
  Indirect<Type<T>> input;
  Indirect<Type<T>> output;
  KNOT_ORDERED(FunctionType);
};

template <typename T>
struct Type {
  std::variant<std::vector<Type<T>>, FunctionType<T>, FloatingType, BorrowType<T>, T> v;
  Slice ref;
  KNOT_ORDERED(Type);
};

template <typename T>
Type<T> leaf_type(T t, Slice ref = {}) {
  return {std::move(t), ref};
}

template <typename T>
Type<T> floating_type(Slice ref = {}) {
  return {FloatingType{}, ref};
}

template <typename T>
Type<T> borrow_type(Type<T> t, Slice ref = {}) {
  return {BorrowType<T>{std::move(t)}, ref};
}

template <typename T>
Type<T> function_type(Type<T> input, Type<T> output, Slice ref = {}) {
  return {FunctionType<T>{std::move(input), std::move(output)}, ref};
}

template <typename T>
Type<T> tuple_type(std::vector<Type<T>> v, Slice ref = {}) {
  return {std::move(v), ref};
}

template <typename T>
Type<TypeID> type_of(knot::Type<T> t) {
  return is_const_ref(t) ? borrow_type(leaf_type(type_id(decay(t)))) : leaf_type(type_id(decay(t)));
}

template <typename... Ts>
Type<TypeID> type_of(knot::TypeList<Ts...> tl) {
  std::vector<Type<TypeID>> types;
  types.reserve(size(tl));
  visit(tl, [&](auto t) { types.push_back(type_of(t)); });
  return tuple_type(std::move(types));
}

} // namespace ooze

template <>
struct std::hash<ooze::TypeID> {
  size_t operator()(ooze::TypeID t) const noexcept { return std::hash<uintptr_t>{}(t.id); }
};
