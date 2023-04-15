#pragma once

#include "ooze/indirect.h"

#include <knot/core.h>

#include <vector>

namespace ooze {

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
  KNOT_ORDERED(CompoundType);
};

template <typename T>
CompoundType<T> leaf_type(T t) {
  return {std::move(t)};
}

template <typename T>
CompoundType<T> floating_type() {
  return {Floating{}};
}

template <typename T>
CompoundType<T> borrow_type(CompoundType<T> t) {
  return {Borrow<T>{std::move(t)}};
}

template <typename T>
CompoundType<T> function_type(CompoundType<T> input, CompoundType<T> output) {
  return {FunctionType<T>{std::move(input), std::move(output)}};
}

template <typename T>
CompoundType<T> tuple_type(std::vector<CompoundType<T>> v) {
  return {std::move(v)};
}

template <typename T, typename... Children>
CompoundType<T> tuple_type(Children... children) {
  std::vector<CompoundType<T>> v;
  v.reserve(sizeof...(Children));
  (v.push_back(std::move(children)), ...);
  return {std::move(v)};
}

} // namespace ooze
