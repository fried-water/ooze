#pragma once

#include "ooze/indirect.h"
#include "ooze/slice.h"

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

} // namespace ooze
