#pragma once

#include "ooze/iter.h"

#include <knot/core.h>

#include <ostream>

namespace ooze {

template <typename Space, typename T = int, T INVALID = T(-1)>
class StrongID {
  static_assert(std::is_integral_v<T>);

  T _value = INVALID;

public:
  using underlying_type = T;

  constexpr static StrongID Invalid() { return StrongID(); }

  constexpr StrongID() = default;
  constexpr explicit StrongID(T t) : _value(t) {}

  constexpr bool is_valid() const { return *this != StrongID(); }

  constexpr T get() const { return _value; }

  StrongID& operator++() {
    ++_value;
    return *this;
  }

  StrongID operator++(int) {
    StrongID tmp = *this;
    ++_value;
    return tmp;
  }

  KNOT_ORDERED(StrongID);
  friend auto as_tie(const StrongID& s) { return std::tie(s._value); }

  friend std::ostream& operator<<(std::ostream& os, StrongID id) { return os << id.get(); }
};

template <typename Space, typename T, T Invalid>
T as_integral(StrongID<Space, T, Invalid> id) {
  return id.get();
}

template <typename T>
T as_integral(T t) {
  return t;
}

template <typename Space, typename T = int, T Invalid = T(-1)>
class StrongIDIter
    : public ForwardIter<StrongIDIter<Space, T, Invalid>, StrongID<Space, T, Invalid>, StrongID<Space, T, Invalid>> {
  StrongID<Space, T, Invalid> _id;

public:
  StrongIDIter() = default;
  StrongIDIter(StrongID<Space, T, Invalid> id) : _id{id} {}

  auto deref() const { return _id; }
  bool eq(const StrongIDIter& rhs) const { return _id == rhs._id; }
  void increment() { ++_id; }
};

template <typename Space, typename T = int, T Invalid = T(-1)>
auto id_range(StrongID<Space, T, Invalid> max) {
  using type = typename StrongID<Space, T, Invalid>::underlying_type;
  return IterRange{StrongIDIter{StrongID<Space, T, Invalid>{type{0}}}, StrongIDIter{max}};
}

template <typename Space, typename T = int, T Invalid = T(-1)>
auto id_range(StrongID<Space, T, Invalid> min, StrongID<Space, T, Invalid> max) {
  using type = typename StrongID<Space, T, Invalid>::underlying_type;
  return IterRange{StrongIDIter{min}, StrongIDIter{max}};
}

template <typename T = int>
class IntIter : public ForwardIter<IntIter<T>, T, T> {
  T _id;

public:
  IntIter() = default;
  IntIter(T id) : _id{id} {}

  auto deref() const { return _id; }
  bool eq(const IntIter& rhs) const { return _id == rhs._id; }
  void increment() { ++_id; }
};

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
auto id_range(T max) {
  return IterRange{IntIter{T(0)}, IntIter{max}};
}

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
auto id_range(T min, T max) {
  return IterRange{IntIter{min}, IntIter{max}};
}

} // namespace ooze

template <typename Space, typename T, T Invalid>
struct std::hash<ooze::StrongID<Space, T, Invalid>> {
  size_t operator()(ooze::StrongID<Space, T, Invalid> t) const noexcept { return std::hash<T>{}(t.get()); }
};
