#pragma once

#include <knot/core.h>

namespace ooze {

template <typename Space, typename T = int, T Invalid = T(-1)>
class StrongID {
  static_assert(std::is_integral_v<T>);

  T _value = Invalid;

public:
  using underlying_type = T;

  constexpr StrongID() = default;
  constexpr explicit StrongID(T t) : _value(t) {}

  constexpr static StrongID invalid() { return StrongID(); }

  bool is_valid() const { return *this == StrongID(); }

  constexpr T get() const { return _value; }

  KNOT_COMPAREABLE(StrongID);
  friend auto as_tie(const StrongID& s) { return std::tie(s._value); }
};

template <typename Space, typename T, T Invalid>
T as_integral(StrongID<Space, T, Invalid> id) {
  return id.get();
}

template <typename T>
T as_integral(T t) {
  return t;
}

} // namespace ooze

template <typename Space, typename T, T Invalid>
struct std::hash<ooze::StrongID<Space, T, Invalid>> {
  size_t operator()(ooze::StrongID<Space, T, Invalid> t) const noexcept { return std::hash<T>{}(t.get()); }
};
