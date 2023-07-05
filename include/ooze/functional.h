#pragma once

#include "knot/core.h"

#include <tuple>
#include <variant>

namespace ooze {

struct Identity {
  template <typename T>
  decltype(auto) operator()(T&& t) const {
    return std::forward<T>(t);
  }
};

template <typename T>
struct Construct {
  template <typename... Ts>
  T operator()(Ts&&... args) const {
    return T{std::forward<Ts>(args)...};
  };
};

template <typename F>
auto tuple_wrap(F f) {
  return [f = std::move(f)](auto&&... ts) {
    constexpr auto result = knot::invoke_result(knot::Type<F>{}, knot::TypeList<decltype(ts)...>{});
    if constexpr(knot::is_tuple_like(result)) {
      return f(std::forward<decltype(ts)>(ts)...);
    } else if constexpr(result == knot::Type<void>{}) {
      f(std::forward<decltype(ts)>(ts)...);
      return std::tuple();
    } else {
      return std::tuple(f(std::forward<decltype(ts)>(ts)...));
    }
  };
}

template <typename F>
auto applied(F f) {
  return [f = std::move(f)](auto&& t) { return std::apply(f, std::forward<decltype(t)>(t)); };
}

template <typename F>
auto visited(F f) {
  return [f = std::move(f)](auto&& t) { return std::visit(f, std::forward<decltype(t)>(t)); };
}

} // namespace ooze
