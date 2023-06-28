#pragma once

#include <tuple>
#include <variant>

namespace ooze {

template <typename T>
struct Construct {
  template <typename... Ts>
  T operator()(Ts&&... args) const {
    return T{std::forward<Ts>(args)...};
  };
};

template <typename F>
auto applied(F f) {
  return [f = std::move(f)](auto&& t) { return std::apply(f, std::forward<decltype(t)>(t)); };
}

template <typename F>
auto visited(F f) {
  return [f = std::move(f)](auto&& t) { return std::visit(f, std::forward<decltype(t)>(t)); };
}

} // namespace ooze
