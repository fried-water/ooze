#pragma once

#include <knot/core.h>

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

template <size_t I>
struct Get {
  template <typename T>
  auto&& operator()(T&& t) const {
    return std::get<I>(std::forward<T>(t));
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

template <typename... Ts>
auto unwrap_1tuple(std::tuple<Ts...> tuple) {
  if constexpr(sizeof...(Ts) == 1) {
    return std::get<0>(std::move(tuple));
  } else {
    return tuple;
  }
}

template <typename F>
auto visited(F f) {
  return [f = std::move(f)](auto&& t, auto&&... ts) {
    return std::visit([&](auto&& v) { return f(std::forward<decltype(v)>(v), std::forward<decltype(ts)>(ts)...); },
                      std::forward<decltype(t)>(t));
  };
}

struct TupleWrap {
  template <typename T>
  auto operator()(T t) const {
    return std::tuple(std::move(t));
  }

  template <typename... Ts>
  auto operator()(std::tuple<Ts...> t) const {
    return t;
  }
};

template <typename... Ts>
auto flatten_tuple(Ts... ts) {
  return std::tuple_cat(TupleWrap{}(std::move(ts))...);
}

template <typename... Ts>
auto append_fn(Ts&&... ts) {
  return [t = std::forward_as_tuple(std::forward<Ts>(ts)...)](auto&&... ts) {
    return std::tuple_cat(std::tuple(std::forward<decltype(ts)>(ts)...), std::move(t));
  };
}

inline auto pop_fn() {
  return [](auto&&... ts, auto&&) { return std::tuple(std::forward<decltype(ts)>(ts)...); };
}

inline auto nullify() {
  return [](auto&&...) {};
}

} // namespace ooze
