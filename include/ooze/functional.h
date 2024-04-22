#pragma once

#include <knot/core.h>

#include <algorithm>
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
auto tuple_wrapped(F f) {
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
auto flatten_to_tuple(Ts... ts) {
  return std::tuple_cat(TupleWrap{}(std::move(ts))...);
}

template <typename F>
auto flattened(F f) {
  return [f = applied(std::move(f))](auto&&... ts) { return f(flatten_to_tuple(std::forward<decltype(ts)>(ts)...)); };
}

template <typename T>
auto flatten_tuple(T tuple) {
  return std::apply([](auto... ts) { return flatten_to_tuple(std::move(ts)...); }, std::move(tuple));
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

template <typename... Fs>
auto and_fn(Fs&&... fs) {
  return [... fs = std::forward<Fs>(fs)](const auto& ele) { return (fs(ele) && ...); };
}

template <typename Range>
auto contained_fn(const Range& rng) {
  return [&rng](const auto& ele) { return std::find(rng.begin(), rng.end(), ele) != rng.end(); };
}

} // namespace ooze
