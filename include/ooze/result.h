#pragma once

#include "ooze/functional.h"

#include <knot/core.h>

#include <optional>
#include <tuple>
#include <variant>

namespace ooze {

template <typename T, typename E, typename... Ts>
class Result;

template <typename E, typename... Ts>
class Result<void, E, Ts...>;

template <typename E, typename T, typename... Ts>
constexpr Result<T, E, Ts...> success(T, Ts...);

template <typename E, typename... Ts>
constexpr Result<void, E, Ts...> success(std::tuple<>, Ts...);

template <typename E>
constexpr Result<void, E> success();

template <typename T, typename E, typename... Ts>
constexpr Result<T, E, Ts...> fail(E, Ts...);

template <typename T, typename E, typename... Ts>
constexpr Result<T, E, Ts...> fail(knot::Type<T>, E, Ts...);

template <typename T, typename E, typename... Ts>
constexpr knot::Type<T> success_type(knot::Type<Result<T, E, Ts...>>) {
  return {};
}

template <typename T, typename E, typename... Ts>
constexpr knot::Type<E> fail_type(knot::Type<Result<T, E, Ts...>>) {
  return {};
}

template <typename E>
struct Failure {
  E value;
  KNOT_ORDERED(Failure);
};

template <typename E>
Failure(E) -> Failure<E>;

template <typename T, typename E, typename... Ts>
class Result {
  std::variant<T, Failure<E>> _result;
  std::tuple<Ts...> _state;

public:
  template <bool b = std::is_default_constructible_v<std::tuple<Ts...>>, typename = std::enable_if_t<b>>
  constexpr Result(T t = {}) : _result(std::move(t)) {}

  constexpr Result(T t, Ts... ts) : _result(std::move(t)), _state(std::move(ts)...) {}
  constexpr Result(T t, std::tuple<Ts...> s) : _result(std::move(t)), _state(std::move(s)) {}

  constexpr Result(Failure<E> e, Ts... ts) : _result(std::move(e)), _state(std::move(ts)...) {}
  constexpr Result(Failure<E> e, std::tuple<Ts...> s) : _result(std::move(e)), _state(std::move(s)) {}

  template <typename F>
  auto and_then(F f) && {
    using R = decltype(applied(std::move(f))(std::move(*this).value_and_state()));
    return has_value() ? applied(std::move(f))(std::move(*this).value_and_state())
                       : R{Failure{std::move(error())}, std::move(state())};
  }

  template <typename F>
  auto map(F f_) && {
    auto f = applied(tuple_wrap(std::move(f_)));
    constexpr auto result = knot::as_typelist(knot::Type<decltype(std::move(f)(std::move(*this).value_and_state()))>{});

    if constexpr(result == knot::TypeList<Ts...>{}) {
      return has_value() ? Result<void, E, Ts...>{std::move(f)(std::move(*this).value_and_state())}
                         : Result<void, E, Ts...>(Failure{std::move(error())}, std::move(state()));
    } else if constexpr(tail(result) == knot::TypeList<Ts...>{}) {
      constexpr auto type = head(result);
      using R = Result<typename decltype(type)::type, E, Ts...>;
      return has_value() ? applied(Construct<R>{})(std::move(f)(std::move(*this).value_and_state()))
                         : R{Failure{std::move(error())}, std::move(state())};
    } else {
      static_assert(result == knot::TypeList<Ts...>{} || tail(result) == knot::TypeList<Ts...>{},
                    "Invalid Result::map() function return value");
    }
  }

  template <typename F>
  auto map_error(F f) && {
    using R = Result<T, decltype(std::move(f)(std::move(error()))), Ts...>;
    return has_value() ? R{std::move(value()), std::move(state())}
                       : R{Failure{std::move(f)(std::move(error()))}, std::move(state())};
  }

  template <typename F>
  Result<T, E, Ts...> or_else(F f) && {
    return has_value() ? std::move(*this) : applied(std::move(f))(std::move(*this).error_and_state());
  }

  template <typename... Ss>
  auto append_state(Ss&&... ss) && {
    return std::move(*this).map_state(append_fn(std::forward<Ss>(ss)...));
  }

  template <typename F>
  auto map_state(F f) && {
    return std::apply(
      [&](auto&&... ts) {
        return has_value() ? success<E>(std::move(value()), std::move(ts)...)
                           : fail<T>(std::move(error()), std::move(ts)...);
      },
      applied(tuple_wrap(std::move(f)))(std::move(state())));
  }

  constexpr T&& value() && { return std::move(std::get<0>(_result)); }
  constexpr T& value() & { return std::get<0>(_result); }
  constexpr const T& value() const& { return std::get<0>(_result); }

  constexpr T&& operator*() && { return std::move(std::get<0>(_result)); }
  constexpr T& operator*() & { return std::get<0>(_result); }
  constexpr const T& operator*() const& { return std::get<0>(_result); }

  constexpr E&& error() && { return std::move(std::get<1>(_result).value); }
  constexpr E& error() & { return std::get<1>(_result).value; }
  constexpr const E& error() const& { return std::get<1>(_result).value; }

  constexpr std::tuple<Ts...>&& state() && { return std::move(_state); }
  constexpr std::tuple<Ts...>& state() & { return _state; }
  constexpr const std::tuple<Ts...>& state() const& { return _state; }

  constexpr auto value_and_state() && { return flatten_tuple(std::move(value()), std::move(_state)); }

  constexpr std::tuple<E, Ts...>&& error_and_state() && { return flatten_tuple(std::move(error()), std::move(_state)); }

  constexpr bool has_value() const { return _result.index() == 0; }
  constexpr operator bool() const { return has_value(); }

  friend auto as_tie(const Result& r) { return std::tie(r._result, r._state); }
  friend auto as_tie(Result& r) { return std::tie(r._result, r._state); }
  friend auto as_tie(Result&& r) { return std::tie(std::move(r._result), std::move(r._state)); }

  KNOT_ORDERED(Result);
};

template <typename E, typename... Ts>
class Result<void, E, Ts...> {
  std::optional<E> _error;
  std::tuple<Ts...> _state;

public:
  template <bool b = std::is_default_constructible_v<std::tuple<Ts...>>, typename = std::enable_if_t<b>>
  constexpr Result() {}

  constexpr Result(Ts... ts) : _state(std::move(ts)...) {}
  constexpr Result(std::tuple<Ts...> s) : _state(std::move(s)) {}

  constexpr Result(Failure<E> e, Ts... ts) : _error(std::move(e.value)), _state(std::move(ts)...) {}
  constexpr Result(Failure<E> e, std::tuple<Ts...> s) : _error(std::move(e.value)), _state(std::move(s)) {}

  template <typename F>
  auto and_then(F f) && {
    using R = decltype(applied(std::move(f))(std::move(*this).value_and_state()));
    return has_value() ? applied(std::move(f))(std::move(*this).value_and_state())
                       : R{Failure{std::move(error())}, std::move(state())};
  }

  template <typename F>
  auto map(F f_) && {
    auto f = applied(tuple_wrap(std::move(f_)));
    constexpr auto result = knot::as_typelist(knot::Type<decltype(std::move(f)(std::move(*this).value_and_state()))>{});

    if constexpr(result == knot::TypeList<Ts...>{}) {
      return has_value() ? Result<void, E, Ts...>{std::move(f)(std::move(*this).value_and_state())}
                         : Result<void, E, Ts...>(Failure{std::move(error())}, std::move(state()));
    } else if constexpr(tail(result) == knot::TypeList<Ts...>{}) {
      constexpr auto type = head(result);
      using R = Result<typename decltype(type)::type, E, Ts...>;
      return has_value() ? applied(Construct<R>{})(std::move(f)(std::move(*this).value_and_state()))
                         : R{Failure{std::move(error())}, std::move(state())};
    } else {
      static_assert(result == knot::TypeList<Ts...>{} || tail(result) == knot::TypeList<Ts...>{},
                    "Invalid Result::map() function return value");
    }
  }

  template <typename F>
  auto map_error(F f) && {
    using R = Result<void, decltype(std::move(f)(std::move(error()))), Ts...>;
    return has_value() ? R{std::move(state())} : R{Failure{std::move(f)(std::move(error()))}, std::move(state())};
  }

  template <typename F>
  Result<void, E> or_else(F f) && {
    return has_value() ? std::move(*this) : applied(std::move(f))(std::move(*this).error_and_state());
  }

  template <typename... Ss>
  auto append_state(Ss&&... ss) && {
    return std::move(*this).map_state(append_fn(std::forward<Ss>(ss)...));
  }

  template <typename F>
  auto map_state(F f) && {
    return std::apply(
      [&](auto&&... ts) {
        return has_value() ? success<E>(std::tuple(), std::move(ts)...)
                           : fail<void>(std::move(error()), std::move(ts)...);
      },
      applied(tuple_wrap(std::move(f)))(std::move(state())));
  }

  constexpr E&& error() && { return std::move(*_error); }
  constexpr E& error() & { return *_error; }
  constexpr const E& error() const& { return *_error; }

  constexpr std::tuple<Ts...>&& state() && { return std::move(_state); }
  constexpr std::tuple<Ts...>& state() & { return _state; }
  constexpr const std::tuple<Ts...>& state() const& { return _state; }

  constexpr std::tuple<Ts...>&& value_and_state() && { return std::move(_state); }
  constexpr std::tuple<E, Ts...>&& error_and_state() && { return flatten_tuple(std::move(error()), std::move(_state)); }

  constexpr bool has_value() const { return !_error.has_value(); }
  constexpr operator bool() const { return has_value(); }

  friend auto as_tie(const Result& r) { return std::tie(r._error, r._state); }
  friend auto as_tie(Result& r) { return std::tie(r._error, r._state); }
  friend auto as_tie(Result&& r) { return std::tie(std::move(r._error), std::move(r._state)); }

  KNOT_ORDERED(Result);
};

template <typename E, typename T, typename... Ts>
constexpr Result<T, E, Ts...> success(T t, Ts... ts) {
  return Result<T, E, Ts...>{std::move(t), std::move(ts)...};
}

template <typename E, typename... Ts>
constexpr Result<void, E, Ts...> success(std::tuple<>, Ts... ts) {
  return Result<void, E, Ts...>{std::tuple(std::move(ts)...)};
}

template <typename E>
constexpr Result<void, E> success() {
  return {};
}

template <typename T, typename E, typename... Ts>
constexpr Result<T, E, Ts...> fail(E e, Ts... ts) {
  return fail(knot::Type<T>{}, std::move(e), std::move(ts)...);
}

template <typename T, typename E, typename... Ts>
constexpr Result<T, E, Ts...> fail(knot::Type<T>, E e, Ts... ts) {
  return Result<T, E, Ts...>{Failure{std::move(e)}, std::tuple(std::move(ts)...)};
}

} // namespace ooze
