#pragma once

#include "ooze/functional.h"

#include <knot/core.h>

#include <optional>
#include <tuple>
#include <variant>

namespace ooze {

template <typename T, typename E, typename... Ts>
constexpr auto success(knot::Type<E>, T, Ts...);

template <typename T, typename E, typename... Ts>
constexpr auto fail(knot::Type<T>, E, Ts...);

template <typename E>
struct Failure {
  E value;
  KNOT_ORDERED(Failure);
};

template <typename E>
Failure(E) -> Failure<E>;

template <typename T, typename E, typename... Ts>
class ResultStorage {
  std::variant<T, Failure<E>> _result;
  std::tuple<Ts...> _state;

public:
  constexpr ResultStorage(T t, Ts... ts) : _result(std::move(t)), _state(std::move(ts)...) {}
  constexpr ResultStorage(Failure<E> e, Ts... ts) : _result(std::move(e)), _state(std::move(ts)...) {}

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

  constexpr std::tuple<T, Ts...> value_and_state() && {
    return std::tuple_cat(std::tuple<T>(std::move(value())), std::move(_state));
  }

  constexpr std::tuple<E, Ts...> error_and_state() && {
    return std::tuple_cat(std::tuple<E>(std::move(error())), std::move(_state));
  }

  constexpr bool has_value() const { return _result.index() == 0; }
  constexpr operator bool() const { return has_value(); }

  friend auto as_tie(const ResultStorage& r) { return std::tie(r._result, r._state); }
  friend auto as_tie(ResultStorage& r) { return std::tie(r._result, r._state); }
  friend auto as_tie(ResultStorage&& r) { return std::tie(std::move(r._result), std::move(r._state)); }

  KNOT_ORDERED(ResultStorage);
};

template <typename E, typename... Ts>
class ResultStorage<void, E, Ts...> {
  std::optional<E> _error;
  std::tuple<Ts...> _state;

public:
  constexpr ResultStorage(Ts... ts) : _state(std::move(ts)...) {}
  constexpr ResultStorage(Failure<E> e, Ts... ts) : _error(std::move(e.value)), _state(std::move(ts)...) {}

  constexpr std::tuple<> value() const { return std::tuple(); }

  constexpr E&& error() && { return std::move(*_error); }
  constexpr E& error() & { return *_error; }
  constexpr const E& error() const& { return *_error; }

  constexpr std::tuple<Ts...>&& state() && { return std::move(_state); }
  constexpr std::tuple<Ts...>& state() & { return _state; }
  constexpr const std::tuple<Ts...>& state() const& { return _state; }

  constexpr std::tuple<Ts...>&& value_and_state() && { return std::move(_state); }
  constexpr std::tuple<E, Ts...> error_and_state() && {
    return std::tuple_cat(std::tuple<E>(std::move(error())), std::move(_state));
  }

  constexpr bool has_value() const { return !_error.has_value(); }
  constexpr operator bool() const { return has_value(); }

  friend auto as_tie(const ResultStorage& r) { return std::tie(r._error, r._state); }
  friend auto as_tie(ResultStorage& r) { return std::tie(r._error, r._state); }
  friend auto as_tie(ResultStorage&& r) { return std::tie(std::move(r._error), std::move(r._state)); }

  KNOT_ORDERED(ResultStorage);
};

template <typename T, typename E, typename... Ts>
class Result : public ResultStorage<T, E, Ts...> {
  using Storage = ResultStorage<T, E, Ts...>;

public:
  using Storage::Storage;

  constexpr static auto success_type = knot::Type<T>{};
  constexpr static auto fail_type = knot::Type<E>{};

  template <typename T2>
  auto error_cast(knot::Type<T2>) {
    Storage&& r = std::move(*this);
    return applied([&](auto e, auto&&... s) {
      return Result<T2, E, Ts...>{Failure{std::move(e)}, std::move(s)...};
    })(std::move(r).error_and_state());
  }

  template <typename F>
  auto and_then(F f) && {
    Storage&& r = std::move(*this);
    return r.has_value() ? applied(std::move(f))(std::move(r).value_and_state())
                         : std::move(*this).error_cast(
                             decltype(applied(std::move(f))(std::move(r).value_and_state()))::success_type);
  }

  template <typename F>
  auto map(F f_) && {
    Storage&& r = std::move(*this);
    auto f = applied(tuple_wrapped(std::move(f_)));
    constexpr auto result = knot::as_typelist(knot::Type<decltype(std::move(f)(std::move(r).value_and_state()))>{});

    if constexpr(result == knot::TypeList<Ts...>{}) {
      return r.has_value() ? applied(Construct<Result<void, E, Ts...>>{})(std::move(f)(std::move(r).value_and_state()))
                           : std::move(*this).error_cast(knot::Type<void>{});
    } else if constexpr(tail(result) == knot::TypeList<Ts...>{}) {
      constexpr auto type = head(result);
      using R = Result<typename decltype(type)::type, E, Ts...>;
      return r.has_value() ? applied(Construct<R>{})(std::move(f)(std::move(r).value_and_state()))
                           : std::move(*this).error_cast(type);
    } else {
      static_assert(result == knot::TypeList<Ts...>{} || tail(result) == knot::TypeList<Ts...>{},
                    "Invalid Result::map() function return value");
    }
  }

  template <typename F>
  auto map_error(F f_) && {
    Storage&& r = std::move(*this);
    auto f = applied(tuple_wrapped(std::move(f_)));
    constexpr auto new_error_type =
      head(knot::as_typelist(knot::Type<decltype(std::move(f)(std::move(r).error_and_state()))>{}));

    return r.has_value() ? applied([&](auto&&... ts) {
      return success(new_error_type, std::move(r).value(), std::move(ts)...);
    })(std::move(r).state())
                         : applied([](auto&&... ts) { return fail(success_type, std::move(ts)...); })(
                             std::move(f)(std::move(r).error_and_state()));
  }

  template <typename F>
  Result or_else(F f) && {
    Storage&& r = std::move(*this);
    return r.has_value() ? std::move(*this) : applied(std::move(f))(std::move(r).error_and_state());
  }

  template <typename... Ss>
  auto append_state(Ss&&... ss) && {
    return std::move(*this).map_state(append_fn(std::forward<Ss>(ss)...));
  }

  template <typename F>
  auto map_state(F f) && {
    Storage&& r = std::move(*this);
    return std::apply(
      [&](auto&&... ts) {
        return r.has_value() ? success(fail_type, std::move(r).value(), std::move(ts)...)
                             : fail(success_type, std::move(r).error(), std::move(ts)...);
      },
      applied(tuple_wrapped(std::move(f)))(std::move(r).state()));
  }
};

template <typename T, typename E, typename... Ts>
constexpr auto success(knot::Type<E>, T t, Ts... ts) {
  if constexpr(std::is_same_v<std::tuple<>, T>) {
    return Result<void, E, Ts...>{std::move(ts)...};
  } else {
    return Result<T, E, Ts...>{std::move(t), std::move(ts)...};
  }
}

template <typename T, typename E, typename... Ts>
constexpr auto fail(knot::Type<T>, E e, Ts... ts) {
  if constexpr(std::is_same_v<std::tuple<>, T>) {
    return Result<void, E, Ts...>{Failure{std::move(e)}, std::move(ts)...};
  } else {
    return Result<T, E, Ts...>{Failure{std::move(e)}, std::move(ts)...};
  }
}

} // namespace ooze
