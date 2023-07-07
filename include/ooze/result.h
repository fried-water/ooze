#pragma once

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
    return std::apply(
      [&](auto&&... ts) {
        constexpr auto result = knot::invoke_result(knot::Type<F>{}, knot::TypeList<T, Ts...>{});

        return has_value() ? f(std::move(value()), std::move(ts)...)
                           : fail(success_type(result), std::move(error()), std::move(ts)...);
      },
      std::move(_state));
  }

  template <typename F>
  auto map(F f_) && {
    if constexpr(sizeof...(Ts) == 0) {
      constexpr auto result = knot::invoke_result(knot::Type<F>{}, knot::TypeList<T>{});

      if constexpr(result == knot::Type<void>{}) {
        return has_value() ? success<E>(tuple_wrap(std::move(f_))(std::move(value())))
                           : fail(result, std::move(error()));
      } else {
        return has_value() ? success<E>(f_(std::move(value()))) : fail(result, std::move(error()));
      }
    } else {
      auto f = tuple_wrap(std::move(f_));
      return std::apply(
        [&](auto&&... ts) {
          constexpr auto result =
            knot::as_typelist(knot::invoke_result(knot::Type<decltype(f)>{}, knot::TypeList<T, Ts...>{}));

          if constexpr(result == knot::TypeList<Ts...>{}) {
            return has_value() ? std::apply([](auto&&... ts) { return success<E>(std::tuple<>{}, std::move(ts)...); },
                                            f(std::move(value()), std::move(ts)...))
                               : fail<void>(std::move(error()), std::move(ts)...);
          } else if constexpr(tail(result) == knot::TypeList<Ts...>{}) {
            return has_value()
                     ? std::apply([](auto&& t, auto&&... ts) { return success<E>(std::move(t), std::move(ts)...); },
                                  f(std::move(value()), std::move(ts)...))
                     : fail(head(result), std::move(error()), std::move(ts)...);
          } else {
            static_assert(result == knot::TypeList<Ts...>{} || tail(result) == knot::TypeList<Ts...>{},
                          "Invalid Result::map() function return value");
          }
        },
        std::move(_state));
    }
  }

  template <typename F>
  auto map_error(F f) && {
    return std::apply(
      [&](auto&&... ts) {
        constexpr auto error_type = knot::invoke_result(knot::Type<F>{}, knot::TypeList<E>{});

        return has_value() ? success<knot::type_t<decltype(error_type)>>(std::move(value()), std::move(ts)...)
                           : fail(knot::Type<T>{}, f(std::move(error())), std::move(ts)...);
      },
      std::move(_state));
  }

  template <typename F>
  Result<T, E> or_else(F f) && {
    return has_value()
             ? std::move(*this)
             : std::apply([&](auto&&... ts) { return f(std::move(error()), std::move(ts)...); }, std::move(_state));
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
    return std::apply(
      [&](auto&&... ts) {
        return has_value() ? f(std::move(ts)...)
                           : fail(success_type(knot::invoke_result(knot::Type<F>{}, knot::TypeList<Ts...>{})),
                                  std::move(error()),
                                  std::move(ts)...);
      },
      std::move(_state));
  }

  template <typename F>
  auto map(F f_) && {
    auto f = tuple_wrap(std::move(f_));
    return std::apply(
      [&](auto&&... ts) {
        constexpr auto result =
          knot::as_typelist(knot::invoke_result(knot::Type<decltype(f)>{}, knot::TypeList<Ts...>{}));

        if constexpr(result == knot::TypeList<Ts...>{}) {
          return has_value() ? std::apply([](auto&&... ts) { return success<E>(std::tuple<>{}, std::move(ts)...); },
                                          f(std::move(ts)...))
                             : fail<void>(std::move(error()), std::move(ts)...);
        } else if constexpr(tail(result) == knot::TypeList<Ts...>{}) {
          return has_value()
                   ? std::apply([](auto&& t, auto&&... ts) { return success<E>(std::move(t), std::move(ts)...); },
                                f(std::move(ts)...))
                   : fail(head(result), std::move(error()), std::move(ts)...);
        } else {
          static_assert(result == knot::TypeList<Ts...>{} || tail(result) == knot::TypeList<Ts...>{},
                        "Invalid Result::map() function return value");
        }
      },
      std::move(_state));
  }

  template <typename F>
  auto map_error(F f) && {
    return std::apply(
      [&](auto&&... ts) {
        constexpr auto error_type = knot::invoke_result(knot::Type<F>{}, knot::TypeList<E>{});
        return has_value() ? success<knot::type_t<decltype(error_type)>>(std::tuple<>{}, std::move(ts)...)
                           : fail(knot::Type<void>{}, f(std::move(error())), std::move(ts)...);
      },
      std::move(_state));
  }

  template <typename F>
  Result<void, E> or_else(F f) && {
    return has_value()
             ? std::move(*this)
             : std::apply([&](auto&&... ts) { return f(std::move(error()), std::move(ts)...); }, std::move(_state));
  }

  constexpr E&& error() && { return std::move(*_error); }
  constexpr E& error() & { return *_error; }
  constexpr const E& error() const& { return *_error; }

  constexpr std::tuple<Ts...>&& state() && { return std::move(_state); }
  constexpr std::tuple<Ts...>& state() & { return _state; }
  constexpr const std::tuple<Ts...>& state() const& { return _state; }

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
