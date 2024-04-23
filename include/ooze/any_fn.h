#pragma once

#include "ooze/any.h"
#include "ooze/traits.h"

#include <cassert>
#include <functional>
#include <span>

namespace ooze {
namespace details {

template <typename T, size_t I, bool Owned>
struct ArgOrd {};

template <typename... Ts, std::size_t... Is, bool... Os, typename F>
void call_with_anys(
  knot::TypeList<ArgOrd<Ts, Is, Os>...>, F& f, std::span<Any> inputs, std::span<const Any*> borrows, Any* outputs) {
  assert(inputs.size() + borrows.size() == sizeof...(Ts));

  const auto get_arg = [&]<typename T, size_t I, bool Owned>(ArgOrd<T, I, Owned>) -> T&& {
    if constexpr(Owned) {
      return std::move(any_cast<T>(inputs[I]));
    } else {
      return std::move(any_cast<T>(*const_cast<Any*>(borrows[I])));
    }
  };

  if constexpr(knot::Type<void>{} == return_type(decay(knot::Type<F>({})))) {
    f(get_arg(ArgOrd<Ts, Is, Os>{})...);
  } else {
    auto&& result = f(get_arg(ArgOrd<Ts, Is, Os>{})...);
    if constexpr(is_tuple(decay(knot::Type<decltype(result)>{}))) {
      std::apply([=](auto&&... e) mutable { ((*outputs++ = Any(std::move(e))), ...); }, std::move(result));
    } else {
      *outputs = Any(std::move(result));
    }
  }
}

template <size_t ValueIdx = 0, size_t BorrowIdx = 0, typename... Ts>
constexpr auto create_arg_ordering(knot::TypeList<Ts...> ts) {
  constexpr auto cons = []<typename U, typename... Us>(U, knot::TypeList<Us...>) { return knot::TypeList<U, Us...>{}; };

  if constexpr(sizeof...(Ts) == 0) {
    return knot::TypeList<>{};
  } else if constexpr(is_const_ref(head(ts))) {
    using T = knot::type_t<decltype(decay(head(ts)))>;
    return cons(ArgOrd<T, BorrowIdx, false>{}, create_arg_ordering<ValueIdx, BorrowIdx + 1>(tail(ts)));
  } else {
    using T = knot::type_t<decltype(decay(head(ts)))>;
    return cons(ArgOrd<T, ValueIdx, true>{}, create_arg_ordering<ValueIdx + 1, BorrowIdx>(tail(ts)));
  }
}

} // namespace details

using AnyFn = std::function<void(std::span<Any>, std::span<const Any*>, Any*)>;

template <typename F>
AnyFn create_any_fn(F&& f) {
  constexpr auto fn_type = decay(knot::Type<F>{});
  constexpr auto fn_ret = return_types(fn_type);
  constexpr auto fn_args = args(fn_type);

  constexpr bool legal_args = none(fn_args, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
                              all(fn_args, [](auto t) { return t == decay(t) || is_rref(t) || is_const_ref(t); });

  constexpr bool legal_return = none(fn_ret, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
                                all(fn_ret, [](auto t) { return t == decay(t) || is_rref(t); });

  if constexpr(legal_args && legal_return && is_const_function(fn_type)) {
    return [f = std::forward<F>(f), fn_args](std::span<Any> inputs, std::span<const Any*> borrows, Any* outputs) {
      details::call_with_anys(details::create_arg_ordering(fn_args), f, inputs, borrows, outputs);
    };
  } else {
    static_assert(is_const_function(fn_type), "No mutable lambdas and non-const operator().");
    static_assert(legal_args,
                  "Function arguments must either be values or "
                  "const refs (no non-const refs or pointers).");
    static_assert(legal_return,
                  "Function return type must be void, a value or tuple of "
                  "values (no refs or pointers).");
    return [](std::span<Any>, std::span<const Any*>, Any*) {};
  }
}

} // namespace ooze
