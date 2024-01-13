#pragma once

#include "ooze/any.h"
#include "ooze/async_fn.h"
#include "ooze/span.h"
#include "ooze/traits.h"

#include <exception>
#include <functional>

namespace ooze {

struct BadInvocation final : std::exception {
  const char* what() const noexcept override { return "bad invocation"; }
};

using AnyFunction = std::function<std::vector<Any>(Span<Any*>)>;

template <typename F>
AsyncFn create_async_any_function(F&&);

namespace details {

template <typename T, typename... Elements>
std::vector<T> make_vector(Elements&&... elements) {
  std::vector<T> vec;
  vec.reserve(sizeof...(Elements));
  (vec.emplace_back(std::forward<Elements>(elements)), ...);
  return vec;
}

template <typename... Ts, typename F, std::size_t... Is>
std::vector<Any> call_with_anys(knot::TypeList<Ts...>, F& f, Span<Any*> inputs, std::index_sequence<Is...>) {
  if(inputs.size() != sizeof...(Ts) || ((type_id(decay(knot::Type<Ts>{})) != inputs[Is]->type()) || ...)) {
    throw BadInvocation();
  }

  if constexpr(knot::Type<void>{} == return_type(decay(knot::Type<F>({})))) {
    std::forward<F>(f)(std::move(*any_cast<std::decay_t<Ts>>(inputs[Is]))...);
    return {};
  } else {
    auto&& result = std::forward<F>(f)(std::move(*any_cast<std::decay_t<Ts>>(inputs[Is]))...);

    if constexpr(is_tuple(decay(knot::Type<decltype(result)>{}))) {
      return std::apply([](auto&&... e) { return make_vector<Any>(std::move(e)...); }, std::move(result));
    } else {
      return make_vector<Any>(std::move(result));
    }
  }
}

} // namespace details

AsyncFn create_async(AnyFunction, std::vector<bool> input_borrows, int output_count);

template <typename F>
AsyncFn create_async_function(F&& f) {
  constexpr auto f_type = decay(knot::Type<F>{});
  constexpr auto fn_ret = return_types(f_type);
  constexpr auto fn_args = args(f_type);

  constexpr bool legal_return = none(fn_ret, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
                                all(fn_ret, [](auto t) { return knot::is_decayed(t) || is_rref(t); });
  constexpr bool legal_args = none(fn_args, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
                              all(fn_args, [](auto t) { return knot::is_decayed(t) || is_const_ref(t) || is_rref(t); });

  if constexpr(legal_return && legal_args && is_const_function(f_type)) {
    std::vector<bool> input_borrows;
    input_borrows.reserve(size(fn_args));
    visit(fn_args, [&](auto type) { input_borrows.push_back(is_const_ref(type)); });

    return create_async(
      [f = std::move(f), fn_args](Span<Any*> inputs) {
        return details::call_with_anys(fn_args, f, inputs, knot::idx_seq(fn_args));
      },
      std::move(input_borrows),
      int(size(fn_ret)));
  } else {
    static_assert(is_const_function(f_type), "No mutable lambdas and non-const operator().");
    static_assert(legal_return,
                  "Function return type must be void, a value or tuple of "
                  "values (no refs or pointers).");
    static_assert(legal_args,
                  "Function arguments must either be values or "
                  "const refs (no non-const refs or pointers).");
    return [](auto, auto, auto) { return std::vector<Future>{}; };
  }
}

} // namespace ooze
