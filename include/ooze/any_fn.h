#pragma once

#include "ooze/any.h"
#include "ooze/span.h"
#include "ooze/traits.h"

#include <cassert>
#include <functional>

namespace ooze {
namespace details {

template <typename... Ts, typename F, std::size_t... Is>
void call_with_anys(knot::TypeList<Ts...>, F& f, Span<Any*> inputs, Any* outputs, std::index_sequence<Is...>) {
  assert(inputs.size() == sizeof...(Ts));
  assert(((type_id(decay(knot::Type<Ts>{})) == inputs[Is]->type()) && ...));

  if constexpr(knot::Type<void>{} == return_type(decay(knot::Type<F>({})))) {
    std::forward<F>(f)(std::move(*any_cast<std::decay_t<Ts>>(inputs[Is]))...);
  } else {
    auto&& result = std::forward<F>(f)(std::move(*any_cast<std::decay_t<Ts>>(inputs[Is]))...);
    if constexpr(is_tuple(decay(knot::Type<decltype(result)>{}))) {
      return std::apply([=](auto&&... e) mutable { ((*outputs++ = Any(std::move(e))), ...); }, std::move(result));
    } else {
      *outputs = Any(std::move(result));
    }
  }
}

} // namespace details

using AnyFn = std::function<void(Span<Any*>, Any*)>;

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
    return [f = std::forward<F>(f), fn_args](Span<Any*> inputs, Any* results) {
      details::call_with_anys(fn_args, f, inputs, results, knot::idx_seq(fn_args));
    };
  } else {
    static_assert(is_const_function(fn_type), "No mutable lambdas and non-const operator().");
    static_assert(legal_args,
                  "Function arguments must either be values or "
                  "const refs (no non-const refs or pointers).");
    static_assert(legal_return,
                  "Function return type must be void, a value or tuple of "
                  "values (no refs or pointers).");
    return [](Span<Any*>, Any*) {};
  }
}

} // namespace ooze
