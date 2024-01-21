#pragma once

#include "ooze/any.h"
#include "ooze/function_graph.h"
#include "ooze/inst.h"
#include "ooze/traits.h"

#include <functional>
#include <utility>
#include <vector>

namespace ooze {

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
  assert(inputs.size() == sizeof...(Ts));
  assert((type_id(decay(knot::Type<Ts>{})) == inputs[Is]->type()) && ...);

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

using AnyFunction = std::function<std::vector<Any>(Span<Any*>)>;

enum class InstOp : u8 { Value, Fn, Graph, Functional, If, Select, Converge, Curry, Placeholder };

struct AnyFunctionInst {
  AnyFunction fn;
  std::vector<bool> input_borrows;
  int output_count;
};
struct FunctionalInst {
  i32 output_count;
};
struct IfInst {
  Inst if_inst;
  Inst else_inst;
  i32 output_count;
};
struct SelectInst {};
struct ConvergeInst {};

struct Program {
  std::vector<InstOp> inst;
  std::vector<i32> inst_data;

  std::vector<Any> values;
  std::vector<AnyFunctionInst> fns;
  std::vector<FunctionGraph> graphs;
  std::vector<IfInst> ifs;

  // TODO handle curry with Inst instead of Any?
  std::vector<std::pair<Inst, Slice>> currys;

  Inst add(Any);
  Inst add(FunctionGraph);
  Inst add(FunctionalInst);
  Inst add(IfInst);
  Inst add(SelectInst);
  Inst add(ConvergeInst);
  Inst curry(Inst, Span<Any>);

  Inst placeholder() {
    inst.push_back(InstOp::Placeholder);
    inst_data.push_back(-1);
    return Inst{i32(inst.size() - 1)};
  }

  void set(Inst, FunctionGraph);
  void set(Inst, Inst, Span<Any>);

  template <typename F>
  Inst add(F&& f) {
    constexpr auto f_type = decay(knot::Type<F>{});
    constexpr auto fn_ret = return_types(f_type);
    constexpr auto fn_args = args(f_type);

    constexpr bool legal_return = none(fn_ret, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
                                  all(fn_ret, [](auto t) { return knot::is_decayed(t) || is_rref(t); });
    constexpr bool legal_args =
      none(fn_args, [](auto t) { return knot::is_raw_pointer(decay(t)); }) &&
      all(fn_args, [](auto t) { return knot::is_decayed(t) || is_const_ref(t) || is_rref(t); });

    if constexpr(legal_return && legal_args && is_const_function(f_type)) {
      std::vector<bool> input_borrows;
      input_borrows.reserve(size(fn_args));
      visit(fn_args, [&](auto type) { input_borrows.push_back(is_const_ref(type)); });

      inst.push_back(InstOp::Fn);
      inst_data.push_back(int(fns.size()));
      fns.push_back(AnyFunctionInst{[f = std::move(f), fn_args](Span<Any*> inputs) {
                                      return details::call_with_anys(fn_args, f, inputs, knot::idx_seq(fn_args));
                                    },
                                    std::move(input_borrows),
                                    int(size(fn_ret))});

      return Inst{i32(inst.size() - 1)};
    } else {
      static_assert(is_const_function(f_type), "No mutable lambdas and non-const operator().");
      static_assert(legal_return,
                    "Function return type must be void, a value or tuple of "
                    "values (no refs or pointers).");
      static_assert(legal_args,
                    "Function arguments must either be values or "
                    "const refs (no non-const refs or pointers).");
      return Inst::Invalid();
    }
  }
};

} // namespace ooze
