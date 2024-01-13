#pragma once

#include "algorithm.h"
#include "ooze/functional.h"
#include "ooze/primitives.h"
#include "ooze/result.h"
#include "ooze/slice.h"
#include "ooze/span.h"

#include <fmt/core.h>
#include <knot/core.h>

#include <cassert>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace ooze {

template <typename Key, typename Value>
using Map = std::unordered_map<Key, Value, knot::Hash>;

template <typename T>
using Set = std::unordered_set<T, knot::Hash>;

template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename T, typename... Ts>
using StringResult = Result<T, std::vector<std::string>, Ts...>;

template <typename... Ts>
auto make_vector(Ts&&... ts) {
  std::vector<std::common_type_t<std::decay_t<Ts>...>> v;
  v.reserve(sizeof...(ts));
  (v.emplace_back(std::forward<Ts>(ts)), ...);
  return v;
}

inline auto err(std::string msg) { return Failure{std::vector<std::string>{std::move(msg)}}; }

template <typename T, typename E, typename... Ts>
auto value_or_errors(T t, std::vector<E> errors, Ts... ts) {
  return errors.empty() ? success(knot::Type<std::vector<E>>{}, std::move(t), std::move(ts)...)
                        : fail(knot::Type<T>{}, std::move(errors), std::move(ts)...);
}

template <typename E, typename... Ts>
auto void_or_errors(std::vector<E> errors, Ts... ts) {
  return value_or_errors(std::tuple(), std::move(errors), std::move(ts)...);
}

template <typename T, typename E>
Result<T, std::vector<E>> result_and_errors(Result<T, std::vector<E>> r, std::vector<E> errors) {
  if(errors.empty()) {
    return r;
  } else if(r.has_value()) {
    return Failure{std::move(errors)};
  } else {
    return Failure{to_vec(std::move(errors), std::move(r.error()))};
  }
}

template <typename T, typename E, typename... Ts, typename F, typename Range>
Result<std::vector<T>, std::vector<E>, Ts...> accumulate_errors(F f, Range&& range, Ts... ts) {
  std::vector<T> values;
  std::vector<E> errors;
  for(auto&& ele : range) {
    auto result = f(ele, std::move(ts)...);
    std::tie(ts...) = std::move(result).state();
    if(result) {
      values = to_vec(std::move(*result), std::move(values));
    } else {
      errors = to_vec(std::move(result).error(), std::move(errors));
    }
  }

  return value_or_errors(std::move(values), std::move(errors), std::move(ts)...);
}

} // namespace ooze
