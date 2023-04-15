#pragma once

#include <algorithm>
#include <type_traits>
#include <vector>

namespace ooze {

template <typename R, typename T = typename std::decay_t<R>::value_type>
auto to_vec(R&& range, std::vector<T> v = {}) {
  using category = typename std::decay_t<R>::iterator::iterator_category;
  if constexpr(std::is_convertible_v<category, std::random_access_iterator_tag>) {
    v.reserve(v.size() + std::distance(std::begin(range), std::end(range)));
  }

  if constexpr(std::is_lvalue_reference_v<R>) {
    std::copy(std::begin(range), std::end(range), std::back_inserter(v));
  } else {
    std::copy(
      std::make_move_iterator(std::begin(range)), std::make_move_iterator(std::end(range)), std::back_inserter(v));
  }

  return v;
}

template <typename R,
          typename F,
          typename T = std::decay_t<decltype(std::declval<F>()(std::move(*std::declval<R>().begin())))>>
auto transform_to_vec(R&& range, F f, std::vector<T> v = {}) {
  using category = typename std::decay_t<R>::iterator::iterator_category;
  if constexpr(std::is_convertible_v<category, std::random_access_iterator_tag>) {
    v.reserve(v.size() + std::distance(std::begin(range), std::end(range)));
  }

  if constexpr(std::is_lvalue_reference_v<R>) {
    std::transform(std::begin(range), std::end(range), std::back_inserter(v), f);
  } else {
    std::transform(
      std::make_move_iterator(std::begin(range)), std::make_move_iterator(std::end(range)), std::back_inserter(v), f);
  }

  return v;
}

template <typename R,
          typename F,
          typename T = std::decay_t<decltype(*std::declval<F>()(std::move(*std::declval<R>().begin())))>>
auto transform_filter_to_vec(R&& range, F f, std::vector<T> v = {}) {
  for(auto&& ele : range) {
    if constexpr(std::is_lvalue_reference_v<R>) {
      auto&& opt_result = f(ele);
      if(opt_result) {
        v.push_back(std::move(*opt_result));
      }
    } else {
      auto&& opt_result = f(std::move(ele));
      if(opt_result) {
        v.push_back(std::move(*opt_result));
      }
    }
  }

  return v;
}

} // namespace ooze
