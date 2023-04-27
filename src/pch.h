#pragma once

#include "algorithm.h"
#include "ooze/slice.h"

#include <anyf/span.h>
#include <fmt/core.h>
#include <knot/core.h>

#include <tl/expected.hpp>

#include <cstdint>
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
using Map = std::unordered_map<Key, Value>;

template <typename T>
using Set = std::unordered_set<T, knot::Hash>;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using f32 = float;
using f64 = double;

using anyf::Span;

template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename T>
using Result = tl::expected<T, std::vector<std::string>>;

inline void dump(const std::vector<std::string>& lines) {
  for(const std::string& line : lines) {
    fmt::print("{}\n", line);
  }
}

template <typename... Ts>
auto make_vector(Ts&&... ts) {
  std::vector<std::common_type_t<std::decay_t<Ts>...>> v;
  v.reserve(sizeof...(ts));
  (v.emplace_back(std::forward<Ts>(ts)), ...);
  return v;
}

inline Result<std::vector<std::string>> convert_errors(std::vector<std::string> errors) { return errors; }

inline auto err(std::string msg) { return tl::unexpected{std::vector<std::string>{std::move(msg)}}; }

template <typename T, typename E>
tl::expected<T, std::vector<E>> value_or_errors(T t, std::vector<E> errors) {
  return errors.empty() ? tl::expected<T, std::vector<E>>{std::move(t)} : tl::unexpected{std::move(errors)};
}

template <typename E, typename T>
auto success(T t) {
  return tl::expected<T, std::vector<E>>{std::move(t)};
}

template <typename E>
std::tuple<> get_value(tl::expected<void, E>&&) {
  return {};
}

template <typename T, typename E>
std::tuple<T> get_value(tl::expected<T, E>&& e) {
  return std::tuple(std::move(e.value()));
}

template <typename T>
std::tuple<T> get_value(T&& t) {
  return std::tuple(std::move(t));
}

template <typename T, typename E>
void append_errors(std::vector<E>& errors, tl::expected<T, std::vector<E>>&& e) {
  if(!e) {
    errors.insert(errors.end(), std::make_move_iterator(e.error().begin()), std::make_move_iterator(e.error().end()));
  }
}

template <typename T, typename E>
void append_errors(std::vector<E>&, T&&) {}

template <typename T>
constexpr bool is_expected(knot::Type<T>) {
  return false;
}

template <typename T, typename E>
constexpr bool is_expected(knot::Type<tl::expected<T, E>>) {
  return true;
}

template <typename T, typename E>
constexpr auto error_type(knot::Type<tl::expected<T, E>>) {
  return knot::Type<E>{};
}

template <typename... Ts>
constexpr auto error_type(knot::TypeList<Ts...> tl) {
  if constexpr(size(tl) == 0) {
    return knot::NotAType{};
  } else if constexpr(is_expected(head(tl))) {
    return error_type(head(tl));
  } else {
    return error_type(tail(tl));
  }
}

template <typename... Ts>
auto merge(Ts... ts) {
  using error_type = knot::type_t<decltype(value_type(error_type(knot::TypeList<Ts...>{})))>;

  std::vector<error_type> errors;
  (append_errors(errors, std::move(ts)), ...);

  return errors.empty() ? success<error_type>(std::tuple_cat(get_value(std::move(ts))...))
                        : tl::unexpected{std::move(errors)};
}

} // namespace ooze
