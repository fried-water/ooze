#pragma once

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

struct Slice {
  u32 begin = 0;
  u32 end = 0;

  KNOT_COMPAREABLE(Slice);
};

inline u32 size(Slice s) { return s.end - s.begin; }

template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename T, typename Range, typename F>
T accumulate(Range&& range, F f, T acc = {}) {
  for(auto&& value : range) {
    if constexpr(std::is_reference_v<Range>) {
      acc = f(std::move(acc), value);
    } else {
      acc = f(std::move(acc), std::move(value));
    }
  }
  return acc;
}

template <typename T>
using Result = tl::expected<T, std::vector<std::string>>;

inline void dump(const std::vector<std::string>& lines) {
  for(const std::string& line : lines) {
    fmt::print("{}\n", line);
  }
}

inline Result<std::vector<std::string>> convert_errors(std::vector<std::string> errors) { return errors; }

inline auto err(std::string msg) { return tl::unexpected{std::vector<std::string>{std::move(msg)}}; }

template <typename T>
Result<T> value_or_errors(T t, std::vector<std::string> errors) {
  return errors.empty() ? Result<T>{std::move(t)} : tl::unexpected{std::move(errors)};
}

template <typename T>
Result<T> success(T t) {
  return Result<T>{std::move(t)};
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
void append_errors(std::vector<std::string>& errors, tl::expected<T, E>&& e) {
  if(!e) {
    errors.insert(errors.end(), std::make_move_iterator(e.error().begin()), std::make_move_iterator(e.error().end()));
  }
}

template <typename T>
void append_errors(std::vector<std::string>&, T&&) {}

template <typename... Ts>
auto merge(Ts... ts) {
  std::vector<std::string> errors;
  (append_errors(errors, std::move(ts)), ...);

  return errors.empty() ? success(std::tuple_cat(get_value(std::move(ts))...)) : tl::unexpected{std::move(errors)};
}

} // namespace ooze
