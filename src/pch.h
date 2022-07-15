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
using Map = std::unordered_map<Key, Value, knot::Hash>;

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

template <typename... Ts>
[[noreturn]] void error(const char* fmt_string, const Ts&... ts) {
  fmt::print("Error: {}\n", fmt::format(fmt_string, ts...));
  exit(1);
}

template <typename... Ts>
void check(bool b, const char* fmt_string, const Ts&... ts) {
  if(!b) {
    error(fmt_string, ts...);
  }
}

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

inline void dump_errors(const std::vector<std::string>& errors) {
  for(const std::string& e : errors) {
    fmt::print("{}\n", e);
  }
}

inline auto err(std::string msg) { return tl::unexpected{std::vector<std::string>{std::move(msg)}}; }

template <typename T>
Result<T> success(T t) {
  return Result<T>{std::move(t)};
}

template <typename... Ts, typename E>
tl::expected<std::tuple<Ts...>, std::vector<E>> merge(tl::expected<Ts, std::vector<E>>... results) {
  std::vector<E> errors;

  const auto copy_errors = [&](auto& res) {
    if(!res) {
      errors.insert(
        errors.end(), std::make_move_iterator(res.error().begin()), std::make_move_iterator(res.error().end()));
    }
  };

  (copy_errors(results), ...);

  return errors.empty() ? tl::expected<std::tuple<Ts...>, std::vector<E>>(std::tuple(std::move(results.value())...))
                        : tl::unexpected{std::move(errors)};
}

} // namespace ooze
