#pragma once

#include <fmt/core.h>
#include <knot.h>
#include <span.h>

#include <cstdint>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ooze {

template <typename Key, typename Value>
using Map = std::unordered_map<Key, Value, knot::Hash>;

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

} // namespace ooze
