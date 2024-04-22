#pragma once

#include "ooze/slice.h"
#include "ooze/strong_id.h"

#include <array>
#include <string>
#include <string_view>

namespace ooze {

using SrcID = StrongID<struct SrcSpace>;

struct SrcRef {
  SrcID file;
  Slice slice;

  friend auto operator<=>(const SrcRef&, const SrcRef&) = default;
};

template <typename... Ts>
std::array<std::string_view, sizeof...(Ts)> make_sv_array(const Ts&... ts) {
  return {std::string_view(ts)...};
}

inline constexpr std::string_view sv(std::string_view src, Slice s) { return {src.data() + s.begin, size_t(size(s))}; }
inline constexpr std::string_view sv(Span<std::string_view> m, SrcRef ref) {
  return ref.file.is_valid() ? sv(m[ref.file.get()], ref.slice) : "";
}

inline Slice append_src(std::string& src, std::string_view name) {
  const auto ref = Slice{i32(src.size()), i32(src.size() + name.size())};
  src.insert(src.end(), name.begin(), name.end());
  return ref;
}

// TODO line / character numbers?

} // namespace ooze
