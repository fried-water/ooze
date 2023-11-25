#pragma once

#include "ooze/slice.h"
#include "ooze/strong_id.h"

#include <string>
#include <string_view>
#include <vector>

namespace ooze {

using SrcID = StrongID<struct SrcSpace>;

struct SrcRef {
  SrcID file;
  Slice slice;

  KNOT_ORDERED(SrcRef);
};

struct SrcFile {
  std::string name;
  std::string src;
};

using SrcMap = std::vector<SrcFile>;

inline constexpr std::string_view sv(std::string_view src, Slice s) { return {src.data() + s.begin, size_t(size(s))}; }
inline constexpr std::string_view sv(const SrcMap& m, SrcRef ref) { return sv(m[ref.file.get()].src, ref.slice); }

// TODO line / character numbers?

} // namespace ooze
