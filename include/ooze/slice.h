#pragma once

#include <knot/core.h>

namespace ooze {

struct Slice {
  int begin = 0;
  int end = 0;

  friend auto operator<=>(const Slice&, const Slice&) = default;
};

inline constexpr int size(Slice s) { return s.end - s.begin; }

} // namespace ooze
