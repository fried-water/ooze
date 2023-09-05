#pragma once

#include <knot/core.h>

namespace ooze {

struct Slice {
  int begin = 0;
  int end = 0;

  KNOT_ORDERED(Slice);
};

inline int size(Slice s) { return s.end - s.begin; }

} // namespace ooze
