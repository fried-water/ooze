#pragma once

#include "vec.h"

namespace rt {

struct Ray {
  Vec3 origin;
  Vec3 dir;
  Vec3 operator()(float t) const { return origin + dir * t; }
};

} // namespace rt
