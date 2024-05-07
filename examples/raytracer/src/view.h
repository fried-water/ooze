#pragma once

#include "vec.h"

#include <numbers>

namespace rt {

constexpr inline Vec3 Zero = {0.0f, 0.0f, 0.0f};
constexpr inline Vec3 Up = {0.0f, 1.0f, 0.0f};

struct Camera {
  Vec3 pos;
  Vec3 look_at = Zero;
  Vec3 up = Up;
};

struct ViewPort {
  Vec2i size; // derive aspect ratio
  float vertical_fov = std::numbers::pi_v<float> / 2.0f;
  float defocus_angle = 0.0f;
  int samples_per_pixel = 100;
  int max_depth = 10;
};

} // namespace rt
