#pragma once

#include "collision.h"
#include "ray.h"
#include "vec.h"
#include "view.h"

#include <fmt/core.h>

#include <atomic>
#include <memory>
#include <span>
#include <string>
#include <vector>

namespace rt {

struct Material {
  Colorf albedo;
  Colorf emissive;
  float scatter_reflectance = 0.0f;
};

struct Object {
  Shape shape;
  Material material;
};

using Scene = std::vector<Object>;

using Tracker = std::shared_ptr<std::pair<int, std::atomic<int>>>;

inline Tracker create_tracker(Vec2i size) {
  return std::make_shared<std::pair<int, std::atomic<int>>>(product(size), 0);
}

inline std::string progress(const Tracker& t) {
  const auto processed_pixels = t->second.load();
  return fmt::format("{}/{} ({}%)", processed_pixels, t->first, int(100.0f * processed_pixels / t->first));
}

void raytrace(const Camera&, const ViewPort&, const Scene&, std::span<Color>, Tracker = {});

void raytrace_row_serial(const Camera&, const ViewPort&, const Scene&, int row, std::span<Color>, Tracker = {});
void raytrace_row_parallel(const Camera&, const ViewPort&, const Scene&, int row, std::span<Color>, Tracker = {});

} // namespace rt
