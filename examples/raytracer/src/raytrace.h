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

void raytrace(const Camera&, const ViewPort&, const Scene&, std::span<Color>);

void raytrace_row_serial(const Camera&, const ViewPort&, const Scene&, int row, std::span<Color>);
void raytrace_row_parallel(const Camera&, const ViewPort&, const Scene&, int row, std::span<Color>);

} // namespace rt
