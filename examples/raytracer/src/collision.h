#pragma once

#include "ray.h"

#include <cmath>

#include <variant>

namespace rt {

struct Sphere {
  Vec3 center = {};
  float radius = 1.0f;
};

struct Plane {
  Vec3 normal = {};
  float d = {};
};

using Shape = std::variant<Sphere, Plane>;

struct Contact {
  Vec3 pos = {};
  Vec3 normal = {};
  float t = {};
};

std::optional<Contact> intersect(const Shape&, const Ray&);

} // namespace rt
