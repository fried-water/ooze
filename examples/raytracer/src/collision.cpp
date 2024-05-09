#include "collision.h"

#include <fmt/core.h>

namespace rt {

namespace {

std::optional<Contact> intersect(const Sphere& s, const Ray& r) {
  const Vec3 oc = s.center - r.origin;
  const float a = dot(r.dir, r.dir);
  const float b = -2.0 * dot(r.dir, oc);
  const float c = dot(oc, oc) - s.radius * s.radius;
  const float discriminant = b * b - 4 * a * c;

  if(discriminant < 0.0f) return {};

  const float d = 2.0f * a;
  const float sq = std::sqrt(discriminant);

  // Throw away when t < 0
  const float t0 = (-b - sq) / d;

  const float t = t0 < 0 ? (-b + sq) / d : t0;

  return t > 0.0001f ? std::optional(Contact{r(t), (r(t) - s.center) / s.radius, t}) : std::nullopt;
}

std::optional<Contact> intersect(const Plane& p, const Ray& r) {
  if(const float d = dot(p.normal, r.dir); d < 0.0f) {
    const float t = dot(p.normal * p.d - r.origin, p.normal) / d;
    return t >= 0.0f ? std::optional(Contact{r(t), p.normal, t}) : std::nullopt;
  }

  return std::nullopt;
}

} // namespace

std::optional<Contact> intersect(const Shape& s, const Ray& r) {
  return std::visit([&](const auto& s) { return intersect(s, r); }, s);
}

} // namespace rt
