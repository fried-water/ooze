#include "raytrace.h"

#include <oneapi/tbb.h>

#include <cassert>
#include <random>

namespace rt {

namespace {

struct Window {
  float pixel_size;
  float defocus_multiplier;
  Vec3 top_left;
  Vec3 u;
  Vec3 pixel_u;
  Vec3 pixel_v;
};

Vec3 pixel_position(const Window& w, Vec2i pixel) { return w.top_left + w.pixel_u * pixel[0] - w.pixel_v * pixel[1]; }

Window calculate_window(Camera cam, ViewPort view) {
  const float focus_length = length(cam.look_at - cam.pos);
  const Vec3 cam_dir = normalized(cam.look_at - cam.pos);

  const Vec3 u = cross(cam.up, cam_dir);
  const Vec3 v = cross(cam_dir, u);

  const float pixel_distance = std::tan(view.vertical_fov) * focus_length / view.size[1];

  const Vec3 pixel_u = u * pixel_distance;
  const Vec3 pixel_v = v * pixel_distance;

  const Vec3 view_top_left =
    cam.look_at + pixel_v * (view.size[1] * 0.5f - 0.5f) - pixel_u * (view.size[0] * 0.5f - 0.5f);

  return {pixel_distance, std::tan(view.defocus_angle) * focus_length, view_top_left, u, pixel_u, pixel_v};
}

Color to_color(Colorf c) {
  c = clamp(c, 0.0f, 1.0f);
  c = apply(c, [](float f) { return std::sqrt(f); }); // gamma correction
  return cast<char>(c * 255.0f);
}

Vec3 random_unit_vector(std::mt19937& rng) {
  std::uniform_real_distribution<float> dist(-1.0f, 1.0f);
  return normalized(Vec3{dist(rng), dist(rng), dist(rng)});
}

Vec3 lambertian(std::mt19937& rng, Vec3 dir, Vec3 normal) {
  const Vec3 scatter_dir = normal + random_unit_vector(rng);
  return normalized(near_zero(scatter_dir) ? normal : scatter_dir);
}

Vec3 reflect(Vec3 dir, Vec3 normal) { return dir - 2.0f * dot(dir, normal) * normal; }

Ray perturbed_ray(std::mt19937& rng, Vec3 origin, Vec3 pixel, Vec3 u, float pixel_radius, float defocus_multiplier) {
  const Vec3 ray_dir = normalized(pixel - origin);

  std::uniform_real_distribution<float> angle_dist(0.0f, std::numbers::pi_v<float> * 2.0f);
  std::uniform_real_distribution<float> offset_dist(0.0f, pixel_radius);

  const Vec3 origin_offset = rotate_around_plane(ray_dir, u, angle_dist(rng)) * defocus_multiplier;
  const Vec3 pixel_offset = rotate_around_plane(ray_dir, u, angle_dist(rng)) * offset_dist(rng);

  return Ray{origin + origin_offset, normalized(pixel + pixel_offset - origin - origin_offset)};
}

Colorf ray_color(std::mt19937& rng, const Ray& ray, const Scene& scene, int depth) {
  if(depth == 0) {
    return Colorf{};
  }

  const auto opt_contact = std::accumulate(
    scene.begin(), scene.end(), std::optional<std::pair<Contact, Material>>{}, [&](auto acc, const Object& obj) {
      if(const auto contact = intersect(obj.shape, ray); acc && contact) {
        return acc->first.t < contact->t ? acc : std::optional(std::pair(*contact, obj.material));
      } else if(contact) {
        return std::optional(std::pair(*contact, obj.material));
      } else {
        return acc;
      }
    });

  if(!opt_contact) {
    return Colorf{};
  }

  const auto [contact, material] = *opt_contact;

  if(material.albedo != Colorf{}) {
    const Vec3 scatter_dir =
      lerp(lambertian(rng, ray.dir, contact.normal), reflect(ray.dir, contact.normal), material.scatter_reflectance);
    return material.emissive + material.albedo * ray_color(rng, {contact.pos, scatter_dir}, scene, depth - 1);
  } else {
    return material.emissive;
  }
}

Color raytrace_pixel(std::mt19937& rng,
                     const Camera& camera,
                     const ViewPort& view,
                     const Window& window,
                     const Scene& scene,
                     Vec2i coord) {
  const float perturb_radius = length(Vec2{window.pixel_size, window.pixel_size});

  const Vec3 pixel = pixel_position(window, coord);

  Colorf color = {};
  for(int r = 0; r < view.samples_per_pixel; r++) {
    color =
      color + ray_color(rng,
                        perturbed_ray(rng, camera.pos, pixel, window.u, perturb_radius, window.defocus_multiplier),
                        scene,
                        view.max_depth);
  }
  return to_color(color / float(view.samples_per_pixel));
}

} // namespace

void raytrace_row_serial(
  const Camera& camera, const ViewPort& view, const Scene& scene, int row, std::span<Color> image_row) {
  const Window window = calculate_window(camera, view);

  assert(view.size[0] == img.size());

  auto rng = std::mt19937{unsigned(row)};

  for(int col = 0; col < view.size[0]; col++) {
    image_row[col] = raytrace_pixel(rng, camera, view, window, scene, {col, row});
  }
}

void raytrace_row_parallel(
  const Camera& camera, const ViewPort& view, const Scene& scene, int row, std::span<Color> image_row) {
  const Window window = calculate_window(camera, view);
  assert(view.size[0] == img.size());
  tbb::parallel_for(tbb::blocked_range<int>(0, view.size[0], 10), [&](auto r) {
    for(int col = r.begin(); col < r.end(); col++) {
      auto rng = std::mt19937{unsigned(view.size[0] * row + col)};
      image_row[col] = raytrace_pixel(rng, camera, view, window, scene, {col, row});
    }
  });
}

void raytrace(const Camera& camera, const ViewPort& view, const Scene& scene, std::span<Color> image) {
  assert(product(view.size) == img.size());

  tbb::parallel_for(tbb::blocked_range<int>(0, view.size[1]), [&](auto r) {
    for(int row = r.begin(); row < r.end(); row++) {
      raytrace_row_serial(camera, view, scene, row, image.subspan(view.size[0] * row, view.size[0]));
    }
  });
}

} // namespace rt
