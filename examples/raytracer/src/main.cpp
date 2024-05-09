#include "image.h"
#include "raytrace.h"

#include <fmt/core.h>
#include <ooze/repl.h>

#include <cassert>
#include <random>

namespace rt {

Scene create_scene(int size, int seed = 0) {
  std::mt19937 rng = std::mt19937(seed);

  std::uniform_real_distribution<float> offset(-0.3f, 0.3f);
  std::uniform_real_distribution<float> dist(0.f, 1.0f);

  const float min_radius = 0.15f;
  const float max_radius = 0.45f;
  const float radius_exp = 8.0f;

  Scene scene;

  for(int i = -size; i <= size; i++) {
    for(int j = -size; j <= size; j++) {
      const float rd = std::pow(dist(rng), radius_exp);
      const float radius = rd * (max_radius - min_radius) + min_radius;
      scene.push_back({Sphere{{float(i) + offset(rng), radius, float(j) + offset(rng)}, radius},
                       Material{Colorf{dist(rng), dist(rng), dist(rng)}, Colorf{}, dist(rng)}});
    }
  }

  // Earth
  scene.push_back({Sphere{{0.0f, -1000.f, 0.0f}, 1000.0f}, {{0.5f, 0.5f, 0.5f}}});

  return scene;
}

// TODO should probably make this an enum....
constexpr std::optional<bool> COPY_TYPE = std::optional(true);

ooze::NativeRegistry add_basic_fns(ooze::NativeRegistry r) {
  return std::move(r)
    .add_fn("add", [](int x, int y) { return x + y; })
    .add_fn("add", [](float x, float y) { return x + y; })
    .add_fn("sub", [](float x, float y) { return x - y; })
    .add_fn("modulo", [](int x, int y) { return x % y; })
    .add_fn("eq", [](int x, int y) { return x == y; });
}

template <typename T, size_t N>
ooze::NativeRegistry add_vec_entries(ooze::NativeRegistry r) {
  constexpr const char* suffix = std::is_same_v<T, int> ? "i" : "";

  using V = Vec<N, T>;

  const auto constructor = []<size_t... Is>(std::index_sequence<Is...>) {
    return [](std::enable_if_t<Is >= 0, T>... args) { return V{args...}; };
  };

  r.add_type<V>(fmt::format("Vec{}{}", N, suffix), COPY_TYPE);

  r.add_fn(fmt::format("vec{}{}", N, suffix), []() { return V{}; });
  r.add_fn(fmt::format("vec{}{}", N, suffix), constructor(std::make_index_sequence<N>()));

  r.add_fn("add", [](V a, V b) { return a + b; });
  r.add_fn("sub", [](V a, V b) { return a - b; });
  r.add_fn("mul", [](V a, V b) { return a * b; });
  r.add_fn("mul", [](T a, V b) { return a * b; });
  r.add_fn("mul", [](V a, T b) { return a * b; });
  r.add_fn("div", [](V a, T b) { return a / b; });
  r.add_fn("to_string", [](const V& v) { return knot::debug(v); });

  r.add_fn("x", [](V a) { return a[0]; });
  r.add_fn("y", [](V a) { return a[1]; });
  if constexpr(N >= 3) {
    r.add_fn("z", [](V a) { return a[2]; });
  }

  return r;
}

ooze::NativeRegistry add_scene_entries(ooze::NativeRegistry r) {
  return std::move(r)
    .add_type<Material>("Material", COPY_TYPE)
    .add_type<Shape>("Shape", COPY_TYPE)
    .add_type<Scene>("Scene")
    .add_fn("empty_scene", []() { return Scene{}; })
    .add_fn("append",
            [](Scene scene, Shape shape, Material mat) {
              scene.emplace_back(shape, mat);
              return scene;
            })
    .add_fn("concat",
            [](Scene s1, Scene s2) {
              s1.insert(s1.end(), s2.begin(), s2.end());
              return s1;
            })
    .add_fn("material",
            [](Vec3 c, Vec3 r, float a) {
              return Material{c, r, a};
            })
    .add_fn("sphere",
            [](Vec3 pos, float radius) {
              return Shape{Sphere{pos, radius}};
            })
    .add_fn("plane", [](Vec3 normal, float d) {
      return Shape{Plane{normal, d}};
    });
}

ooze::NativeRegistry add_view_entries(ooze::NativeRegistry r) {
  return std::move(r)
    .add_type<ViewPort>("ViewPort", COPY_TYPE)
    .add_fn("view",
            [](Vec2i dims) {
              return ViewPort{dims, 3.1415f / 9.0f, 0.0f, 50};
            })
    .add_fn("fov",
            [](ViewPort p, float fov) {
              p.vertical_fov = fov;
              return p;
            })
    .add_fn("samples_per_pixel",
            [](ViewPort p, int s) {
              p.samples_per_pixel = s;
              return p;
            })
    .add_fn("defocus_angle",
            [](ViewPort p, float a) {
              p.defocus_angle = a;
              return p;
            })
    .add_fn("max_depth",
            [](ViewPort p, int d) {
              p.max_depth = d;
              return p;
            })
    .add_fn("size", [](ViewPort v) { return v.size; });
}

ooze::NativeRegistry create_registry() {
  auto r = add_basic_fns(ooze::create_primitive_registry());
  r = add_vec_entries<int, 2>(std::move(r));
  r = add_vec_entries<float, 3>(std::move(r));

  r = add_view_entries(std::move(r));
  r = add_scene_entries(std::move(r));

  return std::move(r)
    .add_type<Camera>("Camera", COPY_TYPE)
    .add_type<Image>("Image")
    .add_fn("look_at",
            [](Vec3 pos, Vec3 target) {
              return Camera{pos, target};
            })
    .add_fn("create_scene", create_scene)
    .add_fn("create_empty_image", create_empty_image)
    .add_fn("raytrace_native",
            [](Camera camera, ViewPort view, const Scene& scene) {
              Image img = create_empty_image(view.size);
              raytrace(camera, view, scene, img.span());
              return img;
            })
    .add_fn("raytrace_row_parallel",
            [](Camera camera, ViewPort view, const Scene& scene, Image img, int row) {
              raytrace_row_parallel(camera, view, scene, row, img.span().subspan(view.size[0] * row, view.size[0]));
              return img;
            })
    .add_fn("write_png", [](const Image& img, const std::string& str) { return write_png(img, str.c_str()); });
}

} // namespace rt

int main(int argc, const char** argv) { return ooze::repl_main(argc, argv, ooze::Env(rt::create_registry())); }
