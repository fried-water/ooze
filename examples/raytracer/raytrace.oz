fn light(c: Vec3) -> Material { material(vec3(), c, 0.0f) }
fn diffuse(c: Vec3) -> Material { material(c, vec3(), 0.0f) }
fn metal(c: Vec3) -> Material { material(c, vec3(), 1.0f) }

fn skybox() -> Scene {
  let sky = light(vec3(0.5f, 0.7f, 1.0f));
  empty_scene()
    .append(plane(vec3(1.0f, 0.0f, 0.0f), -100.0f), sky)
    .append(plane(vec3(-1.0f, 0.0f, 0.0f), -100.0f), sky)
    .append(plane(vec3(0.0f, 1.0f, 0.0f), -100.0f), sky)
    .append(plane(vec3(0.0f, -1.0f, 0.0f), -100.0f), sky)
    .append(plane(vec3(0.0f, 0.0f, 1.0f), -100.0f), sky)
    .append(plane(vec3(0.0f, 0.0f, -1.0f), -100.0f), sky)
}

fn box(radius: f32) -> Scene {
  let m = diffuse(vec3(0.8f, 0.8f, 0.8f));
  empty_scene()
    .append(plane(vec3(1.0f, 0.0f, 0.0f), sub(0.0f, radius)), m)
    .append(plane(vec3(-1.0f, 0.0f, 0.0f), sub(0.0f, radius)), m)
    .append(plane(vec3(0.0f, 1.0f, 0.0f), sub(0.0f, radius)), m)
    .append(plane(vec3(0.0f, -1.0f, 0.0f), sub(0.0f, radius)), m)
    .append(plane(vec3(0.0f, 0.0f, 1.0f), sub(0.0f, radius)), m)
    // no plane in the -z
}

fn rt_helper(camera :Camera, view :ViewPort, scene: &Scene, img: Image, row: i32) -> Image {
  if eq(row, view.size().y()) {
    img
  } else {
    // Supports tail call optimization
    rt_helper(camera, view, scene,
      raytrace_row_parallel(camera, view, scene, img, row),
      add(row, 1))
  }
}

fn raytrace(camera :Camera, view :ViewPort, scene: &Scene) -> Image {
  rt_helper(camera, view, scene, create_empty_image(view.size()), 0)
}

fn render(scene: Scene, pos: Vec3, size: Vec2i, samples: i32, dof: f32, filename: string) -> i32 {
  let cam = look_at(pos, vec3());
  let v = view(size)
    .fov(0.5f)
    .samples_per_pixel(samples)
    .max_depth(5)
    .defocus_angle(dof);
  let image = raytrace(cam, v, &scene);
  write_png(&image, &filename)
}

fn render_spheres(size: Vec2i, samples: i32) -> i32 {
  render(concat(skybox(), create_scene(8, 0)), vec3(3.0f, 2.0f, 12.0f), size, samples, 0.0f, "spheres.png")
}

fn render_dof(size: Vec2i, samples: i32) -> i32 {
  render(concat(skybox(), create_scene(8, 0)), vec3(3.0f, 2.0f, 12.0f), size, samples, 0.004f, "depth_of_field.png")
}

fn render_lightroom(size: Vec2i, samples: i32) -> i32 {
  // TODO: add support for constants
  let white = vec3(1.0f, 1.0f, 1.0f);
  let purple = vec3(1.0f, 0.0f, 1.0f);
  let green = vec3(0.2f, 0.8f, 0.3f);
  let scene: Scene = box(2.0f)
    .append(sphere(vec3(0.0f, 2.0f, 0.0f), 0.5f), light(purple))
    .append(sphere(vec3(-2.0f, 0.0f, 0.0f), 0.3f), light(green))
    .append(sphere(vec3(2.0f, 0.0f, 0.0f), 0.5f), light(white))
    .append(sphere(vec3(-1.0f, -1.5f, -0.2f), 0.5f), diffuse(vec3(1.0f, 0.2f, 0.7f)))
    .append(sphere(vec3(-1.2f, 1.3f, 0.5f), 0.5f), diffuse(vec3(0.5f, 0.5f, 0.5f)))
    .append(sphere(vec3(1.3f, 0.5f, -1.0f), 0.5f), material(purple, vec3(), 0.8f))
    .append(sphere(vec3(0.0f, 0.0f, 0.0f), 0.7f), metal(white))
    .append(sphere(vec3(1.0f, -1.5f, 0.3f), 0.5f), metal(white));

  render(scene, vec3(0.0f, 0.0f, 10.0f), size, samples, 0.0f, "lights.png")
}

fn main() -> i32 {
  let size = vec2i(1024, 768);
  let samples = 1000;

  // These run in parallel since they have no dependencies
  let r1 = render_spheres(size, samples);
  let r2 = render_dof(size, samples);
  let r3 = render_lightroom(size, samples);

  if eq(3, r1.add(r2).add(r3)) { 0 } else { 1 }
}
