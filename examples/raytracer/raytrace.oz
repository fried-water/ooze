fn rt_tail_recursive(camera :Camera, view :ViewPort, scene: &Scene, img: Image, t: Tracker, row: i32) -> Image {
  if eq(row, view.size().y()) {
    img
  } else {
    rt_tail_recursive(camera, view, scene,
      raytrace_row_parallel(camera, view, scene, img, t, row),
      t, add(row, 1))
  }
}

fn raytrace(camera :Camera, view :ViewPort, scene: &Scene, t: Tracker) -> Image {
  rt_tail_recursive(camera, view, scene,
    create_empty_image(view.size()),
    t, 0)
}

fn raytrace(camera :Camera, view :ViewPort, scene: &Scene) -> Image {
  rt_tail_recursive(camera, view, scene,
      create_empty_image(view.size()),
      create_tracker(view.size()), 0)
}

fn demo_setup(size: Vec2i) -> (Camera, ViewPort, Scene, Tracker) {
  (
    look_at(vec3(3.0f, 2.0f, 12.0f), vec3()),
    view(size)
      .with_fov(0.5f)
      .with_samples_per_pixel(100),
    create_scene(5, 0),
    create_tracker(size)
  )
}

fn render_dof(size: Vec2i) -> i32 {
  let cam = look_at(vec3(3.0f, 2.0f, 12.0f), vec3());
  let scene = create_scene(8, 0);
  let v = view(size)
    .with_fov(0.5f)
    .with_samples_per_pixel(300)
    .with_defocus_angle(0.004f);
  let image = raytrace(cam, v, &scene);
  write_png(&image, &"depth_of_field.png")
}

fn main() -> i32 {
  if eq(1, render_dof(vec2i(2560, 1440))) {
    0
  } else {
    1
  }
}
