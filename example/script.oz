fn double(p: Point) -> Point = add(p, p)

fn triple(p: Point) -> Point {
  let x: Point = double(p);
  add(x, p)
}

// Can pass unspecified types through expressions
fn nested(x: i32) -> i32 = take_u(create_u(x))

// Can bind unspecified types, but the type must be deduced
fn bind(x: i32) -> i32 {
  let y = create_u(x);
  take_u(y)
}

fn main() -> Box {
  let p = create_point(&1, &2);
  let x = 1;
  let p1 = create_point(&sleep(bind(x)), &sleep(nested(x)));
  let p2 = double(p);
  let p3 = triple(p);
  create_box(&p2, &add(p1, scale(p3, sleep(x))))
}
