fn double(p: Point) -> Point {
  add(p, p)
}

fn triple(p: Point) -> Point {
 let x: Point = double(p)
 add(x, p)
}

fn main(p: Point, x: i32) -> (Box, Point) {
  let p1 = create_point(sleep(x), sleep(x))
  let p2 = double(p)
  let p3 = triple(p)
  (create_box(p2, p3), scale(p1, sleep(x)))
}
