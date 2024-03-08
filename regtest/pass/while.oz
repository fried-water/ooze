fn f(x: i32, y: &i32, z: &i32) -> i32 =
  while le(x, clone(y)) {
    add(x, clone(z))
  }

fn main() -> i32 = assert_eq(f(1, &8, &3), 10)
