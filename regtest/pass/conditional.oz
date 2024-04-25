fn cond(c: bool, x: i32, y: i32) -> i32 {
  if c { x } else { y }
}

fn main() -> i32 {
  let x = cond(true, 7, 0);
  let x = cond(false, 0, x);
  assert_eq(7, x)
}
