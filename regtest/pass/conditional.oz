fn cond(cond: bool, x: i32, y: i32) -> i32 {
  if cond { x } else { y }
}

fn sel(cond: bool, x: i32, y: i32) -> i32 {
  select cond { x } else { y }
}

fn main() -> i32 {
  let x = cond(true, 7, 0);
  let x = cond(false, 0, x);
  let x = sel(true, x, 0);
  let x = sel(false, 0, x);
  assert_eq(7, x)
}
