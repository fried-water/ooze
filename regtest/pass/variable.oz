fn main() -> i32 {
  let x = 6;
  let y = x;
  let z = y;
  let x = z;
  assert_eq(6, x)
}
