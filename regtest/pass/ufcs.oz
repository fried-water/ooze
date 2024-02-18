fn double(x: i32) -> i32 = add(x, x)

fn main() -> i32 {
  let x = 1.add(2).add(-1).double();
  assert_eq(4, x)
}
