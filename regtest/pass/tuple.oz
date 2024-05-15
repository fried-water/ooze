fn pack(x, y) -> (_, _) { (x, y) }

fn unpack((x): (_)) { x }

fn main() -> i32 {
  let tuple = ((1), 2, pack(3, 4));
  let (x, y, z) = tuple;
  let (z1, z2) = z;
  let x1 = unpack(x);
  assert_eq(10, add(x1, y, z1, z2))
}
