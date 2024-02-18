fn double(x: i32) -> i32 = add(x, x)

fn add1(x: i32) -> i32 = add(x, 1)
fn sub1(x: i32) -> i32 = add(x, -1)

fn apply(f: fn(_) -> _, x) -> _ = f(x)
fn compose(f: fn(_) -> _, g: fn(_) -> _, x) -> _ = g(f(x))

fn main() -> i32 {
  // TODO: make this deducible
  let x: i32 = apply(add1, 3);
  assert_eq(7, compose(double, sub1, x))
}
