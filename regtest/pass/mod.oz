mod m {
  fn f() -> _ = 0

  fn g() -> _ = f()
}

fn h() -> i32 = 1

fn main() -> i32 = assert_eq(1, h())
