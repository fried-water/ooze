mod m {
  fn f() { 0 }
  fn g() { f() }
}

fn h() -> i32 { 1 }

fn main() -> i32 { assert_eq(1, h()) }
