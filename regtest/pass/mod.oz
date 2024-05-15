fn f() { 1 }
fn g() { f() }

mod a {
  fn f() { 2 }
  fn g() { f() }

  mod b {
    fn f() { 3 }
    fn g() { f() }
  }
}

fn main() -> i32 {
  assert_eq(6, add(g(), a::g(), a::b::g()))
}
