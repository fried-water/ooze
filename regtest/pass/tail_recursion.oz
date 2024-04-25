fn tailcall(i: i32, limit: &i32) -> i32 {
  if le(limit.clone(), i) {
    i
  } else {
    tailcall(i.add(1), limit)
  }
}

fn mutual1(i: i32) -> i32 {
  if le(i, 0) {
    i
  } else {
    mutual2(sub(i, 1))
  }
}

fn mutual2(i: i32) -> i32 {
  mutual1(sub(i, 1))
}

fn main() -> i32 {
  // Should't stack overflow
  let x = tailcall(0, &100000);
  let y = mutual1(100000);
  assert_eq(100000, add(x, y))
}
