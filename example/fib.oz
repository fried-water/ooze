fn fib(x: i32) -> i32 {
  if le(x, 1) {
    x
  } else {
    add(fib(sub(x, 1)), fib(sub(x, 2)))
  }
}

fn main() -> () = println(&to_string(&fib(30)))
