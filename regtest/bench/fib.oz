fn fib(x: i32) -> i32 {
  if le(x, 1) {
    x
  } else {
    add(fib(sub(x, 1)), fib(sub(x, 2)))
  }
}

fn main(args) -> () {
  let x =
    if eq(1, len(&args)) {
      stoi(&get(&args, 0))
    } else {
      30
    };
  println(&to_string(&fib(x)))
}
