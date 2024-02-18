fn double(x: i32) -> i32 = add(x, x)

fn triple(x: i32) -> i32 = add(x, double(x))

fn main() -> i32 = assert_eq(9, triple(3))
