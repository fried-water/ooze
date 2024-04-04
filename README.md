# ooze

![build](https://github.com/fried-water/ooze/actions/workflows/cmake-build.yml/badge.svg)

**ooze** is a parallel scripting language. **ooze** takes a set of user defined types and functions and allows the creation and parallel execution of function DAGs. The original motivation was a DSL to define function DAGs that got a bit out of control.

## Example Script
```rust
// script.oz

fn double(p: &Point) -> Point {
  add(p, p)
}

fn triple(p: &Point) -> Point {
  p.double().add(p) // Supports UFCS
}

fn main() -> (Point, Point) {
  let x = create_point(1, 2)
  let y = create_point(2, 3)
  (double(x), triple(y))
}
```

## Setup Code
```cpp
// main.cpp

#include <ooze/repl.h>

struct Point {
  int x;
  int y;
};

int main(int argc, const char** argv) {
  // Creates a basic environment with all the primitive types and string
  ooze::Env e = ooze::create_primative_env();

  e.add_type<Point>("Point");

  e.add_function("create_point", [](int x, int y) { return Point{x, y}; });
  e.add_function("add", [](const Point& a, const Point& b) { return Point{a.x + b.x, a.y + b.y}; });
  e.add_function("to_string", [](const Point& p) { return "(" + std::to_string(p.x) + ", " + std::to_string(p.y) + ")"; });

  return ooze::main(argc, argv, e);
}
```

## Executing the script

When executing a script **ooze** will attempt to call main() and then call to_string() on all its results and dump them to stdout. During execution each function call is assigned its own parallel task and is executed when all its inputs are ready.

```
$ ./ooze run script.oz
(2, 4)
(6, 9)
```

## Repl

**ooze** also supports an asynchronous repl. You can chain function calls on values before the producing function has completed. **ooze** will block only when attempting to access the value.

```
$ ./ooze repl script.oz
Welcome to the ooze repl!
Try :h for help. Use Ctrl^D to exit.
> let x = create_point(1, 2)
> x
(1, 2)
> double(x)
(2, 4)
> :b
1 binding(s)
  x: Point
```

## Limitations

Functions are expected to be pure (outside of IO), arguments cannot be taken by mutable& or raw ptr.

Using fully owning types is preferable, any shared mutable state needs to be externally synchronized.

Currently **ooze** only supports function calls, assignment, conditionals. It does not support any form of type declaration, all types are registered via cpp.

## TODO

While loops
Select/Match statements
Higher Order range functions (map/filter/reduce)

Better repl terminal support (colouring, history, autocomplete)

Allow overload of main that takes a vector of string
