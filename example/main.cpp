#include "ooze/core.h"

#include <fmt/core.h>

#include <chrono>
#include <thread>

namespace {

struct Point {
  int x;
  int y;
};

struct Box {
  Point min;
  Point max;
};

struct Unspecified {
  int x;
};

Unspecified create_u(int x) { return {x}; }
int take_u(Unspecified u) { return u.x; }

Point add(Point a, Point b) { return {a.x + b.x, a.y + b.y}; }
Point scale(Point a, int s) { return {a.x * s, a.y * s}; }

int sleep(int x) {
  std::this_thread::sleep_for(std::chrono::duration<int, std::ratio<1>>(x));
  return x;
}

ooze::Env create_env() {
  ooze::Env e = ooze::create_primative_env();

  ooze::add_tieable_type<Point>(e, "Point");
  ooze::add_tieable_type<Box>(e, "Box");

  e.add_function("add", add);
  e.add_function("scale", scale);
  e.add_function("sleep", sleep);
  e.add_function("create_u", create_u);
  e.add_function("take_u", take_u);

  return e;
}

} // namespace

int main(int argc, const char** argv) {
  try {
    return ooze::main(argc, argv, create_env());
  } catch(const std::exception& e) {
    fmt::print("Error: uncaught exception {}\n", e.what());
  } catch(...) {
    fmt::print("Error: unknown exception\n");
  }

  return 1;
}
