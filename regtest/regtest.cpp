#include <ooze/repl.h>

#include <cstdlib>
#include <iostream>
#include <string>

namespace {

ooze::Env create_env() {
  ooze::Env e = ooze::create_primative_env();

  e.add_function("add", [](int x, int y) { return x + y; });
  e.add_function("add", [](int x, int y, int z) { return x + y + z; });
  e.add_function("add", [](int w, int x, int y, int z) { return w + x + y + z; });

  e.add_function("sub", [](int x, int y) { return x - y; });
  e.add_function("le", [](int x, int y) { return x <= y; });

  e.add_function("len", [](const std::string& x) { return int(x.size()); });

  e.add_function("assert_eq", [](int exp, int act) {
    if(exp != act) {
      std::cerr << "Expected: " << exp << " Actual: " << act << "\n";
    }
    return exp == act ? EXIT_SUCCESS : EXIT_FAILURE;
  });

  return e;
}

} // namespace

int main(int argc, const char** argv) { return ooze::repl_main(argc, argv, create_env()); }
