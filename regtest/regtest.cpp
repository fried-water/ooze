#include <ooze/repl.h>

#include <cstdlib>
#include <iostream>
#include <string>

namespace {

ooze::NativeRegistry create_registry() {
  return ooze::create_primitive_registry()
    .add_fn("add", [](int x, int y) { return x + y; })
    .add_fn("add", [](int x, int y, int z) { return x + y + z; })
    .add_fn("add", [](int w, int x, int y, int z) { return w + x + y + z; })
    .add_fn("sub", [](int x, int y) { return x - y; })
    .add_fn("le", [](int x, int y) { return x <= y; })
    .add_fn("eq", [](int x, int y) { return x == y; })
    .add_fn("len", [](const std::string& x) { return int(x.size()); })
    .add_fn("len", [](const std::vector<std::string>& v) { return int(v.size()); })
    .add_fn("assert_eq",
            [](int exp, int act) {
              if(exp != act) {
                std::cerr << "Expected: " << exp << " Actual: " << act << "\n";
              }
              return exp == act ? EXIT_SUCCESS : EXIT_FAILURE;
            })
    .add_fn("get", [](const std::vector<std::string>& v, int idx) { return v[idx]; })
    .add_fn("stoi", [](const std::string& x) { return std::stoi(x); });
}

} // namespace

int main(int argc, const char** argv) { return ooze::repl_main(argc, argv, ooze::Env(create_registry())); }
