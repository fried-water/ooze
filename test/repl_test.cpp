#include "pch.h"

#include "queries.h"
#include "repl.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

BOOST_AUTO_TEST_CASE(repl_empty) {
  Repl r;
  BOOST_CHECK(step_repl(r, "").empty());
  BOOST_CHECK(r.bindings.empty());
}

BOOST_AUTO_TEST_CASE(repl_run_expr) {
  Repl r{create_primative_env()};

  const std::vector<std::string> expected{"3"};
  BOOST_CHECK(expected == step_repl(r, "3"));

  const std::vector<std::string> expected2{"abc"};
  BOOST_CHECK(expected2 == step_repl(r, "'abc'"));

  r.env.add_function("pow", [](int x) { return x * x; });
  const std::vector<std::string> expected3{"9"};
  BOOST_CHECK(expected3 == step_repl(r, "pow(3)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_function) {
  Repl r{create_primative_env()};

  step_repl(r, "let x = 1");

  const std::vector<std::string> expected{"use of undeclared function 'missing'"};
  BOOST_CHECK(expected == step_repl(r, "missing()"));
  BOOST_CHECK(expected == step_repl(r, "missing(x)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_binding) {
  Repl r{create_primative_env()};

  r.env.add_function("identity", [](int x) { return x; });

  const std::vector<std::string> expected{"Identifier x not found"};
  BOOST_CHECK(expected == step_repl(r, "x"));
  BOOST_CHECK(expected == step_repl(r, "identity(x)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_function_bindings) {
  Repl r{create_primative_env()};

  const std::vector<std::string> expected{
    "Identifier x not found", "Identifier y not found", "use of undeclared function 'missing'"};

  BOOST_CHECK(expected == step_repl(r, "missing(x, y)"));
}

BOOST_AUTO_TEST_CASE(repl_pass_binding_by_value) {
  Repl r{create_primative_env()};

  r.env.add_function("make_ptr", [](int x) { return std::make_unique<int>(x); });
  r.env.add_function("take_ptr", [](std::unique_ptr<int> x) { return *x; });

  BOOST_CHECK(step_repl(r, "let x = make_ptr(5)").empty());

  const std::vector<std::string> expected{"5"};
  BOOST_CHECK(expected == step_repl(r, "take_ptr(x)"));
}

BOOST_AUTO_TEST_CASE(repl_bindings) {
  Repl r;

  const std::vector<std::string> no_bindings{"0 binding(s)"};
  BOOST_CHECK(no_bindings == step_repl(r, ":b"));

  BOOST_CHECK(step_repl(r, "let x = 5").empty());
  BOOST_CHECK(step_repl(r, ":a").empty());

  const std::vector<std::string> one_unknown_binding{"1 binding(s)",
                                                     fmt::format("  x: type 0x{:x}", anyf::type_id<int>())};
  BOOST_CHECK(one_unknown_binding == step_repl(r, ":b"));

  BOOST_CHECK(step_repl(r, "let y = 'abc'").empty());
  BOOST_CHECK(step_repl(r, ":a").empty());

  r.env.add_type<int>("i32");

  const std::vector<std::string> two_bindings{
    "2 binding(s)", "  x: i32", fmt::format("  y: {}", type_name_or_id(r.env, anyf::type_id<std::string>()))};
  BOOST_CHECK(two_bindings == step_repl(r, ":b"));

  BOOST_CHECK(r.bindings.size() == 2);

  BOOST_CHECK(anyf::type_id<int>() == r.bindings.at("x").type);
  BOOST_CHECK(anyf::type_id<std::string>() == r.bindings.at("y").type);

  BOOST_CHECK(5 == anyf::any_cast<int>(std::move(r.bindings.at("x").future).wait()));
  BOOST_CHECK("abc" == anyf::any_cast<std::string>(std::move(r.bindings.at("y").future).wait()));
}

BOOST_AUTO_TEST_CASE(repl_types) {
  Repl r;

  add_tieable_type<int>(r.env, "i32");

  struct A {};
  struct B {};

  r.env.add_type<A>("A");

  r.env.add_function("to_string", [](const B&) { return std::string("B"); });

  const std::vector<std::string> expected{
    "3 type(s)",
    "  A                    [to_string: N, serialize: N]",
    "  i32                  [to_string: Y, serialize: Y]",
    fmt::format("  {:<20} [to_string: Y, serialize: N]", type_name_or_id(r.env, anyf::type_id<B>()))};

  BOOST_CHECK(expected == step_repl(r, ":t"));
}

BOOST_AUTO_TEST_CASE(repl_functions) {
  Repl r{create_primative_env()};

  struct A {};

  const auto a_type = anyf::type_id<A>();

  r.env.add_function("create_a", []() { return A{}; });
  r.env.add_function("read_a", [](const A&) {});
  r.env.add_function("take_a", [](A) {});
  r.env.add_function("pow", [](int x) { return x * x; });
  r.env.add_function("concat", [](const std::string& a, const std::string& b) { return a + b; });

  const std::vector<std::string> expected{"5 function(s)",
                                          "  clone [12 overloads]",
                                          "  to_string [12 overloads]",
                                          "  concat(string&, string&) -> string",
                                          fmt::format("  create_a() -> {}", type_name_or_id(r.env, a_type)),
                                          "  pow(i32) -> i32",
                                          fmt::format("  read_a({}&) -> ()", type_name_or_id(r.env, a_type)),
                                          fmt::format("  take_a({}) -> ()", type_name_or_id(r.env, a_type))};

  BOOST_CHECK(expected == step_repl(r, ":f"));
}

} // namespace ooze
