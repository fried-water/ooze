#include "pch.h"

#include "repl.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

BOOST_AUTO_TEST_CASE(repl_empty) {
  Repl r;
  BOOST_CHECK(step_repl(Env{}, r, "").empty());
  BOOST_CHECK(r.bindings.empty());
  BOOST_CHECK(r.graphs.empty());
}

BOOST_AUTO_TEST_CASE(repl_run_expr) {
  Env e = create_primative_env();
  Repl r;

  const std::vector<std::string> expected{"3"};
  BOOST_CHECK(expected == step_repl(e, r, "3"));

  const std::vector<std::string> expected2{"abc"};
  BOOST_CHECK(expected2 == step_repl(e, r, "'abc'"));

  e.add_function("pow", AnyFunction([](int x) { return x * x; }));
  const std::vector<std::string> expected3{"9"};
  BOOST_CHECK(expected3 == step_repl(e, r, "pow(3)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_function) {
  Env e = create_primative_env();
  Repl r;

  step_repl(e, r, "let x = 1");

  const std::vector<std::string> expected{"use of undeclared function 'missing'"};
  BOOST_CHECK(expected == step_repl(e, r, "missing()"));
  BOOST_CHECK(expected == step_repl(e, r, "missing(x)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_binding) {
  Env e = create_primative_env();
  Repl r;

  e.add_function("identity", [](int x) { return x; });

  const std::vector<std::string> expected{"Identifier x not found"};
  BOOST_CHECK(expected == step_repl(e, r, "x"));
  BOOST_CHECK(expected == step_repl(e, r, "identity(x)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_function_bindings) {
  Env e = create_primative_env();
  Repl r;

  const std::vector<std::string> expected{
    "Identifier x not found", "Identifier y not found", "use of undeclared function 'missing'"};

  BOOST_CHECK(expected == step_repl(e, r, "missing(x, y)"));
}

BOOST_AUTO_TEST_CASE(repl_pass_binding_by_value) {
  Env e = create_primative_env();
  Repl r;

  e.add_function("make_ptr", [](int x) { return std::make_unique<int>(x); });
  e.add_function("take_ptr", [](std::unique_ptr<int> x) { return *x; });

  BOOST_CHECK(step_repl(e, r, "let x = make_ptr(5)").empty());

  const std::vector<std::string> expected{"5"};
  BOOST_CHECK(expected == step_repl(e, r, "take_ptr(x)"));
}

BOOST_AUTO_TEST_CASE(repl_bindings) {
  Env e;
  Repl r;

  const std::vector<std::string> no_bindings{"0 binding(s)"};
  BOOST_CHECK(no_bindings == step_repl(e, r, ":b"));

  BOOST_CHECK(step_repl(e, r, "let x = 5").empty());
  BOOST_CHECK(step_repl(e, r, ":a").empty());

  const std::vector<std::string> one_unknown_binding{"1 binding(s)",
                                                     fmt::format("  x: type 0x{:x}", anyf::type_id<int>())};
  BOOST_CHECK(one_unknown_binding == step_repl(e, r, ":b"));

  BOOST_CHECK(step_repl(e, r, "let y = 'abc'").empty());
  BOOST_CHECK(step_repl(e, r, ":a").empty());

  e.add_type<int>("i32");

  const std::vector<std::string> two_bindings{
    "2 binding(s)", "  x: i32", fmt::format("  y: {}", type_name_or_id(e, anyf::type_id<std::string>()))};
  BOOST_CHECK(two_bindings == step_repl(e, r, ":b"));

  BOOST_CHECK(r.bindings.size() == 2);

  BOOST_CHECK(anyf::type_id<int>() == r.bindings.at("x").type);
  BOOST_CHECK(anyf::type_id<std::string>() == r.bindings.at("y").type);

  BOOST_CHECK(5 == anyf::any_cast<int>(std::move(r.bindings.at("x").future).wait()));
  BOOST_CHECK("abc" == anyf::any_cast<std::string>(std::move(r.bindings.at("y").future).wait()));
}

BOOST_AUTO_TEST_CASE(repl_types) {
  Env e;
  Repl r;

  add_tieable_type<int>(e, "i32");

  struct A {};
  struct B {};

  e.add_type<A>("A");

  e.add_function("to_string", [](const B&) { return std::string("B"); });

  const std::vector<std::string> expected{
    "3 type(s)",
    "  A                    [to_string: N, serialize: N]",
    "  i32                  [to_string: Y, serialize: Y]",
    fmt::format("  {:<20} [to_string: Y, serialize: N]", type_name_or_id(e, anyf::type_id<B>()))};

  BOOST_CHECK(expected == step_repl(e, r, ":t"));
}

BOOST_AUTO_TEST_CASE(repl_functions) {
  Env e = create_primative_env();
  Repl r;

  struct A {};

  const auto a_type = anyf::type_id<A>();

  e.add_function("create_a", []() { return A{}; });
  e.add_function("read_a", [](const A&) {});
  e.add_function("take_a", [](A) {});
  e.add_function("pow", [](int x) { return x * x; });
  e.add_function("concat", [](const std::string& a, const std::string& b) { return a + b; });

  const std::vector<std::string> expected{"5 function(s)",
                                          "  clone [12 overloads]",
                                          "  to_string [12 overloads]",
                                          "  concat(string&, string&) -> string",
                                          fmt::format("  create_a() -> {}", type_name_or_id(e, a_type)),
                                          "  pow(i32) -> i32",
                                          fmt::format("  read_a({}&) -> ()", type_name_or_id(e, a_type)),
                                          fmt::format("  take_a({}) -> ()", type_name_or_id(e, a_type))};

  BOOST_CHECK(expected == step_repl(e, r, ":f"));
}

} // namespace ooze
