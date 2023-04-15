#include "test.h"

#include "queries.h"
#include "repl.h"

namespace ooze {

#define compare_output(_EXP, _ACT)                                                                                     \
  [](const std::vector<std::string>& e, const std::vector<std::string>& a) {                                           \
    if(e != a) {                                                                                                       \
      fmt::print("E: {}\n", knot::debug(e));                                                                           \
      fmt::print("A: {}\n", knot::debug(a));                                                                           \
    }                                                                                                                  \
    BOOST_CHECK(e == a);                                                                                               \
  }(_EXP, _ACT)

BOOST_AUTO_TEST_CASE(repl_empty) {
  RuntimeEnv r = make_default_runtime(Env{});
  BOOST_CHECK(step_repl(r, "").empty());
  BOOST_CHECK(r.bindings.empty());
}

BOOST_AUTO_TEST_CASE(repl_run_expr) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  compare_output({"3"}, step_repl(r, "3"));
  compare_output({"abc"}, step_repl(r, "'abc'"));

  r.env.add_function("pow", [](int x) { return x * x; });
  compare_output({"9"}, step_repl(r, "pow(3)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_function) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  BOOST_CHECK(step_repl(r, "let x = 1").empty());

  const std::vector<std::string> expected{
    "1:0 error: use of undeclared function 'missing'", " | missing()", " | ^~~~~~~~~"};
  compare_output(expected, step_repl(r, "missing()"));

  const std::vector<std::string> expected2{
    "1:0 error: use of undeclared function 'missing'", " | missing(x)", " | ^~~~~~~~~~"};
  compare_output(expected2, step_repl(r, "missing(x)"));
}

BOOST_AUTO_TEST_CASE(repl_missing_binding) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  r.env.add_function("identity", [](int x) { return x; });

  const std::vector<std::string> expected{"1:0 error: use of undeclared binding 'x'", " | x", " | ^"};
  compare_output(expected, step_repl(r, "x"));

  const std::vector<std::string> expected2{
    "1:9 error: use of undeclared binding 'x'", " | identity(x)", " |          ^"};

  compare_output(expected2, step_repl(r, "identity(x)"));
}

BOOST_AUTO_TEST_CASE(repl_pass_binding_by_value) {
  RuntimeEnv r = make_default_runtime(create_primative_env());
  r.env.add_type<std::unique_ptr<int>>("unique_int");

  r.env.add_function("make_ptr", [](int x) { return std::make_unique<int>(x); });
  r.env.add_function("take_ptr", [](std::unique_ptr<int> x) { return *x; });

  BOOST_REQUIRE(step_repl(r, "let x = make_ptr(5)").empty());

  compare_output({"5"}, step_repl(r, "take_ptr(x)"));
}

BOOST_AUTO_TEST_CASE(repl_bindings) {
  RuntimeEnv r = make_default_runtime(Env{});

  compare_output({"0 binding(s)"}, step_repl(r, ":b"));

  BOOST_REQUIRE(step_repl(r, "let x = 5").empty());
  BOOST_REQUIRE(step_repl(r, ":a").empty());

  const std::vector<std::string> one_unknown_binding{"1 binding(s)",
                                                     fmt::format("  x: type 0x{:x}", anyf::type_id<int>())};
  compare_output(one_unknown_binding, step_repl(r, ":b"));

  BOOST_REQUIRE(step_repl(r, "let y = 'abc'").empty());
  BOOST_REQUIRE(step_repl(r, ":a").empty());

  r.env.add_type<int>("i32");

  const std::vector<std::string> two_bindings{
    "2 binding(s)", "  x: i32", fmt::format("  y: {}", type_name_or_id(r.env, anyf::type_id<std::string>()))};
  compare_output(two_bindings, step_repl(r, ":b"));

  BOOST_REQUIRE(r.bindings.size() == 2);

  Binding& x = std::get<Binding>(r.bindings.at("x").v);
  Binding& y = std::get<Binding>(r.bindings.at("y").v);

  BOOST_CHECK(anyf::type_id<int>() == x.type);
  BOOST_CHECK(anyf::type_id<std::string>() == y.type);

  BOOST_CHECK(5 == anyf::any_cast<int>(std::move(x.future).wait()));
  BOOST_CHECK("abc" == anyf::any_cast<std::string>(std::move(y.future).wait()));
}

BOOST_AUTO_TEST_CASE(repl_bindings_post_dump) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  compare_output({}, step_repl(r, "let x = 5"));
  compare_output({"5"}, step_repl(r, "x"));
  const std::vector<std::string> expected{"1 binding(s)", fmt::format("  x: i32")};
  compare_output(expected, step_repl(r, ":b"));
}

BOOST_AUTO_TEST_CASE(repl_types) {
  struct A {};
  struct B {};

  RuntimeEnv r = make_default_runtime(Env{});

  add_tieable_type<int>(r.env, "i32");

  r.env.add_type<A>("A");

  // Types without names won't be reported
  r.env.add_function("to_string", [](const B&) { return std::string("B"); });

  const std::vector<std::string> expected{
    "2 type(s)", "  A                    [to_string: N]", "  i32                  [to_string: Y]"};

  compare_output(expected, step_repl(r, ":t"));
}

BOOST_AUTO_TEST_CASE(repl_functions) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  struct A {};

  const auto a_type = anyf::type_id<A>();

  r.env.add_function("create_a", []() { return A{}; });
  r.env.add_function("read_a", [](const A&) {});
  r.env.add_function("take_a", [](A) {});
  r.env.add_function("pow", [](int x) { return x * x; });
  r.env.add_function("concat", [](const std::string& a, const std::string& b) { return a + b; });

  const std::vector<std::string> expected{"9 function(s)",
                                          "  clone [13 overloads]",
                                          "  to_string [12 overloads]",
                                          "  serialize [12 overloads]",
                                          "  deserialize [12 overloads]",
                                          "  concat(&string, &string) -> string",
                                          fmt::format("  create_a() -> {}", type_name_or_id(r.env, a_type)),
                                          "  pow(i32) -> i32",
                                          "  read(&string) -> string",
                                          "  read(&string) -> vector<byte>",
                                          fmt::format("  read_a(&{}) -> ()", type_name_or_id(r.env, a_type)),
                                          fmt::format("  take_a({}) -> ()", type_name_or_id(r.env, a_type)),
                                          "  write(&string, &string) -> ()",
                                          "  write(&string, &vector<byte>) -> ()"};

  compare_output(expected, step_repl(r, ":f"));
}

} // namespace ooze
