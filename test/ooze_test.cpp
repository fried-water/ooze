#include "pch.h"

#include "ooze/core.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

Result<std::vector<Any>> run(const Env& e, std::string_view script, std::string_view expr) {
  return run(e, script, expr, [](const auto&) -> Any {
    BOOST_REQUIRE(false);
    return Any{};
  });
}

BOOST_AUTO_TEST_CASE(ooze_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 {"
                                      "  sum(sum(x, y), y)"
                                      "}";

  Env env = create_primative_env();
  env.functions.emplace("sum", [](int x, int y) { return x + y; });

  const auto results = run(env, script, "f(5, 6)");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_CHECK(anyf::holds_alternative<i32>(results->front()));
  BOOST_CHECK_EQUAL(17, anyf::any_cast<i32>(results->front()));
}

struct Point {
  int x;
  int y;

  KNOT_COMPAREABLE(Point);
};

BOOST_AUTO_TEST_CASE(ooze_custom_type) {
  const auto script = "fn f(x: Point, y: Point) -> Point { sum(sum(x, y), y) }";

  Env env = create_primative_env();
  add_tieable_type<Point>(env, "Point");
  env.functions.emplace("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  const auto results = run(env, script, "f(create_point(1, 2), create_point(9, 7))");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_CHECK(anyf::holds_alternative<Point>(results->front()));
  BOOST_CHECK((Point{19, 16}) == anyf::any_cast<Point>(results->front()));
}

BOOST_AUTO_TEST_CASE(ooze_wrong_count) {
  const auto wrong_arg = "fn f(x: i32) -> u32 { identity() }";
  const auto wrong_bind = "fn f(x: u32) -> u32 { let () = identity(x) x }";
  const auto wrong_return = "fn f(x: u32) -> () { identity(x) }";
  const auto valid = "fn f(x: u32) -> u32 { identity(x) }";

  Env env = create_primative_env();
  env.functions.emplace("identity", [](u32 u) { return u; });

  BOOST_CHECK(err("identity expects 1 arg(s), given 0") == run(env, wrong_arg, "f()"));
  BOOST_CHECK(err("Assignment expects 0 value(s), given 1") == run(env, wrong_bind, "f()"));
  BOOST_CHECK(err("f returns 0 value(s), given 1") == run(env, wrong_return, "f()"));
  BOOST_CHECK(err("f expects 1 arg(s), given 0") == run(env, valid, "f()"));
}

BOOST_AUTO_TEST_CASE(ooze_wrong_type) {
  const auto wrong_arg = "fn f(x: i32) -> u32 { identity(x) }";
  const auto wrong_bind = "fn f(x: u32) -> u32 { let x: i32 = identity(x) x }";
  const auto wrong_return = "fn f(x: u32) -> i32 { identity(x) }";
  const auto valid = "fn f(x: u32) -> u32 { identity(x) }";

  Env env = create_primative_env();
  env.functions.emplace("identity", [](u32 u) { return u; });

  BOOST_CHECK(err("identity expects u32 for arg 0, given i32") == run(env, wrong_arg, "f()"));
  BOOST_CHECK(err("x expects i32, given u32") == run(env, wrong_bind, "f()"));
  BOOST_CHECK(err("f return element 0 expects i32, given u32") == run(env, wrong_return, "f()"));
  BOOST_CHECK(err("f expects u32 for arg 0, given i32") == run(env, valid, "f(0)"));
}

BOOST_AUTO_TEST_CASE(ooze_already_move) {
  const auto script = "fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }";

  Env env = create_primative_env();
  env.add_name<std::unique_ptr<int>>("unique_int");
  env.functions.emplace("make_unique_int", [](int x) { return std::make_unique<int>(x); });
  BOOST_CHECK(err("f return value for arg 1 already moved") == run(env, script, "f(make_unique_int(0))"));
}

BOOST_AUTO_TEST_CASE(ooze_clone) {
  Env env = create_primative_env();
  env.add_name<std::unique_ptr<int>>("unique_int");
  env.functions.emplace("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  auto res = run(env, "", "clone(make_unique_int(0))");

  BOOST_CHECK(err("clone expects 1 arg, given 0 arg(s)") == run(env, "", "clone()"));
  BOOST_CHECK(err("clone requires a copyable type, given unique_int") == run(env, "", "clone(make_unique_int(0))"));

  const auto results = run(env, "", "clone('abc')");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_REQUIRE(anyf::holds_alternative<std::string>(results->front()));
  BOOST_CHECK("abc" == anyf::any_cast<std::string>(results->front()));
}

} // namespace ooze
