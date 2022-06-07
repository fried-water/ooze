#include "pch.h"

#include "ooze/core.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

namespace {

auto err(std::string err) { return tl::unexpected{std::vector<std::string>{std::move(err)}}; }

} // namespace

BOOST_AUTO_TEST_CASE(ooze_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 {"
                                      "  sum(sum(x, y), y)"
                                      "}";

  Env env = create_primative_env();
  env.add("sum", [](int x, int y) { return x + y; });

  const auto results = run(env, script, std::vector<Any>{Any{5}, Any{6}}, "f");

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
  env.add<Point>("Point");
  env.add("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  const auto results = run(env, script, std::vector<Any>{Any{Point{1, 2}}, Any{Point{9, 7}}}, "f");

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
  env.add("identity", [](u32 u) { return u; });

  BOOST_CHECK(err("identity expects 1 arg(s), given 0") == run(env, wrong_arg, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("Assignment expects 0 value(s), given 1") == run(env, wrong_bind, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("f returns 0 value(s), given 1") == run(env, wrong_return, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("f expects 1 arg(s), given 0") == run(env, valid, std::vector<Any>{}, "f"));
}

BOOST_AUTO_TEST_CASE(ooze_wrong_type) {
  const auto wrong_arg = "fn f(x: i32) -> u32 { identity(x) }";
  const auto wrong_bind = "fn f(x: u32) -> u32 { let x: i32 = identity(x) x }";
  const auto wrong_return = "fn f(x: u32) -> i32 { identity(x) }";
  const auto valid = "fn f(x: u32) -> u32 { identity(x) }";

  Env env = create_primative_env();
  env.add("identity", [](u32 u) { return u; });

  BOOST_CHECK(err("identity expects u32 for arg 0, given i32") == run(env, wrong_arg, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("x expects i32, given u32") == run(env, wrong_bind, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("f return element 0 expects i32, given u32") == run(env, wrong_return, std::vector<Any>{}, "f"));
  BOOST_CHECK(err("f expects u32 for arg 0, given i32") == run(env, valid, std::vector<Any>{0}, "f"));
}

BOOST_AUTO_TEST_CASE(ooze_already_move) {
  const auto script = "fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }";

  Env env = create_primative_env();
  env.add<std::unique_ptr<int>>("unique_int");

  std::vector<Any> inputs;
  inputs.push_back(std::make_unique<int>(0));

  BOOST_CHECK(err("f return value for arg 1 already moved") == run(env, script, std::move(inputs), "f"));
}

} // namespace ooze
