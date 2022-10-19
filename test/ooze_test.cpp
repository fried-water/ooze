#include "pch.h"

#include "bindings.h"
#include "ooze/core.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

#include <boost/test/unit_test.hpp>

namespace ooze {

namespace {

Result<std::vector<Any>> run(Env e, std::string_view script, std::string_view expr) {
  RuntimeEnv r{std::move(e)};

  return parse_script(r.env, script)
    .and_then([&]() { return ooze::run(r, expr); })
    .map([](std::vector<Binding> bindings) {
      return knot::map<std::vector<Any>>(std::move(bindings), [](Binding b) { return take(std::move(b)).wait(); });
    });
}

} // namespace

BOOST_AUTO_TEST_CASE(ooze_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 {"
                                      "  sum(sum(x, y), y)"
                                      "}";

  Env env = create_primative_env();
  env.add_function("sum", [](int x, int y) { return x + y; });

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
  env.add_function("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  const auto results = run(env, script, "f(create_point(1, 2), create_point(9, 7))");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_CHECK(anyf::holds_alternative<Point>(results->front()));
  BOOST_CHECK((Point{19, 16}) == anyf::any_cast<Point>(results->front()));
}

BOOST_AUTO_TEST_CASE(ooze_already_move) {
  const auto script = "fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }";

  Env env = create_primative_env();
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  const std::vector<std::string> expected{"1:54 error: binding already moved",
                                          " | fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }",
                                          " |                                                       ^"};

  BOOST_CHECK(expected == run(env, script, "f(make_unique_int(0))").error());
}

BOOST_AUTO_TEST_CASE(ooze_clone) {
  Env env;
  env.add_type<std::string>("string");
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  const auto results = run(env, "", "clone('abc')");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_REQUIRE(anyf::holds_alternative<std::string>(results->front()));
  BOOST_CHECK("abc" == anyf::any_cast<std::string>(results->front()));
}

BOOST_AUTO_TEST_CASE(ooze_rebind) {
  const auto script = "fn f(x: i32) -> i32 { let x = double(x) let x = double(x) x }";

  Env env = create_primative_env();
  env.add_function("double", [](int x) { return x + x; });

  const auto results = run(env, script, "f(1)");

  BOOST_REQUIRE(results.has_value());
  BOOST_CHECK_EQUAL(1, results->size());
  BOOST_REQUIRE(anyf::holds_alternative<int>(results->front()));
  BOOST_CHECK(4 == anyf::any_cast<int>(results->front()));
}

} // namespace ooze
