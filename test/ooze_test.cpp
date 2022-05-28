#include "pch.h"

#include "ooze.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

BOOST_AUTO_TEST_CASE(ooze_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 {"
                                      "  sum(sum(x, y), y)"
                                      "}";

  Env env = create_primative_env();
  env.add("sum", [](int x, int y) { return x + y; });

  const auto results = run(env, script, std::vector<Any>{Any{5}, Any{6}}, "f");

  BOOST_CHECK_EQUAL(1, results.size());
  BOOST_CHECK(anyf::holds_alternative<i32>(results.front()));
  BOOST_CHECK_EQUAL(17, anyf::any_cast<i32>(results.front()));
}

struct Point {
  int x;
  int y;

  KNOT_COMPAREABLE(Point);
};

BOOST_AUTO_TEST_CASE(ooze_custom_type) {
  constexpr std::string_view script = "fn f(x: Point, y: Point) -> i32 {"
                                      "  sum(sum(x, y), y)"
                                      "}";

  Env env = create_primative_env();
  env.add<Point>("Point");
  env.add("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  const auto results = run(env, script, std::vector<Any>{Any{Point{1, 2}}, Any{Point{9, 7}}}, "f");

  BOOST_CHECK_EQUAL(1, results.size());
  BOOST_CHECK(anyf::holds_alternative<Point>(results.front()));
  BOOST_CHECK((Point{19, 16}) == anyf::any_cast<Point>(results.front()));
}

} // namespace ooze
