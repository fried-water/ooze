#include "pch.h"

#include "parser_combinators.h"

#include <boost/test/unit_test.hpp>

namespace ooze::pc {

namespace {

auto ch(char c) { return constant(std::string() + c, c); }
auto pass(char c) {
  return filter(any(), std::string() + c, [=](int x, char c2) { return c == c2; });
}

template <typename P, typename R>
auto test_pass(P parser,
               const std::string& str,
               R result,
               int remaining = 0,
               std::vector<std::pair<std::string, ParseLocation>> errors = {}) {
  const Span<char> span(str.data(), str.size());

  const auto actual = parser(ParseState<int, char>{0, span}, {});

  BOOST_CHECK_EQUAL(remaining, span.size() - actual.pos);
  BOOST_CHECK(errors == actual.errors);
  BOOST_CHECK(actual.value.has_value());
  BOOST_CHECK(!actual.value.has_value() || result == *actual.value);

  if(errors != actual.errors) {
    fmt::print("Actual:   {}\n", knot::debug(actual.errors));
    fmt::print("Expected: {}\n", knot::debug(errors));
  }
}

template <typename P>
auto test_fail(P parser,
               const std::string& str,
               int remaining = 0,
               std::vector<std::pair<std::string, ParseLocation>> errors = {}) {
  const Span<char> span(str.data(), str.size());

  const auto actual = parser(ParseState<int, char>{0, span}, {});

  BOOST_CHECK_EQUAL(actual.pos, str.size() - remaining);
  BOOST_CHECK(!actual.value.has_value());
  BOOST_CHECK(errors == actual.errors);

  if(errors != actual.errors) {
    fmt::print("Actual:   {}\n", knot::debug(actual.errors));
    fmt::print("Expected: {}\n", knot::debug(errors));
  }
}

} // namespace

BOOST_AUTO_TEST_CASE(pc_constant) {
  test_pass(ch('a'), "a", std::tuple(), 0);
  test_pass(ch('a'), "ab", std::tuple(), 1);
  test_pass(ch('a'), "abc", std::tuple(), 2);

  test_fail(ch('a'), "b", 1, {{"a", {0, 1}}});
  test_fail(ch('a'), "", 0, {{"a", {0, 1}}});
}

BOOST_AUTO_TEST_CASE(pc_any) {
  test_pass(any(), "a", 'a', 0);
  test_pass(any(), "b", 'b', 0);
  test_pass(any(), "ab", 'a', 1);

  test_fail(any(), "", 0, {{"anything", {0, 0}}});
}

BOOST_AUTO_TEST_CASE(pc_transform_if) {
  test_pass(pass('a'), "a", 'a', 0);
  test_pass(pass('b'), "b", 'b', 0);
  test_pass(pass('a'), "ab", 'a', 1);

  test_fail(pass('a'), "", 0, {{"a", {0, 0}}});
  test_fail(pass('a'), "b", 1, {{"a", {0, 0}}});
}

BOOST_AUTO_TEST_CASE(pc_maybe) {
  test_pass(maybe(ch('a')), "a", std::tuple(), 0);
  test_pass(maybe(ch('a')), "ab", std::tuple(), 1);
  test_pass(maybe(ch('a')), "abc", std::tuple(), 2);

  test_pass(maybe(ch('a')), "b", std::nullopt, 1, {{"a", {0, 2}}});
  test_pass(maybe(ch('a')), "", std::nullopt, 0, {{"a", {0, 2}}});
}

BOOST_AUTO_TEST_CASE(pc_n) {
  test_pass(n(ch('a')), "", std::vector<std::tuple<>>{}, 0, {{"a", {0, 2}}});
  test_pass(n(ch('a')), "a", std::vector<std::tuple<>>{{}}, 0, {{"a", {1, 2}}});
  test_pass(n(ch('a')), "aa", std::vector<std::tuple<>>{{}, {}}, 0, {{"a", {2, 2}}});
  test_pass(n(ch('a')), "aab", std::vector<std::tuple<>>{{}, {}}, 1, {{"a", {2, 2}}});
  test_pass(n(ch('a')), "b", std::vector<std::tuple<>>{}, 1, {{"a", {0, 2}}});
}

BOOST_AUTO_TEST_CASE(pc_seq) {
  test_pass(seq(), "", std::tuple(), 0);
  test_pass(seq(), "a", std::tuple(), 1);

  test_pass(seq(ch('a')), "a", std::tuple(), 0);
  test_pass(seq(ch('a'), ch('b')), "ab", std::tuple(), 0);

  test_pass(seq(pass('a'), ch('b')), "ab", 'a', 0);
  test_pass(seq(ch('a'), pass('b')), "ab", 'b', 0);
  test_pass(seq(pass('a'), pass('b')), "ab", std::tuple('a', 'b'), 0);

  test_pass(seq(pass('a'), pass('b'), pass('c')), "abc", std::tuple('a', 'b', 'c'), 0);
  test_pass(seq(ch('a'), pass('b'), ch('c')), "abc", 'b', 0);
  test_pass(seq(pass('a'), ch('b'), pass('c')), "abc", std::tuple('a', 'c'), 0);

  test_fail(seq(ch('a')), "", 0, {{"a", {0, 2}}});
  test_fail(seq(ch('a')), "b", 1, {{"a", {0, 2}}});
  test_fail(seq(ch('a'), ch('b')), "a", 0, {{"b", {1, 2}}});
  test_fail(seq(ch('a'), ch('b')), "aa", 1, {{"b", {1, 2}}});

  test_pass(seq(maybe(ch('a'))), "", std::nullopt, 0, {{"a", {0, 3}}});
  test_pass(seq(maybe(ch('a'))), "a", std::tuple(), 0);

  test_pass(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
            "c",
            std::tuple(std::nullopt, std::nullopt),
            0,
            {{"a", {0, 3}}, {"b", {0, 3}}});
  test_pass(
    seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "bc", std::tuple(std::nullopt, std::tuple()), 0, {{"a", {0, 3}}});
  test_pass(
    seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "ac", std::tuple(std::tuple(), std::nullopt), 0, {{"b", {1, 3}}});

  test_fail(seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "a", 0, {{"b", {1, 3}}, {"c", {1, 2}}});
  test_fail(seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "b", 0, {{"a", {0, 3}}, {"c", {1, 2}}});
  test_fail(seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "", 0, {{"a", {0, 3}}, {"b", {0, 3}}, {"c", {0, 2}}});
  test_fail(seq(maybe(ch('a')), maybe(ch('b')), ch('c')), "d", 1, {{"a", {0, 3}}, {"b", {0, 3}}, {"c", {0, 2}}});
}

struct CH1 {
  char c;
  KNOT_COMPAREABLE(CH1);
};

BOOST_AUTO_TEST_CASE(pc_choose) {
  test_pass(choose(pass('a')), "a", std::variant<char>('a'), 0);
  test_pass(choose(pass('a'), construct<CH1>(pass('b'))), "b", std::variant<char, CH1>(CH1{'b'}), 0);
  test_pass(choose(pass('a'), construct<CH1>(pass('a'))), "a", std::variant<char, CH1>('a'), 0);

  test_fail(choose(pass('a'), construct<CH1>(pass('b'))), "c", 1, {{"a", {0, 1}}, {"b", {0, 2}}});
}

} // namespace ooze::pc
