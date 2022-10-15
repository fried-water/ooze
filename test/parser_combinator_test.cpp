#include "pch.h"

#include "parser_combinators.h"

#include <boost/test/unit_test.hpp>

namespace ooze::pc {

namespace {

auto ch(char c) { return constant(std::string() + c, c); }
auto pass(char c) {
  return filter(any(), std::string() + c, [=](int x, char c2) { return c == c2; });
}

template <typename R, typename P>
auto test(P parser, std::string_view str, ParseResult<R> expected) {
  const auto actual = parser(ParseState<int, char>{0, Span<char>(str.data(), str.size())}, {});

  if(actual != expected) {
    fmt::print("E {} {}\n", expected.value.has_value(), knot::debug(expected));
    fmt::print("A {} {}\n", actual.value.has_value(), knot::debug(actual));
  }

  BOOST_CHECK(expected == actual);
}

} // namespace

BOOST_AUTO_TEST_CASE(pc_constant) {
  test(ch('a'), "a", passing_result({0, 1}, std::tuple()));
  test(ch('a'), "ab", passing_result({0, 1}, std::tuple()));
  test(ch('a'), "abc", passing_result({0, 1}, std::tuple()));

  test(ch('a'), "b", failing_result<std::tuple<>>({0, 0}, {{"a", {0, 1}}}));
  test(ch('a'), "", failing_result<std::tuple<>>({0, 0}, {{"a", {0, 1}}}));
}

BOOST_AUTO_TEST_CASE(pc_any) {
  test(any(), "a", passing_result({0, 1}, 'a'));
  test(any(), "b", passing_result({0, 1}, 'b'));
  test(any(), "ab", passing_result({0, 1}, 'a'));

  test(any(), "", failing_result<char>({0, 0}, {{"anything", {0, 0}}}));
}

BOOST_AUTO_TEST_CASE(pc_filter) {
  test(pass('a'), "a", passing_result({0, 1}, 'a'));
  test(pass('b'), "b", passing_result({0, 1}, 'b'));
  test(pass('a'), "ab", passing_result({0, 1}, 'a'));

  test(pass('a'), "", failing_result<char>({0, 0}, {{"a", {0, 0}}}));
  test(pass('a'), "b", failing_result<char>({0, 0}, {{"a", {0, 0}}}));
}

BOOST_AUTO_TEST_CASE(pc_maybe) {
  test(maybe(ch('a')), "a", passing_result({0, 1}, std::optional(std::tuple())));
  test(maybe(ch('a')), "ab", passing_result({0, 1}, std::optional(std::tuple())));
  test(maybe(ch('a')), "abc", passing_result({0, 1}, std::optional(std::tuple())));

  test(maybe(ch('a')), "", passing_result<std::optional<std::tuple<>>>({0, 0}, std::nullopt, {{"a", {0, 2}}}));
  test(maybe(ch('a')), "b", passing_result<std::optional<std::tuple<>>>({0, 0}, std::nullopt, {{"a", {0, 2}}}));
}

BOOST_AUTO_TEST_CASE(pc_n) {
  test(n(ch('a')), "", passing_result({0, 0}, std::vector<std::tuple<>>{}, {{"a", {0, 2}}}));
  test(n(ch('a')), "a", passing_result({0, 1}, std::vector<std::tuple<>>{{}}, {{"a", {1, 2}}}));
  test(n(ch('a')), "aa", passing_result({0, 2}, std::vector<std::tuple<>>{{}, {}}, {{"a", {2, 2}}}));
  test(n(ch('a')), "aab", passing_result({0, 2}, std::vector<std::tuple<>>{{}, {}}, {{"a", {2, 2}}}));
  test(n(ch('a')), "b", passing_result({0, 0}, std::vector<std::tuple<>>{}, {{"a", {0, 2}}}));
}

BOOST_AUTO_TEST_CASE(pc_seq) {
  test(seq(), "", passing_result({0, 0}, std::tuple()));
  test(seq(), "a", passing_result({0, 0}, std::tuple()));

  test(seq(ch('a')), "a", passing_result({0, 1}, std::tuple()));
  test(seq(ch('a'), ch('b')), "ab", passing_result({0, 2}, std::tuple()));

  test(seq(pass('a'), ch('b')), "ab", passing_result({0, 2}, 'a'));
  test(seq(ch('a'), pass('b')), "ab", passing_result({0, 2}, 'b'));
  test(seq(pass('a'), pass('b')), "ab", passing_result({0, 2}, std::tuple('a', 'b')));

  test(seq(pass('a'), pass('b'), pass('c')), "abc", passing_result({0, 3}, std::tuple('a', 'b', 'c')));
  test(seq(ch('a'), pass('b'), ch('c')), "abc", passing_result({0, 3}, 'b'));
  test(seq(pass('a'), ch('b'), pass('c')), "abc", passing_result({0, 3}, std::tuple('a', 'c')));

  test(seq(ch('a')), "", failing_result<std::tuple<>>({0, 0}, {{"a", {0, 2}}}));
  test(seq(ch('a')), "b", failing_result<std::tuple<>>({0, 0}, {{"a", {0, 2}}}));
  test(seq(ch('a'), ch('b')), "a", failing_result<std::tuple<>>({0, 1}, {{"b", {1, 2}}}));
  test(seq(ch('a'), ch('b')), "aa", failing_result<std::tuple<>>({0, 1}, {{"b", {1, 2}}}));

  test(seq(maybe(ch('a'))), "", passing_result<std::optional<std::tuple<>>>({0, 0}, {}, {{"a", {0, 3}}}));
  test(seq(maybe(ch('a'))), "a", passing_result({0, 1}, std::optional(std::tuple())));

  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "c",
       passing_result({0, 1},
                      std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>(std::nullopt, std::nullopt),
                      {{"a", {0, 3}}, {"b", {0, 3}}}));
  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "bc",
       passing_result({0, 2},
                      std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>(std::nullopt, std::tuple()),
                      {{"a", {0, 3}}}));
  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "ac",
       passing_result({0, 2},
                      std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>(std::tuple(), std::nullopt),
                      {{"b", {1, 3}}}));

  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "a",
       failing_result<std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>>(
         {0, 1}, {{"b", {1, 3}}, {"c", {1, 2}}}));
  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "b",
       failing_result<std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>>(
         {0, 1}, {{"a", {0, 3}}, {"c", {1, 2}}}));
  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "",
       failing_result<std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>>(
         {0, 0}, {{"a", {0, 3}}, {"b", {0, 3}}, {"c", {0, 2}}}));
  test(seq(maybe(ch('a')), maybe(ch('b')), ch('c')),
       "d",
       failing_result<std::tuple<std::optional<std::tuple<>>, std::optional<std::tuple<>>>>(
         {0, 0}, {{"a", {0, 3}}, {"b", {0, 3}}, {"c", {0, 2}}}));
}

struct CH1 {
  char c;
  KNOT_COMPAREABLE(CH1);
};

BOOST_AUTO_TEST_CASE(pc_choose) {
  test(choose(pass('a')), "a", passing_result({0, 1}, 'a'));
  test(choose(pass('a'), pass('b')), "b", passing_result({0, 1}, 'b', {{"a", {0, 1}}}));
  test(choose(pass('a'), construct<CH1>(pass('b'))),
       "b",
       passing_result({0, 1}, std::variant<char, CH1>(CH1{'b'}), {{"a", {0, 1}}}));
  test(
    choose(pass('a'), pass('b'), construct<CH1>(pass('c'))), "a", passing_result({0, 1}, std::variant<char, CH1>('a')));

  test(choose(pass('a'), construct<CH1>(pass('b'))),
       "c",
       failing_result<std::variant<char, CH1>>({0, 0}, {{"a", {0, 1}}, {"b", {0, 2}}}));
}

} // namespace ooze::pc
