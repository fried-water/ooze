#pragma once

#include "pch.h"

#include <boost/test/unit_test.hpp>

#define BOOST_CHECK_RANGE_EQUAL(r1, r2)                                                                                \
  do {                                                                                                                 \
    auto&& _r1 = (r1);                                                                                                 \
    auto&& _r2 = (r2);                                                                                                 \
    BOOST_CHECK_EQUAL_COLLECTIONS(_r1.begin(), _r1.end(), _r2.begin(), _r2.end());                                     \
  } while(0)

#define check_result(expr)                                                                                             \
  [](auto r) {                                                                                                         \
    if(!r.has_value()) fmt::print("Error: {}\n", knot::debug(r.error()));                                              \
    BOOST_REQUIRE(r.has_value());                                                                                      \
    return unwrap_1tuple(std::move(r).value_and_state());                                                              \
  }(expr)

#define check_result_value(expr)                                                                                       \
  [](auto r) {                                                                                                         \
    if(!r.has_value()) fmt::print("Error: {}\n", knot::debug(r.error()));                                              \
    BOOST_REQUIRE(r.has_value());                                                                                      \
    return std::move(*r);                                                                                              \
  }(expr)

#define check_error(expr)                                                                                              \
  [](auto r) {                                                                                                         \
    BOOST_REQUIRE(!r.has_value());                                                                                     \
    return std::move(r.error());                                                                                       \
  }(expr)

#define check_error_state(expr)                                                                                        \
  [](auto r) {                                                                                                         \
    BOOST_REQUIRE(!r.has_value());                                                                                     \
    return std::move(r).error_and_state();                                                                             \
  }(expr)

#define check_range(expected, actual)                                                                                  \
  [](const auto& e, const auto& a) {                                                                                   \
    BOOST_REQUIRE_EQUAL(std::distance(e.begin(), e.end()), std::distance(a.begin(), a.end()));                         \
    for(int i = 0; i < int(e.size()); ++i) BOOST_CHECK_EQUAL(e[i], a[i]);                                              \
  }(expected, actual)

#define check_any(expected, actual)                                                                                    \
  [](const auto& e, const Any& a) {                                                                                    \
    BOOST_REQUIRE(holds_alternative<std::decay_t<decltype(e)>>(a));                                                    \
    BOOST_CHECK(any_cast<std::decay_t<decltype(e)>>(a) == e);                                                          \
  }(expected, actual)

struct Sentinal {
  int copies = 0;
  int moves = 0;

  Sentinal() = default;

  Sentinal(const Sentinal& x) : copies(x.copies + 1), moves(x.moves) {}
  Sentinal(Sentinal&& x) : copies(x.copies), moves(x.moves + 1) {}

  Sentinal& operator=(const Sentinal& x) {
    copies = x.copies + 1;
    moves = x.moves;
    return *this;
  }

  Sentinal& operator=(Sentinal&& x) {
    copies = x.copies;
    moves = x.moves + 1;
    return *this;
  }
};
