#pragma once

#include "pch.h"

#include <boost/test/unit_test.hpp>

#define check_result(expr)                                                                                             \
  [](auto r) {                                                                                                         \
    if(!r.has_value()) {                                                                                               \
      fmt::print("Error: {}\n", knot::debug(r.error()));                                                               \
    }                                                                                                                  \
    BOOST_REQUIRE(r.has_value());                                                                                      \
    return std::move(*r);                                                                                              \
  }(expr)

#define check_void_result(expr)                                                                                        \
  [](auto r) {                                                                                                         \
    if(!r.has_value()) {                                                                                               \
      fmt::print("Error: {}\n", knot::debug(r.error()));                                                               \
    }                                                                                                                  \
    BOOST_REQUIRE(r.has_value());                                                                                      \
  }(expr)

#define check_error(expr)                                                                                              \
  [](auto r) {                                                                                                         \
    BOOST_REQUIRE(!r.has_value());                                                                                     \
    return std::move(r.error());                                                                                       \
  }(expr)

#define check_range(expected, actual)                                                                                  \
  [](const auto& e, const auto& a) {                                                                                   \
    BOOST_REQUIRE_EQUAL(std::distance(e.begin(), e.end()), std::distance(a.begin(), a.end()));                         \
    for(int i = 0; i < int(e.size()); ++i) BOOST_CHECK_EQUAL(e[i], a[i]);                                              \
  }(expected, actual)
