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

#define check_error(expr)                                                                                              \
  [](auto r) {                                                                                                         \
    BOOST_REQUIRE(!r.has_value());                                                                                     \
    return std::move(r.error());                                                                                       \
  }(expr)
