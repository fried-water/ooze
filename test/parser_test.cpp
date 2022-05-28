#include "pch.h"

#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze::ast {

BOOST_AUTO_TEST_CASE(parser_empty) { BOOST_CHECK(AST{} == parse("")); }

BOOST_AUTO_TEST_CASE(parser_basic) {
  const std::string_view script = "fn f() -> T { 5 }";

  AST expected;
  expected.push_back({"f", {}, {{"T"}}, {}, Expr{Literal{5}}});

  BOOST_CHECK(expected == parse(script));
}

} // namespace ooze::ast
