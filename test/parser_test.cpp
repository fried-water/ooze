#include "pch.h"

#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze::ast {

Expr call(std::string name, std::vector<Expr> args = {}) {
  return {Indirect<Call>{Call{std::move(name), std::move(args)}}};
}

Expr ident(std::string name) { return {std::move(name)}; }

static const Expr One{Literal{1}};

BOOST_AUTO_TEST_CASE(parser_empty) { BOOST_CHECK(AST{} == parse("")); }

BOOST_AUTO_TEST_CASE(parser_simple) {
  const std::string_view script = "fn f() -> T { 1 }";
  const AST expected{{"f", {}, {{"T"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_fail) {
  BOOST_CHECK(!parse("fn f() -> T {}"));
  BOOST_CHECK(!parse("fn f() -> { 1 }"));
  BOOST_CHECK(!parse("fn f() { 1 }"));
  BOOST_CHECK(!parse("fn f -> T { 1 }"));
  BOOST_CHECK(!parse("fn () -> T { 1 }"));
  BOOST_CHECK(!parse("fn 1() -> T { 1 }"));
  BOOST_CHECK(!parse("f() -> T { 1 }"));
  BOOST_CHECK(!parse("fn f() -> T"));
  BOOST_CHECK(!parse("fn f( -> T { 1 }"));
  BOOST_CHECK(!parse("fn f) -> T { 1 }"));
  BOOST_CHECK(!parse("fn f(a) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_one_arg) {
  const std::string_view script = "fn f(a: X) -> T { 1 }";
  const AST expected{{"f", {{"a", "X"}}, {{"T"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_two_args) {
  const std::string_view script = "fn f(a: X, b: Y) -> T { 1 }";
  const AST expected{{"f", {{"a", "X"}, {"b", "Y"}}, {{"T"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_borrow_args) {
  const std::string_view script = "fn f(a: &X) -> T { 1 }";
  const AST expected{{"f", {{"a", "X", true}}, {{"T"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_nested_expr) {
  const std::string_view script = "fn f() -> T { f(f(1, a), b) }";
  const AST expected{{"f", {}, {{"T"}}, {}, {call("f", {call("f", {One, ident("a")}), ident("b")})}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return) {
  const std::string_view script = "fn f() -> (T) { 1 }";
  const AST expected{{"f", {}, {{"T"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_types) {
  const std::string_view script = "fn f() -> (T1, T2) { 1 }";
  const AST expected{{"f", {}, {{"T1"}, {"T2"}}, {}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_values) {
  const std::string_view script = "fn f() -> T { (1, 1) }";
  const AST expected{{"f", {}, {{"T"}}, {}, {One, One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_binding_no_type) {
  const std::string_view script = "fn f() -> T { let a = f() 1 }";
  const AST expected{{"f", {}, {{"T"}}, {{{{"a"}}, call("f")}}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_binding_type) {
  const std::string_view script = "fn f() -> T { let a: X = f() 1 }";
  const AST expected{{"f", {}, {{"T"}}, {{{{"a", "X"}}, call("f")}}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_multi_binding) {
  const std::string_view script = "fn f() -> T { let (a: X, b: Y, c) = f() 1 }";
  const AST expected{{"f", {}, {{"T"}}, {{{{"a", "X"}, {"b", "Y"}, {"c"}}, call("f")}}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_two_bindings) {
  const std::string_view script = "fn f() -> T { let a = f() let b = abc 1 }";
  const AST expected{{"f", {}, {{"T"}}, {{{{"a"}}, call("f")}, {{{"b"}}, ident("abc")}}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

BOOST_AUTO_TEST_CASE(parser_empty_binding) {
  const std::string_view script = "fn f() -> T { let () = abc 1 }";
  const AST expected{{"f", {}, {{"T"}}, {{{}, ident("abc")}}, {One}}};
  BOOST_CHECK(expected == parse(script));
}

} // namespace ooze::ast
