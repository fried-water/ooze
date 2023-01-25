#include "pch.h"

#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze::ast {

namespace {

template <typename T>
void check_error(ContextualResult<T> result, ContextualError expected) {
  BOOST_REQUIRE(!result);
  BOOST_REQUIRE(result.error().size() == 1);

  if(expected != result.error().front()) {
    fmt::print("Actual:   {}\n", knot::debug(result.error().front()));
    fmt::print("Expected: {}\n", knot::debug(expected));
  }

  BOOST_CHECK(expected == result.error().front());
}

UnTypedExpr call(std::string name, Slice s, std::vector<UnTypedExpr> args = {}) {
  return {Call<NamedFunction>{std::move(name), std::move(args)}, s};
}

UnTypedExpr ident(std::string_view name, u32 offset) {
  return {std::string(name), {offset, offset + (u32)name.size()}};
}

UnTypedScope scope(UnTypedExpr expr) { return {{}, {std::move(expr)}}; }

UnTypedExpr one(u32 offset) { return {Literal{1}, Slice{offset, offset + 1}}; }

const UnTypedExpr One{Literal{1}};

} // namespace

// Expr tests

BOOST_AUTO_TEST_CASE(parser_literal) { BOOST_CHECK(one(0) == parse_expr("1")); }

BOOST_AUTO_TEST_CASE(parser_ident) { BOOST_CHECK(ident("abc", 0) == parse_expr("abc")); }

BOOST_AUTO_TEST_CASE(parser_call) { BOOST_CHECK(call("f", {0, 3}, {}) == parse_expr("f()")); }

BOOST_AUTO_TEST_CASE(parser_call_one_arg) { BOOST_CHECK(call("f", {0, 4}, {one(2)}) == parse_expr("f(1)")); }

BOOST_AUTO_TEST_CASE(parser_call_two_arg) {
  BOOST_CHECK(call("f", {0, 7}, {one(2), ident("a", 5)}) == parse_expr("f(1, a)"));
}

BOOST_AUTO_TEST_CASE(parser_nested_call) {
  const UnTypedExpr expected = call("f", {0, 13}, {call("f", {2, 9}, {one(4), ident("a", 7)}), ident("b", 11)});
  BOOST_CHECK(expected == parse_expr("f(f(1, a), b)"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs) {
  const UnTypedExpr expected = call("b", {0, 5}, {ident("a", 0)});
  BOOST_CHECK(expected == parse_expr("a.b()"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_literal) {
  const UnTypedExpr expected = call("b", {0, 5}, {one(0)});
  BOOST_CHECK(expected == parse_expr("1.b()"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_function) {
  const UnTypedExpr expected = call("b", {0, 7}, {call("a", {0, 3})});
  BOOST_CHECK(expected == parse_expr("a().b()"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_multi_parameter) {
  const UnTypedExpr expected = call("b", {0, 6}, {ident("a", 0), one(4)});
  BOOST_CHECK(expected == parse_expr("a.b(1)"));
}

// Assignment tests

BOOST_AUTO_TEST_CASE(parser_simple_assignment) {
  const UnTypedAssignment expected{{{"a", {}, {4, 5}}}, one(8)};
  BOOST_CHECK(expected == parse_assignment("let a = 1"));
}

BOOST_AUTO_TEST_CASE(parser_typed_assignment) {
  const UnTypedAssignment expected{{{"a", {{"X", {7, 8}}}, {4, 8}}}, one(11)};
  BOOST_CHECK(expected == parse_assignment("let a: X = 1"));
}

BOOST_AUTO_TEST_CASE(parser_multi_assignment) {
  const UnTypedAssignment expected{{{"a", {}, {5, 6}}, {"b", {}, {8, 9}}}, one(13)};
  BOOST_CHECK(expected == parse_assignment("let (a, b) = 1"));
}

BOOST_AUTO_TEST_CASE(parser_multi_typed_assignment) {
  const UnTypedAssignment expected{{{"a", {{"X", {8, 9}}}, {5, 9}}, {"b", {{"Y", {14, 15}}}, {11, 15}}}, one(19)};
  BOOST_CHECK(expected == parse_assignment("let (a: X, b: Y) = 1"));
}

BOOST_AUTO_TEST_CASE(parser_empty_assignment) {
  const UnTypedAssignment expected{{}, one(9)};
  BOOST_CHECK(expected == parse_assignment("let () = 1"));
}

// Header tests

BOOST_AUTO_TEST_CASE(parser_no_args) {
  const UnTypedHeader expected{{}, {{{"T", {6, 7}}}}, {0, 7}};
  BOOST_CHECK(expected == parse_header("() -> T"));
}

BOOST_AUTO_TEST_CASE(parser_one_arg) {
  const UnTypedHeader expected{{{"a", {"X", {4, 5}}, false, {1, 5}}}, {{{"T", {10, 11}}}}, {0, 11}};
  BOOST_CHECK(expected == parse_header("(a: X) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_two_args) {
  const UnTypedHeader expected{
    {{"a", {"X", {4, 5}}, false, {1, 5}}, {"b", {"Y", {10, 11}}, false, {7, 11}}}, {{{"T", {16, 17}}}}, {0, 17}};
  BOOST_CHECK(expected == parse_header("(a: X, b: Y) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_borrow_arg) {
  const UnTypedHeader expected{{{"a", {"X", {5, 6}}, true, {1, 6}}}, {{{"T", {11, 12}}}}, {0, 12}};
  BOOST_CHECK(expected == parse_header("(a: &X) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_empty_tuple_return) {
  const UnTypedHeader expected{{}, {}, {0, 8}};
  BOOST_CHECK(expected == parse_header("() -> ()"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return) {
  const UnTypedHeader expected{{}, {{{"T", {7, 8}}}}, {0, 9}};
  BOOST_CHECK(expected == parse_header("() -> (T)"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_types) {
  const UnTypedHeader expected{{}, {{"T1", {7, 9}}, {"T2", {11, 13}}}, {0, 14}};
  BOOST_CHECK(expected == parse_header("() -> (T1, T2)"));
}

// Scope tests

BOOST_AUTO_TEST_CASE(parser_scope_simple) {
  const UnTypedScope expected{{}, {one(2)}};
  BOOST_CHECK(expected == parse_scope("{ 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment) {
  const UnTypedScope expected{{{{{"x", {}, {6, 7}}}, one(10)}}, {one(12)}};
  BOOST_CHECK(expected == parse_scope("{ let x = 1 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment2) {
  const UnTypedScope expected{{{{{"x", {}, {6, 7}}}, one(10)}, {{{"y", {}, {16, 17}}}, one(20)}}, {one(22)}};
  BOOST_CHECK(expected == parse_scope("{ let x = 1 let y = 1 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_tuple) {
  const UnTypedScope expected{{}, {one(3)}};
  BOOST_CHECK(expected == parse_scope("{ (1) }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_multi_tuple) {
  const UnTypedScope expected{{}, {one(3), one(6)}};
  BOOST_CHECK(expected == parse_scope("{ (1, 1) }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_empty_tuple) { BOOST_CHECK(UnTypedScope{} == parse_scope("{ () }")); }

// AST tests

BOOST_AUTO_TEST_CASE(parser_empty) { BOOST_CHECK(AST{} == parse("")); }

BOOST_AUTO_TEST_CASE(parser_simple) {
  const AST expected{{"f", {{{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {one(14)}}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_long_name) {
  const AST expected{{"abcdefghijklmnopqrstuvwxyz", {{{}, {{{"T", {35, 36}}}}, {29, 36}}, {{}, {one(39)}}}}};
  BOOST_CHECK(expected == parse("fn abcdefghijklmnopqrstuvwxyz() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multiple_functions) {
  const AST expected{{"f", {{{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {one(14)}}}},
                     {"f", {{{}, {{{"T", {28, 29}}}}, {22, 29}}, {{}, {one(32)}}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 } fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn) { check_error(parse("f"), {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn2) { check_error(parse("a"), {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn3) { check_error(parse(")"), {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_bad_paren) { check_error(parse("fn)"), {{2, 3}, "expected token" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_expr) { check_error(parse_scope("{}"), {{1, 2}, "expected 'let'" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_return_type) {
  check_error(parse_header("() -> { 1 }"), {{6, 7}, "expected '('" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_no_return) { check_error(parse("fn f() { 1 }"), {{7, 8}, "expected '->'" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_params) { check_error(parse("fn f -> T { 1 }"), {{5, 7}, "expected '('" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_fn_name) { check_error(parse("fn () -> T { 1 }"), {{3, 4}, "expected token" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_bad_fn_name) {
  check_error(parse("fn 1() -> T { 1 }"), {{3, 4}, "expected token" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_no_fn_keyword) { check_error(parse("f() -> T { 1 }"), {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_scope) { check_error(parse("fn f() -> T"), {{11, 11}, "expected '{'"}); }

BOOST_AUTO_TEST_CASE(parser_unclosed_paren) { check_error(parse("fn f( -> T { 1 }"), {{6, 8}, "expected ')'"}); }

BOOST_AUTO_TEST_CASE(parser_unopened_paren) { check_error(parse("fn f) -> T { 1 }"), {{4, 5}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_untyped_paren) { check_error(parse("fn f(a) -> T { 1 }"), {{6, 7}, "expected ':'"}); }

BOOST_AUTO_TEST_CASE(parser_untyped_paren2) {
  check_error(parse("fn f(a : ) -> T { 1 }"), {{9, 10}, "expected '&'" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_bad_type_paren) {
  check_error(parse("fn f(a : 1) -> T { 1 }"), {{9, 10}, "expected '&'" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_expr_unclosed) { check_error(parse("fn f() -> T { a( }"), {{17, 18}, "expected ')'"}); }

BOOST_AUTO_TEST_CASE(parser_expr_unopened) { check_error(parse("fn f() -> T { a) }"), {{15, 16}, "expected '}'"}); }

BOOST_AUTO_TEST_CASE(parser_expr_bad_comma) {
  check_error(parse("fn f() -> T { a(1,) }"), {{18, 19}, "expected literal" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_bad_chain) { check_error(parse("fn f() -> T { a.b }"), {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_bad_chain2) { check_error(parse("fn f() -> T { a.1 }"), {{16, 17}, "expected token"}); }

BOOST_AUTO_TEST_CASE(parser_bad_assignment) { check_error(parse("fn f() -> T { let }"), {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_let_no_var) { check_error(parse("fn f() -> T { let = 1 }"), {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_let_no_expr) {
  check_error(parse("fn f() -> T { let x = }"), {{22, 23}, "expected literal"});
}

BOOST_AUTO_TEST_CASE(parser_assignment_no_expr) {
  check_error(parse("fn f() -> T { let x = 0 }"), {{24, 25}, "expected 'let'"});
}

BOOST_AUTO_TEST_CASE(parser_bad_second_fn) { check_error(parse("fn f() -> T { 1 } fn"), {{20, 20}, "expected token"}); }

} // namespace ooze::ast
