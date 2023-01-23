#include "pch.h"

#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze::ast {

namespace {

void parse_error(std::string_view src, ContextualError expected) {
  const auto result = parse(src);

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

BOOST_AUTO_TEST_CASE(parser_empty) { BOOST_CHECK(AST{} == parse("")); }

BOOST_AUTO_TEST_CASE(parser_simple) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {one(14)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_long_name) {
  const AST expected{{"abcdefghijklmnopqrstuvwxyz", {{}, {{{"T", {35, 36}}}}, {29, 36}}, {{}, {one(39)}}}};
  BOOST_CHECK(expected == parse("fn abcdefghijklmnopqrstuvwxyz() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multiple_functions) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {one(14)}}},
                     {"f", {{}, {{{"T", {28, 29}}}}, {22, 29}}, {{}, {one(32)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 } fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_one_arg) {
  const AST expected{{"f", {{{"a", {"X", {8, 9}}, false, {5, 9}}}, {{{"T", {14, 15}}}}, {4, 15}}, {{}, {one(18)}}}};
  BOOST_CHECK(expected == parse("fn f(a: X) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_two_args) {
  const AST expected{
    {"f",
     {{{"a", {"X", {8, 9}}, false, {5, 9}}, {"b", {"Y", {14, 15}}, false, {11, 15}}}, {{{"T", {20, 21}}}}, {4, 21}},
     {{}, {one(24)}}}};
  BOOST_CHECK(expected == parse("fn f(a: X, b: Y) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_borrow_args) {
  const AST expected{{"f", {{{"a", {"X", {9, 10}}, true, {5, 10}}}, {{{"T", {15, 16}}}}, {4, 16}}, {{}, {one(19)}}}};
  BOOST_CHECK(expected == parse("fn f(a: &X) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_nested_expr) {
  const AST expected{{"f",
                      {{}, {{{"T", {10, 11}}}}, {4, 11}},
                      {{}, {call("f", {14, 27}, {call("f", {16, 23}, {one(18), ident("a", 21)}), ident("b", 25)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { f(f(1, a), b) }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return) {
  const AST expected{{"f", {{}, {{{"T", {11, 12}}}}, {4, 13}}, {{}, {one(16)}}}};
  BOOST_CHECK(expected == parse("fn f() -> (T) { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_types) {
  const AST expected{{"f", {{}, {{"T1", {11, 13}}, {"T2", {15, 17}}}, {4, 18}}, {{}, {one(21)}}}};
  BOOST_CHECK(expected == parse("fn f() -> (T1, T2) { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_values) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {one(15), one(18)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { (1, 1) }"));
}

BOOST_AUTO_TEST_CASE(parser_binding_no_type) {
  const AST expected{
    {"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{{{{"a", {}, {18, 19}}}, call("f", {22, 25})}}, {one(26)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_binding_type) {
  const AST expected{{"f",
                      {{}, {{{"T", {10, 11}}}}, {4, 11}},
                      {{{{{"a", {{"X", {21, 22}}}, {18, 22}}}, call("f", {25, 28})}}, {one(29)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a: X = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multi_binding) {
  const AST expected{{"f",
                      {{}, {{{"T", {10, 11}}}}, {4, 11}},
                      {{{{{"a", {{"X", {22, 23}}}, {19, 23}}, {"b", {{"Y", {28, 29}}}, {25, 29}}, {"c", {}, {31, 32}}},
                         call("f", {36, 39})}},
                       {one(40)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let (a: X, b: Y, c) = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_two_bindings) {
  const AST expected{
    {"f",
     {{}, {{{"T", {10, 11}}}}, {4, 11}},
     {{{{{"a", {}, {18, 19}}}, call("f", {22, 25})}, {{{"b", {}, {30, 31}}}, ident("abc", 34)}}, {one(38)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a = f() let b = abc 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_empty_binding) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{{{}, ident("abc", 23)}}, {one(27)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let () = abc 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {call("b", {14, 19}, {ident("a", 14)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a.b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_literal) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {call("b", {14, 19}, {one(14)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1.b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_function) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {call("b", {14, 21}, {call("a", {14, 17})})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a().b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_multi_parameter) {
  const AST expected{{"f", {{}, {{{"T", {10, 11}}}}, {4, 11}}, {{}, {call("b", {14, 20}, {ident("a", 14), one(18)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a.b(1) }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn) { parse_error("f", {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn2) { parse_error("a", {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn3) { parse_error(")", {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_bad_paren) { parse_error("fn)", {{2, 3}, "expected token" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_expr) { parse_error("fn f() -> T {}", {{13, 14}, "expected 'let'" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_return_type) { parse_error("fn f() -> { 1 }", {{10, 11}, "expected '('" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_return_arrow) { parse_error("fn f() { 1 }", {{7, 8}, "expected '->'" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_params) { parse_error("fn f -> T { 1 }", {{5, 7}, "expected '('" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_fn_name) { parse_error("fn () -> T { 1 }", {{3, 4}, "expected token" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_bad_fn_name) { parse_error("fn 1() -> T { 1 }", {{3, 4}, "expected token" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_no_fn_keyword) { parse_error("f() -> T { 1 }", {{0, 1}, "expected 'fn'"}); }

BOOST_AUTO_TEST_CASE(parser_no_scope) { parse_error("fn f() -> T", {{11, 11}, "expected '{'"}); }

BOOST_AUTO_TEST_CASE(parser_unclosed_paren) { parse_error("fn f( -> T { 1 }", {{6, 8}, "expected ')'"}); }

BOOST_AUTO_TEST_CASE(parser_unopened_paren) { parse_error("fn f) -> T { 1 }", {{4, 5}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_untyped_paren) { parse_error("fn f(a) -> T { 1 }", {{6, 7}, "expected ':'"}); }

BOOST_AUTO_TEST_CASE(parser_untyped_paren2) { parse_error("fn f(a : ) -> T { 1 }", {{9, 10}, "expected '&'" /* ? */}); }

BOOST_AUTO_TEST_CASE(parser_bad_type_paren) {
  parse_error("fn f(a : 1) -> T { 1 }", {{9, 10}, "expected '&'" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_expr_unclosed) { parse_error("fn f() -> T { a( }", {{17, 18}, "expected ')'"}); }

BOOST_AUTO_TEST_CASE(parser_expr_unopened) { parse_error("fn f() -> T { a) }", {{15, 16}, "expected '}'"}); }

BOOST_AUTO_TEST_CASE(parser_expr_bad_comma) {
  parse_error("fn f() -> T { a(1,) }", {{18, 19}, "expected literal" /* ? */});
}

BOOST_AUTO_TEST_CASE(parser_bad_chain) { parse_error("fn f() -> T { a.b }", {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_bad_chain2) { parse_error("fn f() -> T { a.1 }", {{16, 17}, "expected token"}); }

BOOST_AUTO_TEST_CASE(parser_bad_assignment) { parse_error("fn f() -> T { let }", {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_let_no_var) { parse_error("fn f() -> T { let = 1 }", {{18, 19}, "expected '('"}); }

BOOST_AUTO_TEST_CASE(parser_let_no_expr) { parse_error("fn f() -> T { let x = }", {{22, 23}, "expected literal"}); }

BOOST_AUTO_TEST_CASE(parser_assignment_no_expr) {
  parse_error("fn f() -> T { let x = 0 }", {{24, 25}, "expected 'let'"});
}

BOOST_AUTO_TEST_CASE(parser_bad_second_fn) { parse_error("fn f() -> T { 1 } fn", {{20, 20}, "expected token"}); }

} // namespace ooze::ast
