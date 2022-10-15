#include "pch.h"

#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze::ast {

namespace {

void parse_error(std::string_view src, std::vector<std::string> expected) {
  const auto result = parse(src);

  BOOST_REQUIRE(!result);

  if(expected != result.error()) {
    fmt::print("Actual\n{}\n", knot::debug(result.error()));
    fmt::print("Expected\n{}\n", knot::debug(expected));
  }

  BOOST_CHECK(expected == result.error());
}

UnTypedExpr call(std::string name, Slice s, std::vector<UnTypedExpr> args = {}) {
  return {Call<NamedFunction>{std::move(name), std::move(args)}, s};
}

UnTypedExpr ident(std::string_view name, u32 offset) {
  return {std::string(name), {offset, offset + (u32)name.size()}};
}

UnTypedBody body(UnTypedExpr expr) { return {{}, {std::move(expr)}}; }

UnTypedExpr one(u32 offset) { return {Literal{1}, Slice{offset, offset + 1}}; }

const UnTypedExpr One{Literal{1}};

} // namespace

BOOST_AUTO_TEST_CASE(parser_empty) { BOOST_CHECK(AST{} == parse("")); }

BOOST_AUTO_TEST_CASE(parser_simple) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {one(14)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multiple_functions) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {one(14)}}}, {"f", {{}, {{{"T"}}}}, {{}, {one(32)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1 } fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_one_arg) {
  const AST expected{{"f", {{{"a", {"X"}}}, {{{"T"}}}}, {{}, {one(18)}}}};
  BOOST_CHECK(expected == parse("fn f(a: X) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_two_args) {
  const AST expected{{"f", {{{"a", {"X"}}, {"b", {"Y"}}}, {{{"T"}}}}, {{}, {one(24)}}}};
  BOOST_CHECK(expected == parse("fn f(a: X, b: Y) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_borrow_args) {
  const AST expected{{"f", {{{"a", {"X"}, true}}, {{{"T"}}}}, {{}, {one(19)}}}};
  BOOST_CHECK(expected == parse("fn f(a: &X) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_nested_expr) {
  const AST expected{{"f",
                      {{}, {{{"T"}}}},
                      {{}, {call("f", {14, 27}, {call("f", {16, 23}, {one(18), ident("a", 21)}), ident("b", 25)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { f(f(1, a), b) }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {one(16)}}}};
  BOOST_CHECK(expected == parse("fn f() -> (T) { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_types) {
  const AST expected{{"f", {{}, {{"T1"}, {"T2"}}}, {{}, {one(21)}}}};
  BOOST_CHECK(expected == parse("fn f() -> (T1, T2) { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_return_two_values) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {one(15), one(18)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { (1, 1) }"));
}

BOOST_AUTO_TEST_CASE(parser_binding_no_type) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{{{{"a"}}, call("f", {22, 25})}}, {one(26)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_binding_type) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{{{{"a", {{"X"}}}}, call("f", {25, 28})}}, {one(29)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a: X = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multi_binding) {
  const AST expected{
    {"f", {{}, {{{"T"}}}}, {{{{{"a", {{"X"}}}, {"b", {{"Y"}}}, {"c"}}, call("f", {36, 39})}}, {one(40)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let (a: X, b: Y, c) = f() 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_two_bindings) {
  const AST expected{
    {"f", {{}, {{{"T"}}}}, {{{{{"a"}}, call("f", {22, 25})}, {{{"b"}}, ident("abc", 34)}}, {one(38)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let a = f() let b = abc 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_empty_binding) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{{{}, ident("abc", 23)}}, {one(27)}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { let () = abc 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {call("b", {14, 19}, {ident("a", 14)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a.b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_literal) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {call("b", {14, 19}, {one(14)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { 1.b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_function) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {call("b", {14, 21}, {call("a", {14, 17})})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a().b() }"));
}

BOOST_AUTO_TEST_CASE(parser_ufcs_multi_parameter) {
  const AST expected{{"f", {{}, {{{"T"}}}}, {{}, {call("b", {14, 20}, {ident("a", 14), one(18)})}}}};
  BOOST_CHECK(expected == parse("fn f() -> T { a.b(1) }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn) { parse_error("f", {"1:0 error: expected 'fn'", " | f", " | ^"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn2) { parse_error("a", {"1:0 error: expected 'fn'", " | a", " | ^"}); }

BOOST_AUTO_TEST_CASE(parser_no_fn3) { parse_error(")", {"1:0 error: expected 'fn'", " | )", " | ^"}); }

BOOST_AUTO_TEST_CASE(parser_bad_paren) {
  parse_error("fn)",
              {"1:2 error: expected token", // ?
               " | fn)",
               " |   ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_expr) {
  parse_error("fn f() -> T {}",
              {"1:13 error: expected 'let'", // ?
               " | fn f() -> T {}",
               " |              ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_return_type) {
  parse_error("fn f() -> { 1 }",
              {"1:10 error: expected '('", // ?
               " | fn f() -> { 1 }",
               " |           ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_return_arrow) {
  parse_error("fn f() { 1 }", {"1:7 error: expected '->'", " | fn f() { 1 }", " |        ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_params) {
  parse_error("fn f -> T { 1 }",
              {
                "1:5 error: expected '('",
                " | fn f -> T { 1 }",
                " |      ^~" // ?
              });
}

BOOST_AUTO_TEST_CASE(parser_no_fn_name) {
  parse_error("fn () -> T { 1 }",
              {"1:3 error: expected token", // ?
               " | fn () -> T { 1 }",
               " |    ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_fn_name) {
  parse_error("fn 1() -> T { 1 }",
              {"1:3 error: expected token", // ?
               " | fn 1() -> T { 1 }",
               " |    ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_fn_keyword) {
  parse_error("f() -> T { 1 }", {"1:0 error: expected 'fn'", " | f() -> T { 1 }", " | ^"});
}

BOOST_AUTO_TEST_CASE(parser_no_body) {
  parse_error("fn f() -> T", {"1:11 error: expected '{'", " | fn f() -> T", " |            ^"});
}

BOOST_AUTO_TEST_CASE(parser_unclosed_paren) {
  parse_error("fn f( -> T { 1 }",
              {"1:6 error: expected ')'", // ?
               " | fn f( -> T { 1 }",
               " |       ^~"});
}

BOOST_AUTO_TEST_CASE(parser_unopened_paren) {
  parse_error("fn f) -> T { 1 }", {"1:4 error: expected '('", " | fn f) -> T { 1 }", " |     ^"});
}

BOOST_AUTO_TEST_CASE(parser_untyped_paren) {
  parse_error("fn f(a) -> T { 1 }", {"1:6 error: expected ':'", " | fn f(a) -> T { 1 }", " |       ^"});
}

BOOST_AUTO_TEST_CASE(parser_untyped_paren2) {
  parse_error("fn f(a : ) -> T { 1 }",
              {"1:9 error: expected '&'", // ?
               " | fn f(a : ) -> T { 1 }",
               " |          ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_type_paren) {
  parse_error("fn f(a : 1) -> T { 1 }",
              {"1:9 error: expected '&'", // ?
               " | fn f(a : 1) -> T { 1 }",
               " |          ^"});
}

BOOST_AUTO_TEST_CASE(parser_expr_unclosed) {
  parse_error("fn f() -> T { a( }", {"1:17 error: expected ')'", " | fn f() -> T { a( }", " |                  ^"});
}

BOOST_AUTO_TEST_CASE(parser_expr_unopened) {
  parse_error("fn f() -> T { a) }", {"1:15 error: expected '}'", " | fn f() -> T { a) }", " |                ^"});
}

BOOST_AUTO_TEST_CASE(parser_expr_bad_comma) {
  parse_error("fn f() -> T { a(1,) }",
              {"1:18 error: expected literal", // ?
               " | fn f() -> T { a(1,) }",
               " |                   ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_chain) {
  parse_error("fn f() -> T { a.b }", {"1:18 error: expected '('", " | fn f() -> T { a.b }", " |                   ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_chain2) {
  parse_error("fn f() -> T { a.1 }",
              {"1:16 error: expected token", // ?
               " | fn f() -> T { a.1 }",
               " |                 ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_assignment) {
  parse_error("fn f() -> T { let }",
              {"1:18 error: expected '('", // ?
               " | fn f() -> T { let }",
               " |                   ^"});
}

BOOST_AUTO_TEST_CASE(parser_let_no_var) {
  parse_error("fn f() -> T { let = 1 }",
              {"1:18 error: expected '('", // ?
               " | fn f() -> T { let = 1 }",
               " |                   ^"});
}

BOOST_AUTO_TEST_CASE(parser_let_no_expr) {
  parse_error("fn f() -> T { let x = }",
              {"1:22 error: expected literal", // ?
               " | fn f() -> T { let x = }",
               " |                       ^"});
}

BOOST_AUTO_TEST_CASE(parser_assignment_no_expr) {
  parse_error("fn f() -> T { let x = 0 }",
              {"1:24 error: expected 'let'", // ?
               " | fn f() -> T { let x = 0 }",
               " |                         ^"});
}

BOOST_AUTO_TEST_CASE(parser_bad_second_fn) {
  parse_error("fn f() -> T { 1 } fn",
              {"1:20 error: expected token", // ?
               " | fn f() -> T { 1 } fn",
               " |                     ^"});
}

} // namespace ooze::ast
