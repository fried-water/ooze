#include "test.h"

#include "parser.h"

namespace ooze::ast {

namespace {

#define check_pass(_EXP, _ACT)                                                                                         \
  [](auto exp, auto result) {                                                                                          \
    BOOST_REQUIRE(result);                                                                                             \
    if(exp != result.value()) {                                                                                        \
      fmt::print("Actual:   {}\n", knot::debug(result.value()));                                                       \
      fmt::print("Expected: {}\n", knot::debug(exp));                                                                  \
      BOOST_CHECK(false);                                                                                              \
    }                                                                                                                  \
  }(_EXP, _ACT)

template <typename T>
void check_single_error(ContextualError expected, ContextualResult<T> result) {
  const std::vector<ContextualError> err = check_error(std::move(result));
  BOOST_REQUIRE(err.size() == 1);

  if(expected != err.front()) {
    fmt::print("Actual:   {}\n", knot::debug(err.front()));
    fmt::print("Expected: {}\n", knot::debug(expected));
    BOOST_CHECK(false);
  }
}

UnTypedExpr call(std::string name, Slice s, std::vector<UnTypedExpr> args = {}) {
  return {Call<NamedFunction>{std::move(name), std::move(args)}, s};
}

UnTypedExpr ident(std::string_view name, u32 offset) {
  return {{IdentExpr{std::string(name)}}, {offset, offset + (u32)name.size()}};
}

UnTypedScope scope(UnTypedExpr expr) { return {{}, {std::move(expr)}}; }

UnTypedExpr one(u32 offset) { return {{Literal{1}}, Slice{offset, offset + 1}}; }

template <typename... Children>
UnTypedExpr expr_tuple(Slice ref, Children... children) {
  return {std::vector<UnTypedExpr>{std::move(children)...}, ref};
}

template <typename... Children>
UnTypedExpr expr_borrow(UnTypedExpr e) {
  return {BorrowExpr<NamedFunction>{std::move(e)}, {e.ref.begin - 1, e.ref.end}};
}

Pattern ident_pattern(std::string_view name, u32 offset) {
  return {{Ident{std::string(name)}}, {offset, offset + (u32)name.size()}};
}

Pattern wildcard_pattern(u32 offset) { return {{WildCard{}}, {offset, offset + 1}}; }

template <typename... Children>
Pattern tuple_pattern(Slice ref, Children... children) {
  return {std::vector<Pattern>{std::move(children)...}, ref};
}

CompoundType<NamedType> type(std::string_view name, u32 offset) {
  return {NamedType{std::string(name), {offset, offset + (u32)name.size()}}};
}

UnTypedHeader
header(Pattern pattern, std::vector<CompoundType<NamedType>> inputs, CompoundType<NamedType> output, Slice ref) {
  return {std::move(pattern), {{{std::move(inputs)}}, std::move(output)}, ref};
}

} // namespace

BOOST_AUTO_TEST_CASE(parser_expr_literal) { check_pass(one(0), parse_expr("1")); }

BOOST_AUTO_TEST_CASE(parser_expr_ident) { check_pass(ident("abc", 0), parse_expr("abc")); }

BOOST_AUTO_TEST_CASE(parser_expr_call) { check_pass(call("f", {0, 3}, {}), parse_expr("f()")); }

BOOST_AUTO_TEST_CASE(parser_expr_call_one_arg) { check_pass(call("f", {0, 4}, {one(2)}), parse_expr("f(1)")); }

BOOST_AUTO_TEST_CASE(parser_call_two_arg) {
  check_pass(call("f", {0, 7}, {one(2), ident("a", 5)}), parse_expr("f(1, a)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_nested_call) {
  const UnTypedExpr expected = call("f", {0, 13}, {call("f", {2, 9}, {one(4), ident("a", 7)}), ident("b", 11)});
  check_pass(expected, parse_expr("f(f(1, a), b)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_borrow) { check_pass(expr_borrow(one(1)), parse_expr("&1")); }

BOOST_AUTO_TEST_CASE(parser_expr_tuple_empty) { check_pass(expr_tuple({0, 2}), parse_expr("()")); }

BOOST_AUTO_TEST_CASE(parser_expr_tuple1) { check_pass(expr_tuple({0, 3}, one(1)), parse_expr("(1)")); }

BOOST_AUTO_TEST_CASE(parser_expr_tuple2) {
  check_pass(expr_tuple({0, 6}, one(1), ident("a", 4)), parse_expr("(1, a)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_nested_tuple) {
  const UnTypedExpr expected = expr_tuple({0, 10}, expr_tuple({1, 3}), expr_tuple({5, 9}, expr_tuple({6, 8})));
  check_pass(expected, parse_expr("((), (()))"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs) { check_pass(call("b", {0, 5}, {ident("a", 0)}), parse_expr("a.b()")); }

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_literal) { check_pass(call("b", {0, 5}, {one(0)}), parse_expr("1.b()")); }

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_function) {
  check_pass(call("b", {0, 7}, {call("a", {0, 3})}), parse_expr("a().b()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_multi_parameter) {
  check_pass(call("b", {0, 6}, {ident("a", 0), one(4)}), parse_expr("a.b(1)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_tuple) {
  check_pass(call("b", {0, 9}, {expr_tuple({0, 5}, expr_tuple({1, 4}, one(2)))}), parse_expr("((1)).b()"));
}

BOOST_AUTO_TEST_CASE(parser_ident_simple) { check_pass(ident_pattern("a", 0), parse_pattern("a")); }

BOOST_AUTO_TEST_CASE(parser_ident_wild_card) { check_pass(wildcard_pattern(0), parse_pattern("_")); }

BOOST_AUTO_TEST_CASE(parser_ident_empty_tuple) { check_pass(tuple_pattern({0, 2}), parse_pattern("()")); }

BOOST_AUTO_TEST_CASE(parser_tuple_pattern1) {
  const Pattern expected = tuple_pattern({0, 3}, ident_pattern("a", 1));
  check_pass(expected, parse_pattern("(a)"));
}

BOOST_AUTO_TEST_CASE(parser_tuple_pattern2) {
  const Pattern expected = tuple_pattern({0, 6}, ident_pattern("a", 1), wildcard_pattern(4));
  check_pass(expected, parse_pattern("(a, _)"));
}

BOOST_AUTO_TEST_CASE(parser_ident_nested_tuple) {
  const Pattern expected = tuple_pattern({0, 8}, tuple_pattern({1, 4}, ident_pattern("a", 2)), wildcard_pattern(6));
  check_pass(expected, parse_pattern("((a), _)"));
}

BOOST_AUTO_TEST_CASE(parser_type_simple) { check_pass(type("a", 0), parse_type("a")); }

BOOST_AUTO_TEST_CASE(parser_type_borrowed) { check_pass(borrow_type(type("a", 1)), parse_type("&a")); }

BOOST_AUTO_TEST_CASE(parser_floating) { check_pass(floating_type<NamedType>(), parse_type("_")); }

BOOST_AUTO_TEST_CASE(parser_floating_borrowed) {
  check_pass(borrow_type(floating_type<NamedType>()), parse_type("&_"));
}

BOOST_AUTO_TEST_CASE(parser_type_empty_tuple) { check_pass(tuple_type<NamedType>(), parse_type("()")); }

BOOST_AUTO_TEST_CASE(parser_type_tuple1) {
  const CompoundType<NamedType> expected = tuple_type<NamedType>(type("a", 1));
  check_pass(expected, parse_type("(a)"));
}

BOOST_AUTO_TEST_CASE(parser_type_tuple2) {
  const CompoundType<NamedType> expected = tuple_type<NamedType>(borrow_type(type("a", 2)), floating_type<NamedType>());
  check_pass(expected, parse_type("(&a, _)"));
}

BOOST_AUTO_TEST_CASE(parser_type_nested_tuple) {
  const CompoundType<NamedType> expected =
    tuple_type<NamedType>(tuple_type<NamedType>(type("a", 2)), borrow_type(floating_type<NamedType>()));
  check_pass(expected, parse_type("((a), &_)"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_simple) {
  const UnTypedAssignment expected{ident_pattern("a", 4), floating_type<NamedType>(), one(8), {0, 9}};
  check_pass(expected, parse_assignment("let a = 1"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_type) {
  const UnTypedAssignment expected{ident_pattern("a", 4), type("X", 7), one(11), {0, 12}};
  check_pass(expected, parse_assignment("let a: X = 1"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_implicit) {
  const UnTypedAssignment expected{ident_pattern("a", 4), floating_type<NamedType>(), one(11), {0, 12}};
  check_pass(expected, parse_assignment("let a: _ = 1"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_tuple) {
  const UnTypedAssignment expected{tuple_pattern({4, 6}), tuple_type<NamedType>(), expr_tuple({13, 15}), {0, 15}};
  check_pass(expected, parse_assignment("let (): () = ()"));
}

BOOST_AUTO_TEST_CASE(parser_header_no_args) {
  const UnTypedHeader expected = header(tuple_pattern({0, 2}), {}, type("T", 6), {0, 7});
  check_pass(expected, parse_header("() -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_one_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 6}, ident_pattern("a", 1)), {type("X", 4)}, type("T", 10), {0, 11});
  check_pass(expected, parse_header("(a: X) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_two_args) {
  const UnTypedHeader expected = header(tuple_pattern({0, 12}, ident_pattern("a", 1), ident_pattern("b", 7)),
                                        {type("X", 4), type("Y", 10)},
                                        type("T", 16),
                                        {0, 17});
  check_pass(expected, parse_header("(a: X, b: Y) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_borrow_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 7}, ident_pattern("a", 1)), {borrow_type(type("X", 5))}, type("T", 11), {0, 12});
  check_pass(expected, parse_header("(a: &X) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_pattern_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 17}, tuple_pattern({1, 7}, ident_pattern("a", 2), ident_pattern("b", 5))),
           {tuple_type<NamedType>(type("X", 10), borrow_type(type("Y", 14)))},
           type("T", 21),
           {0, 22});
  check_pass(expected, parse_header("((a, b): (X, &Y)) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_unspecified_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 3}, ident_pattern("a", 1)), {floating_type<NamedType>()}, type("T", 7), {0, 8});
  check_pass(expected, parse_header("(a) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_return_tuple_empty) {
  const UnTypedHeader expected = header(tuple_pattern({0, 2}), {}, tuple_type<NamedType>(), {0, 8});
  check_pass(expected, parse_header("() -> ()"));
}

BOOST_AUTO_TEST_CASE(parser_header_return_tuple1) {
  const UnTypedHeader expected = header(tuple_pattern({0, 2}), {}, tuple_type<NamedType>(type("T", 7)), {0, 9});
  check_pass(expected, parse_header("() -> (T)"));
}

BOOST_AUTO_TEST_CASE(parser_scope_simple) {
  const UnTypedScope expected{{}, one(2)};
  check_pass(expected, parse_scope("{ 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment) {
  const UnTypedScope expected{{{ident_pattern("x", 6), floating_type<NamedType>(), one(10), {2, 11}}},
                              {ident("x", 13)}};
  check_pass(expected, parse_scope("{ let x = 1; x }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment2) {
  const UnTypedScope expected{{{ident_pattern("x", 6), floating_type<NamedType>(), one(10), {2, 11}},
                               {ident_pattern("y", 17), floating_type<NamedType>(), one(21), {13, 22}}},
                              {ident("x", 24)}};
  check_pass(expected, parse_scope("{ let x = 1; let y = 1; x }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_tuple_empty) {
  const UnTypedScope expected{{}, expr_tuple({2, 4})};
  check_pass(expected, parse_scope("{ () }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_tuple) {
  const UnTypedScope expected{{}, expr_tuple({2, 5}, one(3))};
  check_pass(expected, parse_scope("{ (1) }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_multi_tuple) {
  const UnTypedScope expected{{}, expr_tuple({2, 8}, one(3), one(6))};
  check_pass(expected, parse_scope("{ (1, 1) }"));
}

BOOST_AUTO_TEST_CASE(parser_ast_empty) { check_pass(AST{}, parse("")); }

BOOST_AUTO_TEST_CASE(parser_ast_simple) {
  const AST expected{{"f", {header(tuple_pattern({4, 6}), {}, type("T", 10), {4, 11}), {{}, {one(14)}}}}};
  check_pass(expected, parse("fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_multiple_functions) {
  const AST expected{
    {"f", {header(tuple_pattern({4, 6}), {}, type("T", 10), {4, 11}), {{}, {one(14)}}}},
    {"g", {header(tuple_pattern({22, 24}), {}, type("T", 28), {22, 29}), {{}, {one(32)}}}},
  };
  check_pass(expected, parse("fn f() -> T { 1 } fn g() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn) { check_single_error({{0, 1}, "expected 'fn'"}, parse("f")); }

BOOST_AUTO_TEST_CASE(parser_no_fn2) { check_single_error({{0, 1}, "expected 'fn'"}, parse("a")); }

BOOST_AUTO_TEST_CASE(parser_no_fn3) { check_single_error({{0, 1}, "expected 'fn'"}, parse(")")); }

BOOST_AUTO_TEST_CASE(parser_bad_paren) { check_single_error({{2, 3}, "expected token" /* ? */}, parse("fn)")); }

BOOST_AUTO_TEST_CASE(parser_no_expr) { check_single_error({{1, 2}, "expected literal" /* ? */}, parse_scope("{}")); }

BOOST_AUTO_TEST_CASE(parser_no_return_type) {
  check_single_error({{6, 7}, "expected '('" /* ? */}, parse_header("() -> { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_return) { check_single_error({{7, 8}, "expected '->'" /* ? */}, parse("fn f() { 1 }")); }

BOOST_AUTO_TEST_CASE(parser_no_params) {
  check_single_error({{5, 7}, "expected '('" /* ? */}, parse("fn f -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn_name) {
  check_single_error({{3, 4}, "expected token" /* ? */}, parse("fn () -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_fn_name) {
  check_single_error({{3, 4}, "expected token" /* ? */}, parse("fn 1() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn_keyword) { check_single_error({{0, 1}, "expected 'fn'"}, parse("f() -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(parser_no_scope) { check_single_error({{11, 11}, "expected '{'"}, parse("fn f() -> T")); }

BOOST_AUTO_TEST_CASE(parser_unclosed_paren) { check_single_error({{6, 8}, "expected ')'"}, parse("fn f( -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(parser_unopened_paren) { check_single_error({{4, 5}, "expected '('"}, parse("fn f) -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(parser_untyped_paren2) {
  check_single_error({{9, 10}, "expected '('" /* ? */}, parse("fn f(a : ) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_type_paren) {
  check_single_error({{9, 10}, "expected '('" /* ? */}, parse("fn f(a : 1) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_expr_unclosed) {
  check_single_error({{17, 18}, "expected ')'"}, parse("fn f() -> T { a( }"));
}

BOOST_AUTO_TEST_CASE(parser_expr_unopened) {
  check_single_error({{15, 16}, "expected '}'"}, parse("fn f() -> T { a) }"));
}

BOOST_AUTO_TEST_CASE(parser_expr_bad_comma) {
  check_single_error({{18, 19}, "expected literal" /* ? */}, parse("fn f() -> T { a(1,) }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_chain) { check_single_error({{18, 19}, "expected '('"}, parse("fn f() -> T { a.b }")); }

BOOST_AUTO_TEST_CASE(parser_bad_chain2) {
  check_single_error({{16, 17}, "expected token"}, parse("fn f() -> T { a.1 }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_assignment) {
  check_single_error({{18, 19}, "expected '('"}, parse("fn f() -> T { let }"));
}

BOOST_AUTO_TEST_CASE(parser_let_no_var) {
  check_single_error({{18, 19}, "expected '('"}, parse("fn f() -> T { let = 1; }"));
}

BOOST_AUTO_TEST_CASE(parser_let_no_expr) {
  check_single_error({{22, 23}, "expected literal"}, parse("fn f() -> T { let x = }"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_no_expr) {
  check_single_error({{25, 26}, "expected literal"}, parse("fn f() -> T { let x = 0; }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_second_fn) {
  check_single_error({{20, 20}, "expected token"}, parse("fn f() -> T { 1 } fn"));
}

} // namespace ooze::ast
