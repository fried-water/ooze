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

UnTypedExpr call(UnTypedExpr callee, UnTypedExpr arg, std::optional<Slice> opt_ref = {}) {
  const Slice ref = opt_ref ? *opt_ref : Slice{std::min(callee.ref.begin, arg.ref.begin), arg.ref.end};
  return {UnTypedCallExpr{std::move(callee), std::move(arg)}, ref};
}

UnTypedExpr ident(std::string_view name, int offset) {
  return {{Ident{std::string(name)}}, {offset, offset + (int)name.size()}};
}

UnTypedExpr one(int offset) { return {{Literal{1}}, Slice{offset, offset + 1}}; }

template <typename... Children>
UnTypedExpr expr_tuple(Slice ref, Children... children) {
  return {std::vector<UnTypedExpr>{std::move(children)...}, ref};
}

UnTypedExpr expr_borrow(UnTypedExpr e) { return {UnTypedBorrowExpr{std::move(e)}, {e.ref.begin - 1, e.ref.end}}; }

UnTypedExpr select(UnTypedExpr cond, UnTypedExpr if_, UnTypedExpr else_, Slice ref) {
  return {UnTypedSelectExpr{std::move(cond), std::move(if_), std::move(else_)}, ref};
}

UnTypedExpr scope(std::vector<UnTypedAssignment> assignments, UnTypedExpr expr, Slice ref) {
  return {UnTypedScopeExpr{std::move(assignments), std::move(expr)}, ref};
}

Pattern ident_pattern(std::string_view name, int offset) {
  return {{Ident{std::string(name)}}, {offset, offset + (int)name.size()}};
}

Pattern wildcard_pattern(int offset) { return {{WildCard{}}, {offset, offset + 1}}; }

template <typename... Children>
Pattern tuple_pattern(Slice ref, Children... children) {
  return {std::vector<Pattern>{std::move(children)...}, ref};
}

CompoundType<NamedType> type(std::string_view name, int offset) {
  return {NamedType{std::string(name)}, {offset, offset + (int)name.size()}};
}

UnTypedHeader
header(Pattern pattern, std::vector<CompoundType<NamedType>> inputs, CompoundType<NamedType> output, Slice ref) {
  return {std::move(pattern), {{{std::move(inputs), pattern.ref}}, std::move(output)}, ref};
}

} // namespace

BOOST_AUTO_TEST_CASE(parser_expr_literal) { check_pass(one(0), parse_expr("1")); }

BOOST_AUTO_TEST_CASE(parser_expr_ident) { check_pass(ident("abc", 0), parse_expr("abc")); }

BOOST_AUTO_TEST_CASE(parser_expr_call) { check_pass(call(ident("f", 0), expr_tuple({1, 3})), parse_expr("f()")); }

BOOST_AUTO_TEST_CASE(parser_expr_call_call) {
  check_pass(call(call(ident("f", 0), expr_tuple({1, 3})), expr_tuple({3, 5})), parse_expr("f()()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_call_one_arg) {
  check_pass(call(ident("f", 0), expr_tuple({1, 4}, one(2))), parse_expr("f(1)"));
}

BOOST_AUTO_TEST_CASE(parser_call_two_arg) {
  check_pass(call(ident("f", 0), expr_tuple({1, 7}, one(2), ident("a", 5))), parse_expr("f(1, a)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_nested_call) {
  const UnTypedExpr expected = call(
    ident("f", 0), expr_tuple({1, 13}, call(ident("f", 2), expr_tuple({3, 9}, one(4), ident("a", 7))), ident("b", 11)));
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

BOOST_AUTO_TEST_CASE(parser_expr_ufcs) {
  check_pass(call(ident("b", 2), expr_tuple({3, 5}, ident("a", 0)), Slice{0, 5}), parse_expr("a.b()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_literal) {
  check_pass(call(ident("b", 2), expr_tuple({3, 5}, one(0)), Slice{0, 5}), parse_expr("1.b()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_function) {
  check_pass(call(ident("b", 4), expr_tuple({5, 7}, call(ident("a", 0), expr_tuple({1, 3}))), Slice{0, 7}),
             parse_expr("a().b()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_chain) {
  check_pass(call(ident("c", 6),
                  expr_tuple({7, 9}, call(ident("b", 2), expr_tuple({3, 5}, ident("a", 0)), Slice{0, 5})),
                  Slice{0, 9}),
             parse_expr("a.b().c()"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_multi_parameter) {
  check_pass(call(ident("b", 2), expr_tuple({3, 6}, ident("a", 0), one(4)), Slice{0, 6}), parse_expr("a.b(1)"));
}

BOOST_AUTO_TEST_CASE(parser_expr_ufcs_tuple) {
  check_pass(call(ident("b", 6), expr_tuple({7, 9}, expr_tuple({0, 5}, expr_tuple({1, 4}, one(2)))), Slice{0, 9}),
             parse_expr("((1)).b()"));
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

BOOST_AUTO_TEST_CASE(parser_type_borrowed) { check_pass(borrow_type(type("a", 1), {0, 2}), parse_type("&a")); }

BOOST_AUTO_TEST_CASE(parser_floating) { check_pass(floating_type<NamedType>({0, 1}), parse_type("_")); }

BOOST_AUTO_TEST_CASE(parser_floating_borrowed) {
  check_pass(borrow_type(floating_type<NamedType>({1, 2}), {0, 2}), parse_type("&_"));
}

BOOST_AUTO_TEST_CASE(parser_type_empty_tuple) { check_pass(tuple_type<NamedType>({}, {0, 2}), parse_type("()")); }

BOOST_AUTO_TEST_CASE(parser_type_tuple1) {
  const CompoundType<NamedType> expected = tuple_type<NamedType>({type("a", 1)}, {0, 3});
  check_pass(expected, parse_type("(a)"));
}

BOOST_AUTO_TEST_CASE(parser_type_tuple2) {
  const CompoundType<NamedType> expected =
    tuple_type<NamedType>({borrow_type(type("a", 2), {1, 3}), floating_type<NamedType>({5, 6})}, {0, 7});
  check_pass(expected, parse_type("(&a, _)"));
}

BOOST_AUTO_TEST_CASE(parser_type_nested_tuple) {
  const CompoundType<NamedType> expected = tuple_type<NamedType>(
    {tuple_type<NamedType>({type("a", 2)}, {1, 4}), borrow_type(floating_type<NamedType>({7, 8}), {6, 8})}, {0, 9});
  check_pass(expected, parse_type("((a), &_)"));
}

BOOST_AUTO_TEST_CASE(parser_type_fn) {
  check_pass(function_type<NamedType>(tuple_type<NamedType>({}, {2, 4}), type("a", 8), {0, 9}),
             parse_type("fn() -> a"));
}

BOOST_AUTO_TEST_CASE(parser_type_fn_tuple) {
  check_pass(function_type<NamedType>(tuple_type<NamedType>({type("x", 3)}, {2, 5}), type("a", 9), {0, 10}),
             parse_type("fn(x) -> a"));
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
  const UnTypedAssignment expected{ident_pattern("a", 4), floating_type<NamedType>({7, 8}), one(11), {0, 12}};
  check_pass(expected, parse_assignment("let a: _ = 1"));
}

BOOST_AUTO_TEST_CASE(parser_assignment_tuple) {
  const UnTypedAssignment expected{
    tuple_pattern({4, 6}), tuple_type<NamedType>({}, {8, 10}), expr_tuple({13, 15}), {0, 15}};
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
  const UnTypedHeader expected =
    header(tuple_pattern({0, 12}, ident_pattern("a", 1), ident_pattern("b", 7)),
           {type("X", 4), type("Y", 10)},
           type("T", 16),
           {0, 17});
  check_pass(expected, parse_header("(a: X, b: Y) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_borrow_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 7}, ident_pattern("a", 1)), {borrow_type(type("X", 5), {4, 6})}, type("T", 11), {0, 12});
  check_pass(expected, parse_header("(a: &X) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_fn_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 14}, ident_pattern("x", 1)),
           {function_type<NamedType>(tuple_type<NamedType>({}, {6, 8}), type("a", 12), {4, 13})},
           type("T", 18),
           {0, 19});
  check_pass(expected, parse_header("(x: fn() -> a) -> T"));
}

BOOST_AUTO_TEST_CASE(parser_header_fn_return) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 2}),
           {},
           function_type<NamedType>(tuple_type<NamedType>({}, {8, 10}), type("a", 14), {6, 15}),
           {0, 15});
  check_pass(expected, parse_header("() -> fn() -> a"));
}

BOOST_AUTO_TEST_CASE(parser_header_pattern_arg) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 17}, tuple_pattern({1, 7}, ident_pattern("a", 2), ident_pattern("b", 5))),
           {tuple_type<NamedType>({type("X", 10), borrow_type(type("Y", 14), {13, 15})}, {9, 16})},
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
  const UnTypedHeader expected = header(tuple_pattern({0, 2}), {}, tuple_type<NamedType>({}, {6, 8}), {0, 8});
  check_pass(expected, parse_header("() -> ()"));
}

BOOST_AUTO_TEST_CASE(parser_header_return_tuple1) {
  const UnTypedHeader expected =
    header(tuple_pattern({0, 2}), {}, tuple_type<NamedType>({type("T", 7)}, {6, 9}), {0, 9});
  check_pass(expected, parse_header("() -> (T)"));
}

BOOST_AUTO_TEST_CASE(parser_select) {
  check_pass(select(ident("a", 7), scope({}, one(11), {9, 14}), scope({}, one(22), {20, 25}), {0, 25}),
             parse_expr("select a { 1 } else { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_simple) { check_pass(scope({}, one(2), {0, 5}), parse_expr("{ 1 }")); }

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment) {
  const UnTypedExpr expected =
    scope({{ident_pattern("x", 6), floating_type<NamedType>(), one(10), {2, 11}}}, ident("x", 13), {0, 16});
  check_pass(expected, parse_expr("{ let x = 1; x }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_with_assignment2) {
  const UnTypedExpr expected =
    scope({{ident_pattern("x", 6), floating_type<NamedType>(), one(10), {2, 11}},
           {ident_pattern("y", 17), floating_type<NamedType>(), one(21), {13, 22}}},
          ident("x", 24),
          {0, 27});
  check_pass(expected, parse_expr("{ let x = 1; let y = 1; x }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_tuple_empty) {
  check_pass(scope({}, expr_tuple({2, 4}), {0, 6}), parse_expr("{ () }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_tuple) {
  check_pass(scope({}, expr_tuple({2, 5}, one(3)), {0, 7}), parse_expr("{ (1) }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_return_multi_tuple) {
  check_pass(scope({}, expr_tuple({2, 8}, one(3), one(6)), {0, 10}), parse_expr("{ (1, 1) }"));
}

BOOST_AUTO_TEST_CASE(parser_scope_nested) {
  const UnTypedExpr expected =
    scope({{ident_pattern("x", 6), floating_type<NamedType>(), scope({}, one(12), {10, 15}), {2, 15}}},
          scope({{ident_pattern("y", 23), floating_type<NamedType>(), scope({}, ident("x", 29), {27, 32}), {19, 32}}},
                scope({}, ident("y", 36), {34, 39}),
                {17, 41}),
          {0, 43});

  check_pass(expected, parse_expr("{ let x = { 1 }; { let y = { x }; { y } } }"));
}

BOOST_AUTO_TEST_CASE(parser_ast_empty) { check_pass(UnTypedAST{}, parse("")); }

BOOST_AUTO_TEST_CASE(parser_ast_simple) {
  const UnTypedAST expected{
    {"f", {header(tuple_pattern({4, 6}), {}, type("T", 10), {4, 11}), scope({}, one(14), {12, 17})}}};
  check_pass(expected, parse("fn f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_ast_non_scope) {
  const UnTypedAST expected{
    {"f",
     {header(tuple_pattern({4, 7}, ident_pattern("x", 5)), {floating_type<NamedType>()}, type("T", 11), {4, 12}),
      ident("x", 15)}}};
  check_pass(expected, parse("fn f(x) -> T = x"));
}

BOOST_AUTO_TEST_CASE(parser_multiple_functions) {
  const UnTypedAST expected{
    {"f", {header(tuple_pattern({4, 6}), {}, type("T", 10), {4, 11}), scope({}, one(14), {12, 17})}},
    {"g", {header(tuple_pattern({22, 24}), {}, type("T", 28), {22, 29}), scope({}, one(32), {30, 35})}},
  };
  check_pass(expected, parse("fn f() -> T { 1 } fn g() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_no_fn) { check_single_error({{0, 1}, "expected 'fn'"}, parse("f")); }

BOOST_AUTO_TEST_CASE(parser_no_fn2) { check_single_error({{0, 1}, "expected 'fn'"}, parse("a")); }

BOOST_AUTO_TEST_CASE(parser_no_fn3) { check_single_error({{0, 1}, "expected 'fn'"}, parse(")")); }

BOOST_AUTO_TEST_CASE(parser_bad_paren) { check_single_error({{2, 3}, "expected token" /* ? */}, parse("fn)")); }

BOOST_AUTO_TEST_CASE(parser_no_expr) { check_single_error({{1, 2}, "expected 'let'" /* ? */}, parse_expr("{}")); }

BOOST_AUTO_TEST_CASE(parser_no_return_type) {
  check_single_error({{6, 7}, "expected '('" /* ? */}, parse_header("() -> { 1 }"));
}

BOOST_AUTO_TEST_CASE(parser_fn_no_tupl) { check_single_error({{3, 4}, "expected '('"}, parse_type("fn T -> T")); }

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

BOOST_AUTO_TEST_CASE(parser_select_no_scope) {
  check_single_error({{9, 10}, "expected '{'"}, parse_expr("select a 1 else 1"));
}

BOOST_AUTO_TEST_CASE(parser_no_scope) { check_single_error({{11, 11}, "expected '='"}, parse("fn f() -> T")); }

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
  check_single_error({{18, 19}, "expected '('"}, parse("fn f() -> T { a.1 }"));
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
  check_single_error({{25, 26}, "expected 'let'"}, parse("fn f() -> T { let x = 0; }"));
}

BOOST_AUTO_TEST_CASE(parser_bad_second_fn) {
  check_single_error({{20, 20}, "expected token"}, parse("fn f() -> T { 1 } fn"));
}

} // namespace ooze::ast
