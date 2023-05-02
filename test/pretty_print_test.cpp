#include "test.h"

#include "pretty_print.h"

namespace ooze {

using namespace ast;

BOOST_AUTO_TEST_CASE(pretty_print_pattern) {
  BOOST_CHECK_EQUAL("abc", pretty_print(Pattern{Ident{"abc"}}));
  BOOST_CHECK_EQUAL("_", pretty_print(Pattern{WildCard{}}));
  BOOST_CHECK_EQUAL("()", pretty_print(Pattern{std::vector<Pattern>{}}));
  BOOST_CHECK_EQUAL("(abc, _)", pretty_print(Pattern{std::vector<Pattern>{{Ident{"abc"}}, {WildCard{}}}}));
}

BOOST_AUTO_TEST_CASE(pretty_print_type) {
  const Env e = create_primative_env();

  const auto int_named = leaf_type<NamedType>({"i32"});
  const auto int_typed = leaf_type<TypeID>(anyf::type_id<int>());

  BOOST_CHECK_EQUAL("i32", pretty_print(e, leaf_type<TypeID>(anyf::type_id<int>())));
  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", anyf::type_id<int>().id), pretty_print({}, anyf::type_id<int>()));

  BOOST_CHECK_EQUAL("i32", pretty_print(int_named));
  BOOST_CHECK_EQUAL("i32", pretty_print(e, int_typed));

  BOOST_CHECK_EQUAL("&i32", pretty_print(borrow_type(int_named)));

  BOOST_CHECK_EQUAL("()", pretty_print(tuple_type<NamedType>({})));
  BOOST_CHECK_EQUAL("(i32)", pretty_print(tuple_type<NamedType>({int_named})));
  BOOST_CHECK_EQUAL("(i32, i32)", pretty_print(tuple_type<NamedType>({int_named, int_named})));

  BOOST_CHECK_EQUAL("i32 -> i32", pretty_print(function_type(int_named, int_named)));
  BOOST_CHECK_EQUAL("i32 -> i32 -> i32", pretty_print(function_type(int_named, function_type(int_named, int_named))));
  BOOST_CHECK_EQUAL("(i32) -> i32", pretty_print(function_type(tuple_type<NamedType>({int_named}), int_named)));
}

BOOST_AUTO_TEST_CASE(pretty_print_expr) {
  const auto one = UnTypedExpr{Literal{1}};
  const auto ident = UnTypedExpr{IdentExpr{"abc"}};

  BOOST_CHECK_EQUAL("1i32", pretty_print(one));
  BOOST_CHECK_EQUAL("1i32", pretty_print({}, TypedExpr{Literal{1}}));
  BOOST_CHECK_EQUAL("1i32", pretty_print({}, CheckedExpr{Literal{1}}));
  BOOST_CHECK_EQUAL("\"abc\"", pretty_print(UnTypedExpr{Literal{std::string{"abc"}}}));

  BOOST_CHECK_EQUAL("abc", pretty_print(ident));

  BOOST_CHECK_EQUAL("&abc", pretty_print(UnTypedExpr{UnTypedBorrowExpr{ident}}));

  BOOST_CHECK_EQUAL("(1i32, abc)", pretty_print(UnTypedExpr{std::vector<UnTypedExpr>{one, ident}}));

  BOOST_CHECK_EQUAL("f(1i32)", pretty_print(UnTypedExpr{UnTypedCallExpr{{"f"}, std::vector<UnTypedExpr>{one}}}));
  BOOST_CHECK_EQUAL("f(1i32)",
                    pretty_print({}, CheckedExpr{CheckedCallExpr{{"f", 0}, std::vector<CheckedExpr>{{Literal{1}}}}}));
}

BOOST_AUTO_TEST_CASE(pretty_print_assignment) {
  const auto y_expr = UnTypedExpr{IdentExpr{"y"}};

  BOOST_CHECK_EQUAL("let x = y",
                    pretty_print(UnTypedAssignment{Pattern{Ident{"x"}}, floating_type<NamedType>(), y_expr}));
  BOOST_CHECK_EQUAL("let x: i32 = y",
                    pretty_print(UnTypedAssignment{Pattern{Ident{"x"}}, leaf_type<NamedType>({"i32"}), y_expr}));
}

BOOST_AUTO_TEST_CASE(pretty_print_scope) {
  const auto y_expr = UnTypedExpr{IdentExpr{"y"}};
  const auto assign = UnTypedAssignment{Pattern{Ident{"x"}}, floating_type<NamedType>(), y_expr};

  const auto scope = UnTypedExpr{UnTypedScopeExpr{{}, y_expr}};

  BOOST_CHECK_EQUAL("{\n  y\n}", pretty_print(scope));

  BOOST_CHECK_EQUAL("{\n  let x = y;\n  y\n}", pretty_print(UnTypedExpr{UnTypedScopeExpr{{assign}, y_expr}}));
  BOOST_CHECK_EQUAL("{\n  let x = y;\n  let x = y;\n  y\n}",
                    pretty_print(UnTypedExpr{UnTypedScopeExpr{{assign, assign}, y_expr}}));

  const auto nested_scope =
    UnTypedExpr{UnTypedScopeExpr{{UnTypedAssignment{Pattern{Ident{"x"}}, floating_type<NamedType>(), scope}},
                                 UnTypedExpr{UnTypedScopeExpr{{}, y_expr}}}};

  BOOST_CHECK_EQUAL("{\n  let x = {\n    y\n  };\n  {\n    y\n  }\n}", pretty_print(nested_scope));
}

BOOST_AUTO_TEST_CASE(pretty_print_header) {
  const auto i = leaf_type<NamedType>({"i"});

  BOOST_CHECK_EQUAL("x: i -> i", pretty_print(UnTypedHeader{Pattern{Ident{"x"}}, {i, i}}));
  BOOST_CHECK_EQUAL("() -> i",
                    pretty_print(UnTypedHeader{Pattern{std::vector<Pattern>{}}, {tuple_type<NamedType>({}), i}}));
  BOOST_CHECK_EQUAL(
    "(x: i) -> i",
    pretty_print(UnTypedHeader{Pattern{std::vector<Pattern>{Pattern{Ident{"x"}}}}, {tuple_type<NamedType>({i}), i}}));
}

BOOST_AUTO_TEST_CASE(pretty_print_function) {
  const auto i = leaf_type<NamedType>({"i"});
  const auto x_expr = UnTypedExpr{IdentExpr{"x"}};

  BOOST_CHECK_EQUAL("() -> i {\n  x\n}",
                    pretty_print(UnTypedFunction{{Pattern{std::vector<Pattern>{}}, {tuple_type<NamedType>({}), i}},
                                                 {UnTypedScopeExpr{{}, x_expr}}}));
  BOOST_CHECK_EQUAL(
    "() -> i = x",
    pretty_print(UnTypedFunction{{Pattern{std::vector<Pattern>{}}, {tuple_type<NamedType>({}), i}}, x_expr}));
}

BOOST_AUTO_TEST_CASE(pretty_print_ast) {
  const auto f =
    UnTypedFunction{{Pattern{std::vector<Pattern>{}}, {tuple_type<NamedType>({}), leaf_type<NamedType>({"i"})}},
                    UnTypedExpr{IdentExpr{"x"}}};

  BOOST_CHECK_EQUAL("fn f() -> i = x\n\nfn g() -> i = x", pretty_print(UnTypedAST{{"f", f}, {"g", f}}));
}

} // namespace ooze
