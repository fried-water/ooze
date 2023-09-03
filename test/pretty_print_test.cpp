#include "test.h"

#include "pretty_print.h"

namespace ooze {

using namespace ast;

BOOST_AUTO_TEST_SUITE(pp)

BOOST_AUTO_TEST_CASE(pattern) {
  BOOST_CHECK_EQUAL("abc", pretty_print(UnTypedPattern{Ident{"abc"}}));
  BOOST_CHECK_EQUAL("_", pretty_print(UnTypedPattern{WildCard{}}));
  BOOST_CHECK_EQUAL("()", pretty_print(UnTypedPattern{std::vector<UnTypedPattern>{}}));
  BOOST_CHECK_EQUAL("(abc, _)",
                    pretty_print(UnTypedPattern{std::vector<UnTypedPattern>{{Ident{"abc"}}, {WildCard{}}}}));
}

BOOST_AUTO_TEST_CASE(compound_type) {
  const Env e = create_primative_env();

  const auto int_named = leaf_type<NamedType>({"i32"});
  const auto int_typed = leaf_type<TypeID>(type_id<int>());

  BOOST_CHECK_EQUAL("i32", pretty_print(e, leaf_type<TypeID>(type_id<int>())));
  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id<int>().id), pretty_print({}, type_id<int>()));

  BOOST_CHECK_EQUAL("i32", pretty_print(int_named));
  BOOST_CHECK_EQUAL("i32", pretty_print(e, int_typed));

  BOOST_CHECK_EQUAL("&i32", pretty_print(borrow_type(int_named)));

  BOOST_CHECK_EQUAL("()", pretty_print(tuple_type<NamedType>({})));
  BOOST_CHECK_EQUAL("(i32)", pretty_print(tuple_type<NamedType>({int_named})));
  BOOST_CHECK_EQUAL("(i32, i32)", pretty_print(tuple_type<NamedType>({int_named, int_named})));

  BOOST_CHECK_EQUAL("fn() -> i32", pretty_print(function_type(tuple_type<NamedType>({}), int_named)));
  BOOST_CHECK_EQUAL("fn(i32) -> i32", pretty_print(function_type(tuple_type<NamedType>({int_named}), int_named)));
}

BOOST_AUTO_TEST_CASE(expr) {
  const auto one = UnTypedExpr{Literal{1}};
  const auto ident = UnTypedExpr{Ident{"abc"}};

  BOOST_CHECK_EQUAL("1i32", pretty_print(one));
  BOOST_CHECK_EQUAL("1i32", pretty_print({}, TypedExpr{Literal{1}}));
  BOOST_CHECK_EQUAL("1i32", pretty_print({}, CheckedExpr{Literal{1}}));
  BOOST_CHECK_EQUAL("\"abc\"", pretty_print(UnTypedExpr{Literal{std::string{"abc"}}}));

  BOOST_CHECK_EQUAL("abc", pretty_print(ident));

  BOOST_CHECK_EQUAL("&abc", pretty_print(UnTypedExpr{UnTypedBorrowExpr{ident}}));

  BOOST_CHECK_EQUAL("(1i32, abc)", pretty_print(UnTypedExpr{std::vector<UnTypedExpr>{one, ident}}));

  BOOST_CHECK_EQUAL("f(1i32)",
                    pretty_print(UnTypedExpr{
                      UnTypedCallExpr{UnTypedExpr{ast::Ident{"f"}}, UnTypedExpr{std::vector<UnTypedExpr>{one}}}}));
  BOOST_CHECK_EQUAL("f(1i32)",
                    pretty_print({},
                                 CheckedExpr{CheckedCallExpr{CheckedExpr{EnvFunctionRef{"f", 0}},
                                                             CheckedExpr{std::vector<CheckedExpr>{{Literal{1}}}}}}));
}

BOOST_AUTO_TEST_CASE(assignment) {
  const auto y_expr = UnTypedExpr{Ident{"y"}};

  BOOST_CHECK_EQUAL("let x = y", pretty_print(UnTypedAssignment{UnTypedPattern{Ident{"x"}}, y_expr}));
  BOOST_CHECK_EQUAL("let x: i32 = y",
                    pretty_print(UnTypedAssignment{UnTypedPattern{Ident{"x"}, leaf_type<NamedType>({"i32"})}, y_expr}));
}

BOOST_AUTO_TEST_CASE(scope) {
  const auto y_expr = UnTypedExpr{Ident{"y"}};
  const auto assign = UnTypedAssignment{UnTypedPattern{Ident{"x"}}, y_expr};

  const auto scope = UnTypedExpr{UnTypedScopeExpr{{}, y_expr}};

  BOOST_CHECK_EQUAL("{\n  y\n}", pretty_print(scope));

  BOOST_CHECK_EQUAL("{\n  let x = y;\n  y\n}", pretty_print(UnTypedExpr{UnTypedScopeExpr{{assign}, y_expr}}));
  BOOST_CHECK_EQUAL("{\n  let x = y;\n  let x = y;\n  y\n}",
                    pretty_print(UnTypedExpr{UnTypedScopeExpr{{assign, assign}, y_expr}}));

  const auto nested_scope = UnTypedExpr{UnTypedScopeExpr{
    {UnTypedAssignment{UnTypedPattern{Ident{"x"}}, scope}}, UnTypedExpr{UnTypedScopeExpr{{}, y_expr}}}};

  BOOST_CHECK_EQUAL("{\n  let x = {\n    y\n  };\n  {\n    y\n  }\n}", pretty_print(nested_scope));
}

BOOST_AUTO_TEST_CASE(function) {
  const auto i = leaf_type<NamedType>({"i"});
  const auto x_expr = UnTypedExpr{Ident{"x"}, i};

  BOOST_CHECK_EQUAL("x: i -> i = x", pretty_print(UnTypedFunction{UnTypedPattern{Ident{"x"}, i}, x_expr}));
  BOOST_CHECK_EQUAL(
    "() -> i = x",
    pretty_print(UnTypedFunction{UnTypedPattern{std::vector<UnTypedPattern>{}, tuple_type<NamedType>({})}, x_expr}));
  BOOST_CHECK_EQUAL(
    "(x: i) -> i = x",
    pretty_print(UnTypedFunction{
      UnTypedPattern{std::vector<UnTypedPattern>{UnTypedPattern{Ident{"x"}}}, tuple_type<NamedType>({i})}, x_expr}));

  BOOST_CHECK_EQUAL(
    "() -> i {\n  x\n}",
    pretty_print(UnTypedFunction{
      UnTypedPattern{std::vector<UnTypedPattern>{}, tuple_type<NamedType>({})}, {UnTypedScopeExpr{{}, x_expr}, i}}));
}

BOOST_AUTO_TEST_CASE(ast) {
  const auto f = UnTypedFunction{
    {UnTypedPattern{
      std::vector<UnTypedPattern>{},
      tuple_type<NamedType>({}),
    }},
    UnTypedExpr{Ident{"x"}, leaf_type<NamedType>({"i"})}};

  BOOST_CHECK_EQUAL("fn f() -> i = x\n\nfn g() -> i = x", pretty_print(UnTypedAST{{"f", f}, {"g", f}}));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
