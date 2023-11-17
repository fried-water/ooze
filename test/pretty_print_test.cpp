#include "test.h"

#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

using namespace ast;

namespace {

auto resolve_names(const Env& e, std::string_view src, ContextualResult<std::tuple<AST, ASTTypes>> parse_result) {
  return std::move(parse_result).and_then(applied([&](AST ast, ASTTypes types) {
    return type_name_resolution(e, src, types).map([&](std::vector<TypeID> type_ids) {
      auto [g, tags, srcs] = std::move(types.graph).decompose();
      return std::tuple(std::move(ast),
                        std::move(types.ast_types),
                        std::move(g).append_column(std::move(tags)).append_column(std::move(type_ids)));
    });
  }));
}

void test(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(src, e, ast, type_graph, ast_types));
}

void test_pattern(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse_pattern2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(src, e, ast, type_graph, ast_types));
}

void test_type(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse_type2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(e, type_graph, TypeRef(type_graph.num_nodes() - 1)));
}

void test_expr(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse_expr2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(src, e, ast, type_graph, ast_types));
}

void test_assign(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse_assignment2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(src, e, ast, type_graph, ast_types));
}

void test_fn(std::string_view exp, std::string_view src) {
  const Env e = create_primative_env();
  const auto [ast, ast_types, type_graph] = check_result(resolve_names(e, src, parse_function2(src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(src, e, ast, type_graph, ast_types));
}

} // namespace

BOOST_AUTO_TEST_SUITE(pp)

BOOST_AUTO_TEST_CASE(pattern) {
  test_pattern("abc", "abc");
  test_pattern("_", "_");
  test_pattern("(abc, _)", "(abc, _)");
}

BOOST_AUTO_TEST_CASE(compound_type) {
  test_type("_", "_");

  test_type("i32", "i32");
  test_type("&i32", "&i32");

  test_type("()", "()");
  test_type("(i32)", "(i32)");
  test_type("(i32, i32)", "(i32, i32)");

  test_type("fn() -> i32", "fn() -> i32");
  test_type("fn(i32) -> i32", "fn(i32) -> i32");
}

BOOST_AUTO_TEST_CASE(unnamed_type) {
  Graph<TypeRef, TypeTag, TypeID> g;
  const auto int_type = type_id(knot::Type<int>{});

  const TypeRef id = g.add_node(TypeTag::Leaf, int_type);

  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id(knot::Type<int>{}).id), pretty_print({}, g, TypeRef(0)));
}

BOOST_AUTO_TEST_CASE(expr) {
  test_expr("1i32", "1");
  test_expr("\"abc\"", "\"abc\"");
  test_expr("abc", "abc");
  test_expr("&abc", "&abc");

  test_expr("(1i32, abc)", "(1, abc)");

  test_expr("f(1i32)", "f(1)");
}

BOOST_AUTO_TEST_CASE(assignment) {
  test_assign("let x = y", "let x = y");
  test_assign("let x = y", "let x : _ = y");
  test_assign("let x: i32 = y", "let x: i32 = y");
}

BOOST_AUTO_TEST_CASE(scope) {
  test_expr("abc", "{ abc }");

  test_expr("{\n  let x = y;\n  x\n}", "{ let x = y; x }");
  test_expr("{\n  let x = y;\n  let y = z;\n  y\n}", "{ let x = y; let y = z; y }");
  test_expr("{\n  let x = {\n    let y = z;\n    y\n  };\n  x\n}", "{ let x = { let y = z; y }; x }");
}

BOOST_AUTO_TEST_CASE(fn) {
  test_fn("() -> i32 = x", "() -> i32 = x");
  test_fn("(x) -> i32 = x", "(x) -> i32 = x");
  test_fn("(x: i32) -> i32 = x", "(x: i32) -> i32 = x");
  test_fn("() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test("fn f() -> i32 = x", "fn f() -> i32 = x");
  test("fn f() -> i32 = x\n\nfn g() -> i32 = x", "fn f() -> i32 = x fn g() -> i32 = x");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
