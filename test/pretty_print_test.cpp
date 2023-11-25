#include "test.h"

#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

namespace {

void test(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(sm, e, ast, types));
}

void test_pattern(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse_pattern2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(sm, e, ast, types));
}

void test_type(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse_type2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(e, types.graph, TypeRef(types.graph.num_nodes() - 1)));
}

void test_expr(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse_expr2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(sm, e, ast, types));
}

void test_assign(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse_assignment2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(sm, e, ast, types));
}

void test_fn(std::string_view exp, std::string_view src) {
  const SrcMap sm = {{"", std::string(src)}};
  const Env e = create_primative_env();
  const auto [ast, types] = check_result(type_name_resolution(sm, e, parse_function2({}, {}, SrcID{0}, src)));
  BOOST_CHECK_EQUAL(exp, pretty_print(sm, e, ast, types));
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
  TypeGraph g;
  const auto int_type = type_id(knot::Type<int>{});

  const TypeRef id = g.add_node(TypeTag::Leaf, {}, int_type);

  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id(knot::Type<int>{}).id), pretty_print({}, g, TypeRef(0)));
}

BOOST_AUTO_TEST_CASE(expr) {
  test_expr("1", "1");
  test_expr("1i16", "1i16");
  test_expr("1.0f", "1.0f");
  test_expr("1.0", "1.0");
  test_expr("\"abc\"", "\"abc\"");
  test_expr("abc", "abc");
  test_expr("&abc", "&abc");

  test_expr("(1, abc)", "(1, abc)");

  test_expr("select x { y } else { z }", "select x { y } else { z }");

  test_expr("f(1)", "f(1)");
}

BOOST_AUTO_TEST_CASE(assignment) {
  test_assign("let x = y", "let x = y");
  test_assign("let x = y", "let x : _ = y");
  test_assign("let x: i32 = y", "let x: i32 = y");
  test_assign("let (x, y): (i32, _) = (1, 2)", "let (x, y): (i32, _) = (1, 2)");
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
  test_fn("(x: i32, _) -> i32 = x", "(x: i32, _) -> i32 = x");
  test_fn("() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test("fn f() -> i32 = x", "fn f() -> i32 = x");
  test("fn f() -> i32 = x\n\nfn g() -> i32 = x", "fn f() -> i32 = x fn g() -> i32 = x");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
