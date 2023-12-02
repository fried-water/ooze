#include "test.h"

#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

namespace {

template <typename Parser>
void test(Parser p, std::string_view exp, std::string_view src) {
  Env e = create_primative_env();
  e.sm.push_back({"", std::string(src)});

  const auto [ast, tg] = check_result(p({}, std::move(e.tg), SrcID{1}, src).and_then([&](AST ast, TypeGraph tg) {
    return type_name_resolution(e.sm, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
      return std::tuple(std::move(ast), std::move(tg));
    });
  }));

  BOOST_CHECK_EQUAL(exp, pretty_print(e.sm, ast, tg));
}

void test_type(std::string_view exp, std::string_view src) {
  Env e = create_primative_env();
  e.sm.push_back({"", std::string(src)});
  const auto [ast, tg] =
    check_result(parse_type2({}, std::move(e.tg), SrcID{1}, src).and_then([&](AST ast, TypeGraph tg) {
      return type_name_resolution(e.sm, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    }));
  BOOST_CHECK_EQUAL(exp, pretty_print(e.sm, tg, TypeRef(tg.num_nodes() - 1)));
}

} // namespace

BOOST_AUTO_TEST_SUITE(pp)

BOOST_AUTO_TEST_CASE(pattern) {
  test(parse_pattern2, "abc", "abc");
  test(parse_pattern2, "_", "_");
  test(parse_pattern2, "(abc, _)", "(abc, _)");
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

BOOST_AUTO_TEST_CASE(native_fn) {
  Env e;
  e.type_cache = create_type_cache(e.tg);
  e.add_type<i32>("i32");
  e.add_function("f", [](i32) { return 1; });

  BOOST_CHECK_EQUAL("fn f(i32) -> i32 = <native_fn>",
                    pretty_print(e.sm, e.ast, e.tg, e.ast.forest.root_ids().get<1>()));
}

BOOST_AUTO_TEST_CASE(expr) {
  test(parse_expr2, "1", "1");
  test(parse_expr2, "1i16", "1i16");
  test(parse_expr2, "1.0f", "1.0f");
  test(parse_expr2, "1.0", "1.0");
  test(parse_expr2, "\"abc\"", "\"abc\"");
  test(parse_expr2, "abc", "abc");
  test(parse_expr2, "&abc", "&abc");

  test(parse_expr2, "(1, abc)", "(1, abc)");

  test(parse_expr2, "select x { y } else { z }", "select x { y } else { z }");

  test(parse_expr2, "f(1)", "f(1)");
}

BOOST_AUTO_TEST_CASE(assignment) {
  test(parse_assignment2, "let x = y", "let x = y");
  test(parse_assignment2, "let x = y", "let x : _ = y");
  test(parse_assignment2, "let x: i32 = y", "let x: i32 = y");
  test(parse_assignment2, "let (x, y): (i32, _) = (1, 2)", "let (x, y): (i32, _) = (1, 2)");
}

BOOST_AUTO_TEST_CASE(scope) {
  test(parse_expr2, "abc", "{ abc }");

  test(parse_expr2, "{\n  let x = y;\n  x\n}", "{ let x = y; x }");
  test(parse_expr2, "{\n  let x = y;\n  let y = z;\n  y\n}", "{ let x = y; let y = z; y }");
  test(parse_expr2, "{\n  let x = {\n    let y = z;\n    y\n  };\n  x\n}", "{ let x = { let y = z; y }; x }");
}

BOOST_AUTO_TEST_CASE(fn) {
  test(parse_function2, "() -> i32 = x", "() -> i32 = x");
  test(parse_function2, "(x) -> i32 = x", "(x) -> i32 = x");
  test(parse_function2, "(x: i32) -> i32 = x", "(x: i32) -> i32 = x");
  test(parse_function2, "(x: i32, _) -> i32 = x", "(x: i32, _) -> i32 = x");
  test(parse_function2, "() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test(parse2, "fn f() -> i32 = x", "fn f() -> i32 = x");
  test(parse2, "fn f() -> i32 = x\n\nfn g() -> i32 = x", "fn f() -> i32 = x fn g() -> i32 = x");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
