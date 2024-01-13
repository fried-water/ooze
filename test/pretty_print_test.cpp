#include "test.h"

#include "parser.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

namespace {

template <typename Parser>
void test(Parser p, std::string_view exp, std::string_view src) {
  const auto srcs = make_sv_array(src);
  const TypeNames names{{"T", TypeID{1}}};

  const auto [ast, tg] = check_result(p({}, {}, SrcID{0}, src).and_then([&](auto type_srcs, AST ast, TypeGraph tg) {
    return type_name_resolution(srcs, names, type_srcs, std::move(tg)).map_state([&](TypeGraph tg) {
      return std::tuple(std::move(ast), std::move(tg));
    });
  }));

  BOOST_CHECK_EQUAL(exp, pretty_print(srcs, ast, tg, names));
}

void test_type(std::string_view exp, std::string_view src) {
  const auto srcs = make_sv_array(src);
  const TypeNames names{{"T", TypeID{1}}};

  const auto [ast, tg] =
    check_result(parse_type({}, {}, SrcID{0}, src).and_then([&](auto type_srcs, AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, names, type_srcs, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    }));
  BOOST_CHECK_EQUAL(exp, pretty_print(srcs, tg, names, TypeRef(tg.num_nodes() - 1)));
}

} // namespace

BOOST_AUTO_TEST_SUITE(pp)

BOOST_AUTO_TEST_CASE(pattern) {
  test(parse_pattern, "abc", "abc");
  test(parse_pattern, "_", "_");
  test(parse_pattern, "(abc, _)", "(abc, _)");
}

BOOST_AUTO_TEST_CASE(compound_type) {
  test_type("_", "_");

  test_type("T", "T");
  test_type("&T", "&T");

  test_type("()", "()");
  test_type("(T)", "(T)");
  test_type("(T, T)", "(T, T)");

  test_type("fn() -> T", "fn() -> T");
  test_type("fn(T) -> T", "fn(T) -> T");
}

BOOST_AUTO_TEST_CASE(unnamed_type) {
  const auto int_type = type_id(knot::Type<int>{});

  TypeGraph g;
  const TypeRef id = g.add_node(TypeTag::Leaf, int_type);

  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id(knot::Type<int>{}).id), pretty_print({{}}, g, {}, id));
}

BOOST_AUTO_TEST_CASE(native_fn) {
  constexpr static std::string_view src = "f";

  AST ast;
  TypeGraph tg;

  const TypeRef t = tg.add_node(TypeTag::Leaf, TypeID{1});
  const TypeRef tuple_t = tg.add_node({t}, TypeTag::Tuple, TypeID{});
  const TypeRef fn_t = tg.add_node({tuple_t, t}, TypeTag::Fn, TypeID{});

  const ASTID global = *ast.forest.parent(add_global(ast, SrcRef{SrcID{0}, {0, 1}}, fn_t));

  BOOST_CHECK_EQUAL("let f: fn(T) -> T = <env_value>",
                    pretty_print(make_sv_array(src), ast, tg, {{"T", TypeID{1}}}, global));
}

BOOST_AUTO_TEST_CASE(expr) {
  test(parse_expr, "1", "1");
  test(parse_expr, "1i16", "1i16");
  test(parse_expr, "1.0f", "1.0f");
  test(parse_expr, "1.0", "1.0");
  test(parse_expr, "\"abc\"", "\"abc\"");
  test(parse_expr, "abc", "abc");
  test(parse_expr, "&abc", "&abc");

  test(parse_expr, "(1, abc)", "(1, abc)");

  test(parse_expr, "select x { y } else { z }", "select x { y } else { z }");

  test(parse_expr, "f(1)", "f(1)");
}

BOOST_AUTO_TEST_CASE(assignment) {
  test(parse_assignment, "let x = y", "let x = y");
  test(parse_assignment, "let x = y", "let x : _ = y");
  test(parse_assignment, "let x: T = y", "let x: T = y");
  test(parse_assignment, "let (x, y): (T, _) = (1, 2)", "let (x, y): (T, _) = (1, 2)");
}

BOOST_AUTO_TEST_CASE(scope) {
  test(parse_expr, "abc", "{ abc }");

  test(parse_expr, "{\n  let x = y;\n  x\n}", "{ let x = y; x }");
  test(parse_expr, "{\n  let x = y;\n  let y = z;\n  y\n}", "{ let x = y; let y = z; y }");
  test(parse_expr, "{\n  let x = {\n    let y = z;\n    y\n  };\n  x\n}", "{ let x = { let y = z; y }; x }");
}

BOOST_AUTO_TEST_CASE(fn) {
  test(parse_function, "() -> T = x", "() -> T = x");
  test(parse_function, "(x) -> T = x", "(x) -> T = x");
  test(parse_function, "(x: T) -> T = x", "(x: T) -> T = x");
  test(parse_function, "(x: T, _) -> T = x", "(x: T, _) -> T = x");
  test(parse_function, "() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test(parse, "fn f() -> T = x", "fn f() -> T = x");
  test(parse, "fn f() -> T = x\n\nfn g() -> T = x", "fn f() -> T = x fn g() -> T = x");
}

BOOST_AUTO_TEST_CASE(fn_type) {
  const auto srcs = make_sv_array("fn(T) -> T");
  const TypeNames names{{"T", TypeID{1}}};

  const TypeGraph tg =
    std::get<1>(check_result(parse_type({}, {}, SrcID{0}, srcs[0]).and_then([&](auto type_srcs, AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, names, type_srcs, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    })));

  BOOST_CHECK_EQUAL("(T) -> T", pretty_print_fn_type(srcs, tg, names, TypeRef(tg.num_nodes() - 1)));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
