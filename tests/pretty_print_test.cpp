#include "test.h"

#include "frontend_helpers.h"
#include "parser.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

namespace {

template <typename Parser>
void test(Parser p, std::string_view exp, std::string_view src) {
  const auto srcs = make_sv_array(src);
  const TypeNames names{{"T", TypeID{1}}};
  const auto [roots, ast] = check_result(parse_and_name_resolution(p, srcs, names, {}, SrcID{0}));
  BOOST_CHECK_EQUAL(exp, pretty_print(srcs, ast, names));
}

void test_type(std::string_view exp, std::string_view src) {
  const TypeNames names{{"T", TypeID{1}}};
  const auto [type, ast] = check_result(parse_and_name_resolution(parse_type, make_sv_array(src), names, {}, SrcID{0}));
  BOOST_CHECK_EQUAL(exp, pretty_print(ast.tg, names, type));
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
  const Type id = g.add_node(TypeTag::Leaf, int_type);

  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id(knot::Type<int>{}).id), pretty_print(g, {}, id));
}

BOOST_AUTO_TEST_CASE(native_fn) {
  constexpr static std::string_view src = "f";

  AST ast;

  const Type unit = ast.tg.add_node(TypeTag::Tuple, TypeID{});
  const Type t = ast.tg.add_node(TypeTag::Leaf, TypeID{1});
  const Type tuple_t = ast.tg.add_node(std::array{t}, TypeTag::Tuple, TypeID{});
  const Type fn_t = ast.tg.add_node(std::array{tuple_t, t}, TypeTag::Fn, TypeID{});

  const SrcRef ref = {SrcID{0}, {0, 1}};

  const ASTID ident_id = append_root(ast, ASTTag::PatternIdent, ref, fn_t);
  const ASTID fn_id = append_root(ast, ASTTag::EnvValue, ref, fn_t);
  const ASTID global = append_root(ast, ASTTag::Assignment, SrcRef{}, unit, std::array{ident_id, fn_id});

  BOOST_CHECK_EQUAL("let f: fn(T) -> T = <env_value>",
                    pretty_print(make_sv_array(src), ast, {{"T", TypeID{1}}}, global));
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

  test(parse_expr, "if x { y } else { z }", "if x { y } else { z }");

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
  test(parse_fn, "() -> T = x", "() -> T = x");
  test(parse_fn, "(x) -> T = x", "(x) -> T = x");
  test(parse_fn, "(x: T) -> T = x", "(x: T) -> T = x");
  test(parse_fn, "(x: T, _) -> T = x", "(x: T, _) -> T = x");
  test(parse_fn, "() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test(parse, "fn f() -> T = x", "fn f() -> T = x");
  test(parse, "fn f() -> T = x\n\nfn g() -> T = x", "fn f() -> T = x fn g() -> T = x");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
