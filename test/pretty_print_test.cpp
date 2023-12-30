#include "test.h"

#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"

namespace ooze {

namespace {

template <typename Parser>
void test(Parser p, std::string_view exp, std::string_view src) {
  const auto srcs = make_sv_array(src);
  const std::unordered_map<std::string, TypeID> type_ids = {{"T", TypeID{1}}};

  const auto [ast, tg] = check_result(p({}, {}, SrcID{0}, src).and_then([&](AST ast, TypeGraph tg) {
    return type_name_resolution(srcs, type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
      return std::tuple(std::move(ast), std::move(tg));
    });
  }));

  BOOST_CHECK_EQUAL(exp, pretty_print(srcs, ast, tg));
}

void test_type(std::string_view exp, std::string_view src) {
  const auto srcs = make_sv_array(src);
  const std::unordered_map<std::string, TypeID> type_ids = {{"T", TypeID{1}}};

  const auto [ast, tg] = check_result(parse_type2({}, {}, SrcID{0}, src).and_then([&](AST ast, TypeGraph tg) {
    return type_name_resolution(srcs, type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
      return std::tuple(std::move(ast), std::move(tg));
    });
  }));
  BOOST_CHECK_EQUAL(exp, pretty_print(srcs, tg, TypeRef(tg.num_nodes() - 1)));
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
  const TypeRef id = g.add_node(TypeTag::Leaf, {SrcID{0}}, int_type);

  BOOST_CHECK_EQUAL(fmt::format("type 0x{:x}", type_id(knot::Type<int>{}).id), pretty_print({{}}, g, id));
}

BOOST_AUTO_TEST_CASE(native_fn) {
  constexpr static std::string_view src = "Tf";

  AST ast;
  TypeGraph tg;

  const TypeRef t = tg.add_node(TypeTag::Leaf, SrcRef{SrcID{0}, {0, 1}}, TypeID{});
  const TypeRef tuple_t = tg.add_node({t}, TypeTag::Tuple, SrcRef{}, TypeID{});
  const TypeRef fn_t = tg.add_node({tuple_t, t}, TypeTag::Fn, SrcRef{}, TypeID{});

  const ASTID ident = ast.forest.append_root(ASTTag::PatternIdent);
  const ASTID fn = ast.forest.append_root(ASTTag::EnvValue);
  const ASTID global = ast.forest.append_root_post_order(ASTTag::Global, std::array{ident, fn});

  ast.srcs.push_back(SrcRef{SrcID{0}, {1, 2}});
  ast.srcs.push_back(SrcRef{SrcID{0}, {1, 2}});
  ast.srcs.push_back(SrcRef{});

  ast.types.push_back(fn_t);
  ast.types.push_back(fn_t);
  ast.types.push_back(TypeRef::Invalid());

  BOOST_CHECK_EQUAL("fn f(T) -> T = <native_fn>", pretty_print(make_sv_array(src), ast, tg, global));
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
  test(parse_assignment2, "let x: T = y", "let x: T = y");
  test(parse_assignment2, "let (x, y): (T, _) = (1, 2)", "let (x, y): (T, _) = (1, 2)");
}

BOOST_AUTO_TEST_CASE(scope) {
  test(parse_expr2, "abc", "{ abc }");

  test(parse_expr2, "{\n  let x = y;\n  x\n}", "{ let x = y; x }");
  test(parse_expr2, "{\n  let x = y;\n  let y = z;\n  y\n}", "{ let x = y; let y = z; y }");
  test(parse_expr2, "{\n  let x = {\n    let y = z;\n    y\n  };\n  x\n}", "{ let x = { let y = z; y }; x }");
}

BOOST_AUTO_TEST_CASE(fn) {
  test(parse_function2, "() -> T = x", "() -> T = x");
  test(parse_function2, "(x) -> T = x", "(x) -> T = x");
  test(parse_function2, "(x: T) -> T = x", "(x: T) -> T = x");
  test(parse_function2, "(x: T, _) -> T = x", "(x: T, _) -> T = x");
  test(parse_function2, "() -> _ {\n  let x = y;\n  x\n}", "() -> _ { let x = y; x }");
}

BOOST_AUTO_TEST_CASE(ast) {
  test(parse2, "fn f() -> T = x", "fn f() -> T = x");
  test(parse2, "fn f() -> T = x\n\nfn g() -> T = x", "fn f() -> T = x fn g() -> T = x");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
