#include "test.h"

#include "parser_flat.h"

namespace ooze {

namespace {

#define check_pass(_EXP_AST, _EXP_TYPES, _ACT)                                                                         \
  [](const AST& exp_ast, const ASTTypes& exp_types, ContextualResult<std::tuple<AST, ASTTypes>> result) {              \
    BOOST_REQUIRE(result);                                                                                             \
    const auto& [ast, types] = *result;                                                                                \
    if(exp_ast != ast) {                                                                                               \
      fmt::print("Actual:   {}\n", knot::debug(std::get<0>(result.value())));                                          \
      fmt::print("Expected: {}\n", knot::debug(exp_ast));                                                              \
      BOOST_CHECK(exp_ast == ast);                                                                                     \
    }                                                                                                                  \
    if(exp_types != types) {                                                                                           \
      fmt::print("Actual:   {}\n", knot::debug(std::get<1>(result.value())));                                          \
      fmt::print("Expected: {}\n", knot::debug(exp_types));                                                            \
      BOOST_CHECK(exp_types == types);                                                                                 \
    }                                                                                                                  \
  }(_EXP_AST, _EXP_TYPES, _ACT)

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

std::vector<Slice> slices(std::string_view src, const std::vector<std::string_view>& svs) {
  return transform_to_vec(svs, [&](std::string_view sv) {
    const auto pos = src.find(sv);
    BOOST_REQUIRE_NE(std::string_view::npos, pos);
    return Slice{int(pos), int(pos + sv.size())};
  });
}

Forest<ASTTag, ASTID> ast_forest(std::vector<std::vector<ASTTag>> vs) {
  Forest<ASTTag, ASTID> f;
  for(const auto& v : vs) {
    f.merge_path(v);
  }
  return f;
}

ASTTypes make_types(std::vector<TypeTag> tags,
                    std::vector<Slice> slices,
                    std::vector<std::vector<i32>> edges = {},
                    std::vector<TypeRef> ast_ids = {}) {
  Graph<TypeRef> g;

  if(edges.empty()) {
    for(int i = 0; i < tags.size(); i++) {
      g.add_node();
    }
  }
  for(const auto& fanout : edges) {
    g.add_node();
    for(i32 t : fanout) {
      g.add_fanout_to_last_node(TypeRef(t));
    }
  }

  return {std::move(g), std::move(tags), std::move(slices), std::move(ast_ids)};
}

ASTTypes types(size_t s) { return {{}, {}, {}, std::vector<TypeRef>(s)}; }

} // namespace

BOOST_AUTO_TEST_SUITE(parser2)

BOOST_AUTO_TEST_CASE(pattern_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {src})};
  check_pass(ast, types(1), parse_pattern2(src));
}

BOOST_AUTO_TEST_CASE(pattern_wildcard) {
  const std::string_view src = "_";
  const AST ast = {ast_forest({{ASTTag::PatternWildCard}}), slices(src, {src})};
  check_pass(ast, types(1), parse_pattern2("_"));
}

BOOST_AUTO_TEST_CASE(pattern_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::PatternTuple}}), slices(src, {src})};
  check_pass(ast, types(1), parse_pattern2("()"));
}

BOOST_AUTO_TEST_CASE(pattern_tuple1) {
  const std::string_view src = "(x)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", src})};
  check_pass(ast, types(2), parse_pattern2(src));
}

BOOST_AUTO_TEST_CASE(pattern_tuple2) {
  const std::string_view src = "(x, _)";
  const AST ast = {
    ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::PatternTuple, ASTTag::PatternWildCard}}),
    slices(src, {"x", "_", src})};
  check_pass(ast, types(3), parse_pattern2(src));
}

BOOST_AUTO_TEST_CASE(pattern_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternTuple}}), slices(src, {"()", src})};
  check_pass(ast, types(2), parse_pattern2(src));
}

BOOST_AUTO_TEST_CASE(type_ident) {
  const std::string_view src = "A";
  const ASTTypes types = make_types({TypeTag::Leaf}, slices(src, {src}));
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_floating) {
  const std::string_view src = "_";
  const ASTTypes types = make_types({TypeTag::Floating}, slices(src, {src}));
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_borrowed) {
  const std::string_view src = "&A";
  const ASTTypes types = make_types({TypeTag::Leaf, TypeTag::Borrow}, slices(src, {"A", src}), {{}, {0}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_borrowed_floating) {
  const std::string_view src = "&_";
  const ASTTypes types = make_types({TypeTag::Floating, TypeTag::Borrow}, slices(src, {"_", src}), {{}, {0}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_tuple) {
  const std::string_view src = "()";
  const ASTTypes types = make_types({TypeTag::Tuple}, slices(src, {src}));
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_tuple1) {
  const std::string_view src = "(A)";
  const ASTTypes types = make_types({TypeTag::Leaf, TypeTag::Tuple}, slices(src, {"A", src}), {{}, {0}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_tuple2) {
  const std::string_view src = "(A, _)";
  const ASTTypes types =
    make_types({TypeTag::Leaf, TypeTag::Floating, TypeTag::Tuple}, slices(src, {"A", "_", src}), {{}, {}, {0, 1}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_tuple_nested) {
  const std::string_view src = "(())";
  const ASTTypes types = make_types({TypeTag::Tuple, TypeTag::Tuple}, slices(src, {"()", src}), {{}, {0}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_fn) {
  const std::string_view src = "fn() -> A";
  const ASTTypes types =
    make_types({TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, slices(src, {"()", "A", src}), {{}, {}, {0, 1}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(type_fn_arg) {
  const std::string_view src = "fn(A) -> B";
  const ASTTypes types =
    make_types({TypeTag::Leaf, TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn},
               slices(src, {"A", "(A)", "B", src}),
               {{}, {0}, {}, {1, 2}});
  check_pass({}, types, parse_type2(src));
}

BOOST_AUTO_TEST_CASE(expr_literal) {
  const std::string_view src = "1";
  const AST ast = {ast_forest({{ASTTag::ExprLiteral}}), slices(src, {"1"}), {{ASTID{0}, 1}}};
  check_pass(ast, types(1), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {src})};
  check_pass(ast, types(1), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::ExprTuple}}), slices(src, {src})};
  check_pass(ast, types(1), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_tuple1) {
  const std::string_view src = "(a)";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}}), slices(src, {"a", src})};
  check_pass(ast, types(2), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_tuple2) {
  const std::string_view src = "(a, b)";
  auto f = ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"a", "b", src})};
  check_pass(ast, types(3), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprTuple}}), slices(src, {"()", src})};
  check_pass(ast, types(2), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_borrow) {
  const std::string_view src = "&x";
  const AST ast = {ast_forest({{ASTTag::ExprBorrow, ASTTag::ExprIdent}}), slices(src, {"x", src})};
  check_pass(ast, types(2), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call) {
  const std::string_view src = "f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   slices(src, {"f", "()", src})};
  check_pass(ast, types(3), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_arg) {
  const std::string_view src = "f(a)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"f", "a", "(a)", src})};
  check_pass(ast, types(4), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_call) {
  const std::string_view src = "f()()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprTuple},
                               {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   {{0, 1}, {1, 3}, {3, 5}, {0, 3}, {0, 5}}}; // TODO make post order

  check_pass(ast, types(5), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs) {
  const std::string_view src = "x.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"x", "f", "()", src})};
  check_pass(ast, types(4), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_literal) {
  const std::string_view src = "1.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"1", "f", "()", src}),
    {{ASTID{0}, 1}},
  };
  check_pass(ast, types(4), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_call) {
  const std::string_view src = "g().f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   {{0, 1}, {1, 3}, {0, 3}, {4, 5}, {5, 7}, {0, 7}}};
  check_pass(ast, types(6), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_arg) {
  const std::string_view src = "x.f(1)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"x", "f", "1", "(1)", src}),
    {{ASTID{2}, 1}},
  };
  check_pass(ast, types(5), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_chain) {
  const std::string_view src = "x.g().f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    {{0, 1}, {2, 3}, {3, 5}, {0, 5}, {6, 7}, {7, 9}, {0, 9}}};
  check_pass(ast, types(7), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_select) {
  const std::string_view src = "select x { y } else { z }";
  auto f = ast_forest({{ASTTag::ExprSelect, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"x", "y", "z", src})};
  check_pass(ast, types(4), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(binding_no_type) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"})};
  check_pass(ast, types(1), parse_binding2(src));
}

BOOST_AUTO_TEST_CASE(binding_ident) {
  const std::string_view src = "x: T";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"})};
  const ASTTypes types = make_types({TypeTag::Leaf}, slices(src, {"T"}), {}, {TypeRef{0}});
  check_pass(ast, types, parse_binding2(src));
}

BOOST_AUTO_TEST_CASE(binding_floating) {
  const std::string_view src = "x: _";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"})};
  const ASTTypes types = make_types({TypeTag::Floating}, slices(src, {"_"}), {}, {TypeRef{0}});
  check_pass(ast, types, parse_binding2(src));
}

BOOST_AUTO_TEST_CASE(binding_tuple) {
  const std::string_view src = "(): ()";
  const AST ast = {ast_forest({{ASTTag::PatternTuple}}), {{0, 2}}};
  const ASTTypes types = make_types({TypeTag::Tuple}, {{4, 6}}, {}, {TypeRef{0}});
  check_pass(ast, types, parse_binding2(src));
}

BOOST_AUTO_TEST_CASE(binding_tuple_arg) {
  const std::string_view src = "(x): (T)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", "(x)"})};
  const ASTTypes types =
    make_types({TypeTag::Leaf, TypeTag::Tuple}, slices(src, {"T", "(T)"}), {{}, {0}}, {TypeRef{-1}, TypeRef{1}});
  check_pass(ast, types, parse_binding2(src));
}

BOOST_AUTO_TEST_CASE(assignment) {
  const std::string_view src = "let x: T = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y"})};
  const ASTTypes types = make_types({TypeTag::Leaf}, slices(src, {"T"}), {}, {TypeRef{0}, TypeRef{-1}, TypeRef{-1}});
  check_pass(ast, types, parse_assignment2(src));
}

BOOST_AUTO_TEST_CASE(assignment_no_type) {
  const std::string_view src = "let x = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", src})};
  check_pass(ast, types(3), parse_assignment2(src));
}

BOOST_AUTO_TEST_CASE(expr_scope) {
  const std::string_view src = "{ x }";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {"x"})};
  check_pass(ast, types(1), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign) {
  const std::string_view src = "{ let x = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x = y", "z", "let x = y; z"})};
  check_pass(ast, types(5), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign_type) {
  const std::string_view src = "{ let x: T = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y", "z", "let x: T = y; z"})};
  const ASTTypes types = make_types(
    {TypeTag::Leaf}, slices(src, {"T"}), {}, {TypeRef{0}, TypeRef{-1}, TypeRef{-1}, TypeRef{-1}, TypeRef{-1}});
  check_pass(ast, types, parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign2) {
  const std::string_view src = "{ let v = w; let x = y; z }";
  const AST ast = {
    ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::ExprIdent}}),
    slices(src, {"v", "w", "let v = w", "x", "y", "let x = y", "z", "let x = y; z", "let v = w; let x = y; z"})};
  check_pass(ast, types(9), parse_expr2(src));
}

BOOST_AUTO_TEST_CASE(fn) {
  const std::string_view src = "() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   slices(src, {"()", "x", "() -> T = x"})};
  const ASTTypes types = make_types({TypeTag::Leaf}, slices(src, {"T"}), {}, {TypeRef{-1}, TypeRef{0}, TypeRef{-1}});
  check_pass(ast, types, parse_function2(src));
}

BOOST_AUTO_TEST_CASE(fn_one_arg) {
  const std::string_view src = "(x: T1) -> T2 = y";
  const AST ast = {
    ast_forest({{ASTTag::Fn, ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::Fn, ASTTag::ExprIdent}}),
    slices(src, {"x", "(x: T1)", "y", "(x: T1) -> T2 = y"})};
  const ASTTypes types =
    make_types({TypeTag::Leaf, TypeTag::Leaf},
               slices(src, {"T1", "T2"}),
               {{}, {}},
               {TypeRef{0}, TypeRef{-1}, TypeRef{1}, TypeRef{-1}});
  check_pass(ast, types, parse_function2(src));
}

BOOST_AUTO_TEST_CASE(fn_return_fn) {
  const std::string_view src = "() -> fn() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   {{0, 2}, {18, 19}, {0, 19}}};
  const ASTTypes types =
    make_types({TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn},
               {{8, 10}, {14, 15}, {6, 15}},
               {{}, {}, {0, 1}},
               {TypeRef{-1}, TypeRef{2}, TypeRef{-1}});
  check_pass(ast, types, parse_function2(src));
}

BOOST_AUTO_TEST_CASE(ast_empty) { check_pass({}, {}, parse2("")); }

BOOST_AUTO_TEST_CASE(ast_fn) {
  const std::string_view src = "fn f() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::Assignment, ASTTag::Fn, ASTTag::PatternTuple},
                               {ASTTag::Assignment, ASTTag::Fn, ASTTag::ExprIdent}}),
                   {{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}}};
  const ASTTypes types = make_types(
    {TypeTag::Leaf}, slices(src, {"T"}), {}, {TypeRef{-1}, TypeRef{-1}, TypeRef{0}, TypeRef{-1}, TypeRef{-1}});
  check_pass(ast, types, parse2(src));
}

BOOST_AUTO_TEST_CASE(ast_multiple_fn) {
  const std::string_view src = "fn f() -> T = x fn g() -> T = x";
  Forest<ASTTag, ASTID> f =
    ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::PatternTuple},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::ExprIdent}});

  const ASTID f2 = f.append_root(ASTTag::Assignment);
  f.merge_path(f2, std::array{ASTTag::PatternIdent});
  f.merge_path(f2, std::array{ASTTag::Fn, ASTTag::PatternTuple});
  f.merge_path(f2, std::array{ASTTag::Fn, ASTTag::ExprIdent});
  const AST ast = {std::move(f),
                   {{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}, {19, 20}, {20, 22}, {30, 31}, {20, 31}, {16, 31}}};

  const ASTTypes types = make_types(
    {TypeTag::Leaf, TypeTag::Leaf},
    {{10, 11}, {26, 27}},
    {{}, {}},
    {TypeRef{-1},
     TypeRef{-1},
     TypeRef{0},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{1},
     TypeRef{-1},
     TypeRef{-1}});
  check_pass(ast, types, parse2(src));
}

BOOST_AUTO_TEST_CASE(no_fn) { check_single_error({{0, 1}, "expected 'fn'"}, parse2("f")); }
BOOST_AUTO_TEST_CASE(no_fn2) { check_single_error({{0, 1}, "expected 'fn'"}, parse2("a")); }
BOOST_AUTO_TEST_CASE(no_fn3) { check_single_error({{0, 1}, "expected 'fn'"}, parse2(")")); }

BOOST_AUTO_TEST_CASE(bad_paren) { check_single_error({{2, 3}, "expected token" /* ? */}, parse2("fn)")); }

BOOST_AUTO_TEST_CASE(no_expr) { check_single_error({{1, 2}, "expected 'let'" /* ? */}, parse_expr2("{}")); }

BOOST_AUTO_TEST_CASE(no_return_type) {
  check_single_error({{6, 7}, "expected '&'" /* ? */}, parse_function2("() -> { 1 }"));
}

BOOST_AUTO_TEST_CASE(fn_no_tupl) { check_single_error({{3, 4}, "expected '('"}, parse_type2("fn T -> T")); }

BOOST_AUTO_TEST_CASE(no_return) { check_single_error({{7, 8}, "expected '->'" /* ? */}, parse2("fn f() { 1 }")); }

BOOST_AUTO_TEST_CASE(no_params) { check_single_error({{5, 7}, "expected '('" /* ? */}, parse2("fn f -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(no_fn_name) { check_single_error({{3, 4}, "expected token" /* ? */}, parse2("fn () -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(bad_fn_name) {
  check_single_error({{3, 4}, "expected token" /* ? */}, parse2("fn 1() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(no_fn_keyword) { check_single_error({{0, 1}, "expected 'fn'"}, parse2("f() -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(select_no_scope) {
  check_single_error({{9, 10}, "expected '{'"}, parse_expr2("select a 1 else 1"));
}

BOOST_AUTO_TEST_CASE(no_scope) { check_single_error({{11, 11}, "expected '='"}, parse2("fn f() -> T")); }

BOOST_AUTO_TEST_CASE(unclosed_paren) { check_single_error({{6, 8}, "expected ')'"}, parse2("fn f( -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(unopened_paren) { check_single_error({{4, 5}, "expected '('"}, parse2("fn f) -> T { 1 }")); }

BOOST_AUTO_TEST_CASE(untyped_paren2) {
  check_single_error({{9, 10}, "expected '&'" /* ? */}, parse2("fn f(a : ) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(bad_type_paren) {
  check_single_error({{9, 10}, "expected '&'" /* ? */}, parse2("fn f(a : 1) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(expr_unclosed) { check_single_error({{17, 18}, "expected ')'"}, parse2("fn f() -> T { a( }")); }

BOOST_AUTO_TEST_CASE(expr_unopened) { check_single_error({{15, 16}, "expected '}'"}, parse2("fn f() -> T { a) }")); }

BOOST_AUTO_TEST_CASE(expr_bad_comma) {
  check_single_error({{18, 19}, "expected literal" /* ? */}, parse2("fn f() -> T { a(1,) }"));
}

BOOST_AUTO_TEST_CASE(bad_chain) { check_single_error({{18, 19}, "expected '('"}, parse2("fn f() -> T { a.b }")); }

BOOST_AUTO_TEST_CASE(bad_chain2) { check_single_error({{18, 19}, "expected '('"}, parse2("fn f() -> T { a.1 }")); }

BOOST_AUTO_TEST_CASE(bad_assignment) {
  check_single_error({{18, 19}, "expected token" /* ? */}, parse2("fn f() -> T { let }"));
}

BOOST_AUTO_TEST_CASE(let_no_var) {
  check_single_error({{18, 19}, "expected token" /* ? */}, parse2("fn f() -> T { let = 1; }"));
}

BOOST_AUTO_TEST_CASE(let_no_expr) {
  check_single_error({{22, 23}, "expected literal"}, parse2("fn f() -> T { let x = }"));
}

BOOST_AUTO_TEST_CASE(assignment_no_expr) {
  check_single_error({{25, 26}, "expected 'let'"}, parse2("fn f() -> T { let x = 0; }"));
}

BOOST_AUTO_TEST_CASE(bad_second_fn) {
  check_single_error({{20, 20}, "expected token"}, parse2("fn f() -> T { 1 } fn"));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
