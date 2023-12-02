#include "test.h"

#include "parser_flat.h"

namespace ooze {

namespace {

#define check_pass(_EXP_AST, _EXP_TYPES, _ACT)                                                                         \
  [](const AST& exp_ast, const TypeGraph& exp_tg, ContextualResult2<void, AST, TypeGraph> result) {                    \
    BOOST_REQUIRE(result.has_value());                                                                                 \
                                                                                                                       \
    const auto& [ast, tg] = result.state();                                                                            \
    if(exp_ast != ast) {                                                                                               \
      fmt::print("Actual:   {}\n", knot::debug(ast));                                                                  \
      fmt::print("Expected: {}\n", knot::debug(exp_ast));                                                              \
      BOOST_CHECK(exp_ast == ast);                                                                                     \
    }                                                                                                                  \
    if(exp_tg != tg) {                                                                                                 \
      fmt::print("Actual:   {}\n", knot::debug(tg));                                                                   \
      fmt::print("Expected: {}\n", knot::debug(exp_tg));                                                               \
      BOOST_CHECK(exp_tg == tg);                                                                                       \
    }                                                                                                                  \
  }(_EXP_AST, _EXP_TYPES, _ACT)

template <typename T, typename... Ts>
void check_single_error(ContextualError2 expected, ContextualResult2<T, Ts...> result) {
  const std::vector<ContextualError2> err = check_error(std::move(result));
  BOOST_REQUIRE(err.size() == 1);

  if(expected != err.front()) {
    fmt::print("Actual:   {}\n", knot::debug(err.front()));
    fmt::print("Expected: {}\n", knot::debug(expected));
    BOOST_CHECK(false);
  }
}

std::vector<SrcRef> slices(std::string_view src, const std::vector<std::string_view>& svs) {
  return transform_to_vec(svs, [&](std::string_view sv) {
    const auto pos = src.find(sv);
    BOOST_REQUIRE_NE(std::string_view::npos, pos);
    return SrcRef{SrcID::Invalid(), Slice{int(pos), int(pos + sv.size())}};
  });
}

std::vector<SrcRef> as_invalid_refs(const std::vector<Slice>& slices) {
  return transform_to_vec(slices, [&](Slice s) { return SrcRef{SrcID::Invalid(), s}; });
}

Forest<ASTTag, ASTID> ast_forest(std::vector<std::vector<ASTTag>> vs) {
  Forest<ASTTag, ASTID> f;
  for(const auto& v : vs) {
    f.merge_path(v);
  }
  return f;
}

TypeGraph make_tg(std::vector<TypeTag> tags, std::vector<SrcRef> slices, std::vector<std::vector<i32>> edges = {}) {
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

  const size_t size = tags.size();

  return std::move(g).append_column(std::move(tags), std::move(slices), std::vector<TypeID>(size));
}

std::vector<TypeRef> types(size_t s) { return std::vector<TypeRef>(s); }

} // namespace

BOOST_AUTO_TEST_SUITE(parser2)

BOOST_AUTO_TEST_CASE(pattern_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {src}), types(1)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(pattern_wildcard) {
  const std::string_view src = "_";
  const AST ast = {ast_forest({{ASTTag::PatternWildCard}}), slices(src, {src}), types(1)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, "_"));
}

BOOST_AUTO_TEST_CASE(pattern_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::PatternTuple}}), slices(src, {src}), types(1)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, "()"));
}

BOOST_AUTO_TEST_CASE(pattern_tuple1) {
  const std::string_view src = "(x)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", src}), types(2)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(pattern_tuple2) {
  const std::string_view src = "(x, _)";
  const AST ast = {
    ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::PatternTuple, ASTTag::PatternWildCard}}),
    slices(src, {"x", "_", src}),
    types(3)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(pattern_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternTuple}}), slices(src, {"()", src}), types(2)};
  check_pass(ast, {}, parse_pattern2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_ident) {
  const std::string_view src = "A";
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {src}));
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_floating) {
  const std::string_view src = "_";
  const TypeGraph tg = make_tg({TypeTag::Floating}, slices(src, {src}));
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_borrowed) {
  const std::string_view src = "&A";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Borrow}, slices(src, {"A", src}), {{}, {0}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_borrowed_floating) {
  const std::string_view src = "&_";
  const TypeGraph tg = make_tg({TypeTag::Floating, TypeTag::Borrow}, slices(src, {"_", src}), {{}, {0}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_tuple) {
  const std::string_view src = "()";
  const TypeGraph tg = make_tg({TypeTag::Tuple}, slices(src, {src}));
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_tuple1) {
  const std::string_view src = "(A)";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple}, slices(src, {"A", src}), {{}, {0}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_tuple2) {
  const std::string_view src = "(A, _)";
  const TypeGraph tg =
    make_tg({TypeTag::Leaf, TypeTag::Floating, TypeTag::Tuple}, slices(src, {"A", "_", src}), {{}, {}, {0, 1}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_tuple_nested) {
  const std::string_view src = "(())";
  const TypeGraph tg = make_tg({TypeTag::Tuple, TypeTag::Tuple}, slices(src, {"()", src}), {{}, {0}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_fn) {
  const std::string_view src = "fn() -> A";
  const TypeGraph tg =
    make_tg({TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, slices(src, {"()", "A", src}), {{}, {}, {0, 1}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(type_fn_arg) {
  const std::string_view src = "fn(A) -> B";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn},
                               slices(src, {"A", "(A)", "B", src}),
                               {{}, {0}, {}, {1, 2}});
  check_pass({}, tg, parse_type2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_literal) {
  const std::string_view src = "1";
  const AST ast = {ast_forest({{ASTTag::ExprLiteral}}), slices(src, {"1"}), types(1), {{ASTID{0}, 1}}};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {src}), types(1)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::ExprTuple}}), slices(src, {src}), types(1)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_tuple1) {
  const std::string_view src = "(a)";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}}), slices(src, {"a", src}), types(2)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_tuple2) {
  const std::string_view src = "(a, b)";
  auto f = ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"a", "b", src}), types(3)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprTuple}}), slices(src, {"()", src}), types(2)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_borrow) {
  const std::string_view src = "&x";
  const AST ast = {ast_forest({{ASTTag::ExprBorrow, ASTTag::ExprIdent}}), slices(src, {"x", src}), types(2)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call) {
  const std::string_view src = "f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   slices(src, {"f", "()", src}),
                   types(3)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_arg) {
  const std::string_view src = "f(a)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"f", "a", "(a)", src}),
    types(4)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_call) {
  const std::string_view src = "f()()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprTuple},
                               {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   as_invalid_refs({{0, 1}, {1, 3}, {3, 5}, {0, 3}, {0, 5}}),
                   types(5)}; // TODO make post order

  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs) {
  const std::string_view src = "x.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"x", "f", "()", src}),
    types(4)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_literal) {
  const std::string_view src = "1.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"1", "f", "()", src}),
    types(4),
    {{ASTID{0}, 1}},
  };
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_call) {
  const std::string_view src = "g().f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   as_invalid_refs({{0, 1}, {1, 3}, {0, 3}, {4, 5}, {5, 7}, {0, 7}}),
                   types(6)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_arg) {
  const std::string_view src = "x.f(1)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"x", "f", "1", "(1)", src}),
    types(5),
    {{ASTID{2}, 1}},
  };
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_chain) {
  const std::string_view src = "x.g().f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    as_invalid_refs({{0, 1}, {2, 3}, {3, 5}, {0, 5}, {6, 7}, {7, 9}, {0, 9}}),
    types(7)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_select) {
  const std::string_view src = "select x { y } else { z }";
  auto f = ast_forest({{ASTTag::ExprSelect, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"x", "y", "z", src}), types(4)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(binding_no_type) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), types(1)};
  check_pass(ast, {}, parse_binding2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(binding_ident) {
  const std::string_view src = "x: T";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), {TypeRef{0}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {"T"}));
  check_pass(ast, tg, parse_binding2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(binding_floating) {
  const std::string_view src = "x: _";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), {TypeRef{0}}};
  const TypeGraph tg = make_tg({TypeTag::Floating}, slices(src, {"_"}), {});
  check_pass(ast, tg, parse_binding2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(binding_tuple) {
  const std::string_view src = "(): ()";
  const AST ast = {ast_forest({{ASTTag::PatternTuple}}), as_invalid_refs({{0, 2}}), {TypeRef{0}}};
  const TypeGraph tg = make_tg({TypeTag::Tuple}, as_invalid_refs({{4, 6}}));
  check_pass(ast, tg, parse_binding2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(binding_tuple_arg) {
  const std::string_view src = "(x): (T)";
  const AST ast = {
    ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", "(x)"}), {TypeRef{-1}, TypeRef{1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple}, slices(src, {"T", "(T)"}), {{}, {0}});
  check_pass(ast, tg, parse_binding2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(assignment) {
  const std::string_view src = "let x: T = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y"}),
                   {TypeRef{0}, TypeRef{-1}, TypeRef{-1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {"T"}));
  check_pass(ast, tg, parse_assignment2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(assignment_no_type) {
  const std::string_view src = "let x = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", src}),
                   types(3)};
  check_pass(ast, {}, parse_assignment2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_scope) {
  const std::string_view src = "{ x }";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {"x"}), types(1)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign) {
  const std::string_view src = "{ let x = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x = y", "z", "let x = y; z"}),
                   types(5)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign_type) {
  const std::string_view src = "{ let x: T = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y", "z", "let x: T = y; z"}),
                   {TypeRef{0}, TypeRef{-1}, TypeRef{-1}, TypeRef{-1}, TypeRef{-1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {"T"}), {});
  check_pass(ast, tg, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(expr_scope_assign2) {
  const std::string_view src = "{ let v = w; let x = y; z }";
  const AST ast = {
    ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                {ASTTag::ExprWith, ASTTag::ExprWith, ASTTag::ExprIdent}}),
    slices(src, {"v", "w", "let v = w", "x", "y", "let x = y", "z", "let x = y; z", "let v = w; let x = y; z"}),
    types(9)};
  check_pass(ast, {}, parse_expr2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(fn) {
  const std::string_view src = "() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   slices(src, {"()", "x", "() -> T = x"}),
                   {TypeRef{-1}, TypeRef{0}, TypeRef{-1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {"T"}), {});
  check_pass(ast, tg, parse_function2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(fn_one_arg) {
  const std::string_view src = "(x: T1) -> T2 = y";
  const AST ast = {
    ast_forest({{ASTTag::Fn, ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::Fn, ASTTag::ExprIdent}}),
    slices(src, {"x", "(x: T1)", "y", "(x: T1) -> T2 = y"}),
    {TypeRef{0}, TypeRef{-1}, TypeRef{1}, TypeRef{-1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Leaf}, slices(src, {"T1", "T2"}), {{}, {}});
  check_pass(ast, tg, parse_function2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(fn_return_fn) {
  const std::string_view src = "() -> fn() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   as_invalid_refs({{0, 2}, {18, 19}, {0, 19}}),
                   {TypeRef{-1}, TypeRef{2}, TypeRef{-1}}};
  const TypeGraph tg = make_tg(
    {TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, as_invalid_refs({{8, 10}, {14, 15}, {6, 15}}), {{}, {}, {0, 1}});
  check_pass(ast, tg, parse_function2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(ast_empty) { check_pass({}, {}, parse2({}, {}, {}, "")); }

BOOST_AUTO_TEST_CASE(ast_fn) {
  const std::string_view src = "fn f() -> T = x";
  const AST ast = {ast_forest({{ASTTag::RootFn, ASTTag::PatternIdent},
                               {ASTTag::RootFn, ASTTag::Fn, ASTTag::PatternTuple},
                               {ASTTag::RootFn, ASTTag::Fn, ASTTag::ExprIdent}}),
                   as_invalid_refs({{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}}),
                   {TypeRef{-1}, TypeRef{-1}, TypeRef{0}, TypeRef{-1}, TypeRef{-1}}};
  const TypeGraph tg = make_tg({TypeTag::Leaf}, slices(src, {"T"}), {});
  check_pass(ast, tg, parse2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(ast_multiple_fn) {
  const std::string_view src = "fn f() -> T = x fn g() -> T = x";
  Forest<ASTTag, ASTID> f =
    ast_forest({{ASTTag::RootFn, ASTTag::PatternIdent},
                {ASTTag::RootFn, ASTTag::Fn, ASTTag::PatternTuple},
                {ASTTag::RootFn, ASTTag::Fn, ASTTag::ExprIdent}});

  const ASTID f2 = f.append_root(ASTTag::RootFn);
  f.merge_path(f2, std::array{ASTTag::PatternIdent});
  f.merge_path(f2, std::array{ASTTag::Fn, ASTTag::PatternTuple});
  f.merge_path(f2, std::array{ASTTag::Fn, ASTTag::ExprIdent});
  const AST ast = {
    std::move(f),
    as_invalid_refs({{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}, {19, 20}, {20, 22}, {30, 31}, {20, 31}, {16, 31}}),
    {TypeRef{-1},
     TypeRef{-1},
     TypeRef{0},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{-1},
     TypeRef{1},
     TypeRef{-1},
     TypeRef{-1}}};

  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Leaf}, as_invalid_refs({{10, 11}, {26, 27}}), {{}, {}});
  check_pass(ast, tg, parse2({}, {}, {}, src));
}

BOOST_AUTO_TEST_CASE(parse_consecutive) {
  AST ast;
  TypeGraph tg;
  std::tie(ast, tg) = check_result(parse2({}, {}, SrcID{0}, "fn f() -> T = x"));
  std::tie(ast, tg) = check_result(parse2(std::move(ast), std::move(tg), SrcID{0}, "fn g() -> T = y"));

  auto [exp_ast, exp_tg] = check_result(parse2({}, {}, SrcID{0}, "fn f() -> T = x fn g() -> T = y"));

  ast.srcs = {};
  exp_ast.srcs = {};

  tg.set(TypeRef{1}, SrcRef{});
  exp_tg.set(TypeRef{1}, SrcRef{});

  BOOST_CHECK(exp_ast == ast);
  BOOST_CHECK(exp_tg == tg);
}

BOOST_AUTO_TEST_CASE(no_fn) { check_single_error({{{}, {0, 1}}, "expected 'fn'"}, parse2({}, {}, {}, "f")); }
BOOST_AUTO_TEST_CASE(no_fn2) { check_single_error({{{}, {0, 1}}, "expected 'fn'"}, parse2({}, {}, {}, "a")); }
BOOST_AUTO_TEST_CASE(no_fn3) { check_single_error({{{}, {0, 1}}, "expected 'fn'"}, parse2({}, {}, {}, ")")); }

BOOST_AUTO_TEST_CASE(bad_paren) {
  check_single_error({{{}, {2, 3}}, "expected token" /* ? */}, parse2({}, {}, {}, "fn)"));
}

BOOST_AUTO_TEST_CASE(no_expr) {
  check_single_error({{{}, {1, 2}}, "expected 'let'" /* ? */}, parse_expr2({}, {}, {}, "{}"));
}

BOOST_AUTO_TEST_CASE(no_return_type) {
  check_single_error({{{}, {6, 7}}, "expected '&'" /* ? */}, parse_function2({}, {}, {}, "() -> { 1 }"));
}

BOOST_AUTO_TEST_CASE(fn_no_tupl) {
  check_single_error({{{}, {3, 4}}, "expected '('"}, parse_type2({}, {}, {}, "fn T -> T"));
}

BOOST_AUTO_TEST_CASE(no_return) {
  check_single_error({{{}, {7, 8}}, "expected '->'" /* ? */}, parse2({}, {}, {}, "fn f() { 1 }"));
}

BOOST_AUTO_TEST_CASE(no_params) {
  check_single_error({{{}, {5, 7}}, "expected '('" /* ? */}, parse2({}, {}, {}, "fn f -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(no_fn_name) {
  check_single_error({{{}, {3, 4}}, "expected token" /* ? */}, parse2({}, {}, {}, "fn () -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(bad_fn_name) {
  check_single_error({{{}, {3, 4}}, "expected token" /* ? */}, parse2({}, {}, {}, "fn 1() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(no_fn_keyword) {
  check_single_error({{{}, {0, 1}}, "expected 'fn'"}, parse2({}, {}, {}, "f() -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(select_no_scope) {
  check_single_error({{{}, {9, 10}}, "expected '{'"}, parse_expr2({}, {}, {}, "select a 1 else 1"));
}

BOOST_AUTO_TEST_CASE(no_scope) {
  check_single_error({{{}, {11, 11}}, "expected '='"}, parse2({}, {}, {}, "fn f() -> T"));
}

BOOST_AUTO_TEST_CASE(unclosed_paren) {
  check_single_error({{{}, {6, 8}}, "expected ')'"}, parse2({}, {}, {}, "fn f( -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(unopened_paren) {
  check_single_error({{{}, {4, 5}}, "expected '('"}, parse2({}, {}, {}, "fn f) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(untyped_paren2) {
  check_single_error({{{}, {9, 10}}, "expected '&'" /* ? */}, parse2({}, {}, {}, "fn f(a : ) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(bad_type_paren) {
  check_single_error({{{}, {9, 10}}, "expected '&'" /* ? */}, parse2({}, {}, {}, "fn f(a : 1) -> T { 1 }"));
}

BOOST_AUTO_TEST_CASE(expr_unclosed) {
  check_single_error({{{}, {17, 18}}, "expected ')'"}, parse2({}, {}, {}, "fn f() -> T { a( }"));
}

BOOST_AUTO_TEST_CASE(expr_unopened) {
  check_single_error({{{}, {15, 16}}, "expected '}'"}, parse2({}, {}, {}, "fn f() -> T { a) }"));
}

BOOST_AUTO_TEST_CASE(expr_bad_comma) {
  check_single_error({{{}, {18, 19}}, "expected literal" /* ? */}, parse2({}, {}, {}, "fn f() -> T { a(1,) }"));
}

BOOST_AUTO_TEST_CASE(bad_chain) {
  check_single_error({{{}, {18, 19}}, "expected '('"}, parse2({}, {}, {}, "fn f() -> T { a.b }"));
}

BOOST_AUTO_TEST_CASE(bad_chain2) {
  check_single_error({{{}, {18, 19}}, "expected '('"}, parse2({}, {}, {}, "fn f() -> T { a.1 }"));
}

BOOST_AUTO_TEST_CASE(bad_assignment) {
  check_single_error({{{}, {18, 19}}, "expected token" /* ? */}, parse2({}, {}, {}, "fn f() -> T { let }"));
}

BOOST_AUTO_TEST_CASE(let_no_var) {
  check_single_error({{{}, {18, 19}}, "expected token" /* ? */}, parse2({}, {}, {}, "fn f() -> T { let = 1; }"));
}

BOOST_AUTO_TEST_CASE(let_no_expr) {
  check_single_error({{{}, {22, 23}}, "expected literal"}, parse2({}, {}, {}, "fn f() -> T { let x = }"));
}

BOOST_AUTO_TEST_CASE(assignment_no_expr) {
  check_single_error({{{}, {25, 26}}, "expected 'let'"}, parse2({}, {}, {}, "fn f() -> T { let x = 0; }"));
}

BOOST_AUTO_TEST_CASE(bad_second_fn) {
  check_single_error({{{}, {20, 20}}, "expected token"}, parse2({}, {}, {}, "fn f() -> T { 1 } fn"));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
