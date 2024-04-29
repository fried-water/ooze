#include "test.h"

#include "parser.h"

namespace ooze {

namespace {

#define check_pass(_PARSER, _EXP_AST, _EXP_SRCS, _SRC)                                                                 \
  [](auto parser,                                                                                                      \
     const AST& exp_ast,                                                                                               \
     const std::vector<std::pair<Type, SrcRef>>& exp_type_srcs,                                                        \
     std::string_view src) {                                                                                           \
    const auto [pd, ast] = check_result(parser({}, {}, src));                                                          \
    check_eq("type_srcs", exp_type_srcs, pd.type_srcs);                                                                \
    check_eq("ast", exp_ast, ast);                                                                                     \
    check_range(ast.forest.root_ids(), std::array{pd.parsed});                                                         \
  }(_PARSER, _EXP_AST, _EXP_SRCS, _SRC)

#define check_pass_roots(_PARSER, _EXP_AST, _EXP_SRCS, _SRC)                                                           \
  [](auto parser,                                                                                                      \
     const AST& exp_ast,                                                                                               \
     const std::vector<std::pair<Type, SrcRef>>& exp_type_srcs,                                                        \
     std::string_view src) {                                                                                           \
    const auto [pd, ast] = check_result(parser({}, {}, src));                                                          \
    check_eq("type_srcs", exp_type_srcs, pd.type_srcs);                                                                \
    check_eq("ast", exp_ast, ast);                                                                                     \
    check_range(ast.forest.root_ids(), pd.parsed);                                                                     \
  }(_PARSER, _EXP_AST, _EXP_SRCS, _SRC)

#define check_parse_type(_EXP_TYPES, _EXP_SRCS, _SRC)                                                                  \
  [](const TypeGraph& exp_tg, const std::vector<std::pair<Type, SrcRef>>& exp_type_srcs, std::string_view src) {       \
    auto [pd, ast] = check_result(parse_type({}, {}, src));                                                            \
    check_eq("type_srcs", exp_type_srcs, pd.type_srcs);                                                                \
    check_eq("tg", exp_tg, ast.tg);                                                                                    \
    check_eq("type", Type(ast.tg.num_nodes() - 1), pd.parsed);                                                         \
    ast.tg = {};                                                                                                       \
    check_eq("ast", AST{}, ast);                                                                                       \
  }(_EXP_TYPES, _EXP_SRCS, _SRC)

template <typename Parser>
void check_single_error(Parser parser, ContextualError expected, std::string_view src) {
  const std::vector<ContextualError> err = check_error(parser({}, {}, src));
  BOOST_REQUIRE(err.size() == 1);
  check_eq("error", expected, err.front());
}

std::vector<SrcRef> slices(std::string_view src, const std::vector<std::string_view>& svs) {
  return transform_to_vec(svs, [&](std::string_view sv) {
    const auto pos = src.find(sv);
    BOOST_REQUIRE_NE(std::string_view::npos, pos);
    return SrcRef{SrcID::Invalid(), Slice{int(pos), int(pos + sv.size())}};
  });
}

std::vector<std::pair<Type, SrcRef>>
type_refs(std::string_view src, const std::vector<std::pair<i32, std::string_view>>& svs) {
  return transform_to_vec(svs, [&](auto p) {
    const auto& [type, sv] = p;
    const auto pos = src.find(sv);
    BOOST_REQUIRE_NE(std::string_view::npos, pos);
    return std::pair(Type{type}, SrcRef{SrcID::Invalid(), Slice{int(pos), int(pos + sv.size())}});
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

TypeGraph make_tg(std::vector<TypeTag> tags, std::vector<std::vector<i32>> edges = {}) {
  Graph<Type> g;

  if(edges.empty()) {
    for(int i = 0; i < tags.size(); i++) {
      g.add_node();
    }
  }
  for(const auto& fanout : edges) {
    g.add_node();
    for(const i32 t : fanout) {
      g.add_fanout_to_last_node(Type(t));
    }
  }

  const size_t size = tags.size();

  return std::move(g).append_column(std::move(tags), std::vector<TypeID>(size));
}

std::vector<Type> types(size_t s) { return std::vector<Type>(s); }

} // namespace

BOOST_AUTO_TEST_SUITE(parser)

BOOST_AUTO_TEST_CASE(pattern_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {src}), types(1)};
  check_pass(parse_pattern, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(pattern_wildcard) {
  const std::string_view src = "_";
  const AST ast = {ast_forest({{ASTTag::PatternWildCard}}), slices(src, {src}), types(1)};
  check_pass(parse_pattern, ast, {}, "_");
}

BOOST_AUTO_TEST_CASE(pattern_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::PatternTuple}}), slices(src, {src}), types(1)};
  check_pass(parse_pattern, ast, {}, "()");
}

BOOST_AUTO_TEST_CASE(pattern_tuple1) {
  const std::string_view src = "(x)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", src}), types(2)};
  check_pass(parse_pattern, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(pattern_tuple2) {
  const std::string_view src = "(x, _)";
  const AST ast = {
    ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::PatternTuple, ASTTag::PatternWildCard}}),
    slices(src, {"x", "_", src}),
    types(3)};
  check_pass(parse_pattern, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(pattern_tuple_trailing_comma) {
  const std::string_view src = "(x,)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}), slices(src, {"x", src}), types(2)};
  check_pass(parse_pattern, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(pattern_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternTuple}}), slices(src, {"()", src}), types(2)};
  check_pass(parse_pattern, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(type_ident) {
  const std::string_view src = "A";
  const TypeGraph tg = make_tg({TypeTag::Leaf});
  check_parse_type(tg, type_refs(src, {{0, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_floating) {
  const std::string_view src = "_";
  const TypeGraph tg = make_tg({TypeTag::Floating});
  check_parse_type(tg, {}, src);
}

BOOST_AUTO_TEST_CASE(type_borrowed) {
  const std::string_view src = "&A";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Borrow}, {{}, {0}});
  check_parse_type(tg, type_refs(src, {{0, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_borrowed_floating) {
  const std::string_view src = "&_";
  const TypeGraph tg = make_tg({TypeTag::Floating, TypeTag::Borrow}, {{}, {0}});
  check_parse_type(tg, {}, src);
}

BOOST_AUTO_TEST_CASE(type_tuple) {
  const std::string_view src = "()";
  const TypeGraph tg = make_tg({TypeTag::Tuple});
  check_parse_type(tg, {}, src);
}

BOOST_AUTO_TEST_CASE(type_tuple1) {
  const std::string_view src = "(A)";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple}, {{}, {0}});
  check_parse_type(tg, type_refs(src, {{0, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_tuple2) {
  const std::string_view src = "(A, _)";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Floating, TypeTag::Tuple}, {{}, {}, {0, 1}});
  check_parse_type(tg, type_refs(src, {{0, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_tuple_trailing_comma) {
  const std::string_view src = "(A,)";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple}, {{}, {0}});
  check_parse_type(tg, type_refs(src, {{0, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_tuple_nested) {
  const std::string_view src = "(())";
  const TypeGraph tg = make_tg({TypeTag::Tuple, TypeTag::Tuple}, {{}, {0}});
  check_parse_type(tg, {}, src);
}

BOOST_AUTO_TEST_CASE(type_fn) {
  const std::string_view src = "fn() -> A";
  const TypeGraph tg = make_tg({TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, {{}, {}, {0, 1}});
  check_parse_type(tg, type_refs(src, {{1, "A"}}), src);
}

BOOST_AUTO_TEST_CASE(type_fn_arg) {
  const std::string_view src = "fn(A) -> B";
  const TypeGraph tg = make_tg({TypeTag::Leaf, TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, {{}, {0}, {}, {1, 2}});
  check_parse_type(tg, type_refs(src, {{0, "A"}, {2, "B"}}), src);
}

BOOST_AUTO_TEST_CASE(expr_literal) {
  const std::string_view src = "1";
  const AST ast = {ast_forest({{ASTTag::ExprLiteral}}), slices(src, {"1"}), types(1), {}, {{ASTID{0}, 1}}};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_ident) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {src}), types(1)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_qualified) {
  const std::string_view src = "a::b::c";
  auto f = ast_forest({{ASTTag::ExprQualified, ASTTag::ModuleRef}});
  f.append_child(ASTID{0}, ASTTag::ModuleRef);
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"a", "b", "c", src}), types(4)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_tuple) {
  const std::string_view src = "()";
  const AST ast = {ast_forest({{ASTTag::ExprTuple}}), slices(src, {src}), types(1)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_tuple1) {
  const std::string_view src = "(a)";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}}), slices(src, {"a", src}), types(2)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_tuple2) {
  const std::string_view src = "(a, b)";
  auto f = ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"a", "b", src}), types(3)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_tuple_trailing_comma) {
  const std::string_view src = "(a,)";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprIdent}}), slices(src, {"a", src}), types(2)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_tuple_nested) {
  const std::string_view src = "(())";
  const AST ast = {ast_forest({{ASTTag::ExprTuple, ASTTag::ExprTuple}}), slices(src, {"()", src}), types(2)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_borrow) {
  const std::string_view src = "&x";
  const AST ast = {ast_forest({{ASTTag::ExprBorrow, ASTTag::ExprIdent}}), slices(src, {"x", src}), types(2)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call) {
  const std::string_view src = "f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   slices(src, {"f", "()", src}),
                   types(3)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_arg) {
  const std::string_view src = "f(a)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"f", "a", "(a)", src}),
    types(4)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_call) {
  const std::string_view src = "f()()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprCall, ASTTag::ExprTuple},
                               {ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   as_invalid_refs({{0, 1}, {1, 3}, {3, 5}, {0, 3}, {0, 5}}),
                   types(5)}; // TODO make post order

  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs) {
  const std::string_view src = "x.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    slices(src, {"x", "f", "()", src}),
    types(4)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_literal) {
  const std::string_view src = "1.f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent}, {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"1", "f", "()", src}),
    types(4),
    {},
    {{ASTID{0}, 1}},
  };
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_call) {
  const std::string_view src = "g().f()";
  const AST ast = {ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple}}),
                   as_invalid_refs({{0, 1}, {1, 3}, {0, 3}, {4, 5}, {5, 7}, {0, 7}}),
                   types(6)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_arg) {
  const std::string_view src = "x.f(1)";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprLiteral}}),
    slices(src, {"x", "f", "1", "(1)", src}),
    types(5),
    {},
    {{ASTID{2}, 1}},
  };
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_chain) {
  const std::string_view src = "x.g().f()";
  const AST ast = {
    ast_forest({{ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprIdent},
                {ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
    as_invalid_refs({{0, 1}, {2, 3}, {3, 5}, {0, 5}, {6, 7}, {7, 9}, {0, 9}}),
    types(7)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_call_ufcs_borrow) {
  // TODO: should this be (&x).f()?
  const std::string_view src = "&x.f()";
  const AST ast = {ast_forest({{ASTTag::ExprBorrow, ASTTag::ExprCall, ASTTag::ExprIdent},
                               {ASTTag::ExprBorrow, ASTTag::ExprCall, ASTTag::ExprTuple, ASTTag::ExprIdent}}),
                   slices(src, {"x", "f", "()", "x.f()", src}),
                   types(5)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_if) {
  const std::string_view src = "if x { y } else { z }";
  auto f = ast_forest({{ASTTag::ExprIf, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const AST ast = {std::move(f), slices(src, {"x", "y", "z", src}), types(4)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_if_nested) {
  const std::string_view src = "if a { b } else if c { d } else { e }";
  auto f = ast_forest({{ASTTag::ExprIf, ASTTag::ExprIdent}});
  f.append_child(ASTID{0}, ASTTag::ExprIdent);
  const ASTID nested_id = f.append_child(ASTID{0}, ASTTag::ExprIf);
  f.append_child(nested_id, ASTTag::ExprIdent);
  f.append_child(nested_id, ASTTag::ExprIdent);
  f.append_child(nested_id, ASTTag::ExprIdent);
  const AST ast = {
    std::move(f),
    as_invalid_refs({{3, 4},   // a
                     {7, 8},   // b
                     {19, 20}, // c
                     {23, 24}, // d
                     {34, 35}, // e
                     {16, 37},
                     {0, 37}}),
    types(7)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(binding_no_type) {
  const std::string_view src = "x";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), types(1)};
  check_pass(parse_binding, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(binding_ident) {
  const std::string_view src = "x: T";
  const AST ast = {ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), {Type{0}}, make_tg({TypeTag::Leaf})};
  check_pass(parse_binding, ast, type_refs(src, {{0, "T"}}), src);
}

BOOST_AUTO_TEST_CASE(binding_floating) {
  const std::string_view src = "x: _";
  const AST ast = {
    ast_forest({{ASTTag::PatternIdent}}), slices(src, {"x"}), {Type{0}}, make_tg({TypeTag::Floating}, {})};
  check_pass(parse_binding, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(binding_tuple) {
  const std::string_view src = "(): ()";
  const AST ast = {
    ast_forest({{ASTTag::PatternTuple}}), as_invalid_refs({{0, 2}}), {Type{0}}, make_tg({TypeTag::Tuple})};
  check_pass(parse_binding, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(binding_tuple_arg) {
  const std::string_view src = "(x): (T)";
  const AST ast = {ast_forest({{ASTTag::PatternTuple, ASTTag::PatternIdent}}),
                   slices(src, {"x", "(x)"}),
                   {Type{-1}, Type{1}},
                   make_tg({TypeTag::Leaf, TypeTag::Tuple}, {{}, {0}})};
  check_pass(parse_binding, ast, type_refs(src, {{0, "T"}}), src);
}

BOOST_AUTO_TEST_CASE(assignment) {
  const std::string_view src = "let x: T = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y"}),
                   {Type{0}, Type{-1}, Type{-1}},
                   make_tg({TypeTag::Leaf})};
  check_pass(parse_assignment, ast, type_refs(src, {{0, "T"}}), src);
}

BOOST_AUTO_TEST_CASE(assignment_no_type) {
  const std::string_view src = "let x = y";
  const AST ast = {ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent}, {ASTTag::Assignment, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", src}),
                   types(3)};
  check_pass(parse_assignment, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_scope) {
  const std::string_view src = "{ x }";
  const AST ast = {ast_forest({{ASTTag::ExprIdent}}), slices(src, {"x"}), types(1)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_scope_assign) {
  const std::string_view src = "{ let x = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x = y", "z", "let x = y; z"}),
                   types(5)};
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(expr_scope_assign_type) {
  const std::string_view src = "{ let x: T = y; z }";
  const AST ast = {ast_forest({{ASTTag::ExprWith, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::ExprWith, ASTTag::Assignment, ASTTag::ExprIdent},
                               {ASTTag::ExprWith, ASTTag::ExprIdent}}),
                   slices(src, {"x", "y", "let x: T = y", "z", "let x: T = y; z"}),
                   {Type{0}, Type{-1}, Type{-1}, Type{-1}, Type{-1}},
                   make_tg({TypeTag::Leaf})};
  check_pass(parse_expr, ast, type_refs(src, {{0, "T"}}), src);
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
  check_pass(parse_expr, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(fn) {
  const std::string_view src = "() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   slices(src, {"()", "x", "() -> T = x"}),
                   {Type{-1}, Type{0}, Type{-1}},
                   make_tg({TypeTag::Leaf})};
  check_pass(parse_fn, ast, type_refs(src, {{0, "T"}}), src);
}

BOOST_AUTO_TEST_CASE(fn_one_arg) {
  const std::string_view src = "(x: T1) -> T2 = y";
  const AST ast = {
    ast_forest({{ASTTag::Fn, ASTTag::PatternTuple, ASTTag::PatternIdent}, {ASTTag::Fn, ASTTag::ExprIdent}}),
    slices(src, {"x", "(x: T1)", "y", "(x: T1) -> T2 = y"}),
    {Type{0}, Type{-1}, Type{1}, Type{-1}},
    make_tg({TypeTag::Leaf, TypeTag::Leaf}, {{}, {}})};
  check_pass(parse_fn, ast, type_refs(src, {{0, "T1"}, {1, "T2"}}), src);
}

BOOST_AUTO_TEST_CASE(fn_return_fn) {
  const std::string_view src = "() -> fn() -> T = x";
  const AST ast = {ast_forest({{ASTTag::Fn, ASTTag::PatternTuple}, {ASTTag::Fn, ASTTag::ExprIdent}}),
                   as_invalid_refs({{0, 2}, {18, 19}, {0, 19}}),
                   {Type{-1}, Type{2}, Type{-1}},
                   make_tg({TypeTag::Tuple, TypeTag::Leaf, TypeTag::Fn}, {{}, {}, {0, 1}})};
  check_pass(parse_fn, ast, type_refs(src, {{1, "T"}}), src);
}

BOOST_AUTO_TEST_CASE(mod_empty) {
  const std::string_view src = "mod x {}";
  const AST ast = {ast_forest({{ASTTag::Module}}), slices(src, {"x"}), types(1)};
  check_pass_roots(parse, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(mod_nested) {
  const std::string_view src = "mod x { mod y {} }";
  const AST ast = {ast_forest({{ASTTag::Module, ASTTag::Module}}), slices(src, {"y", "x"}), types(2)};
  check_pass_roots(parse, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(mod_fn) {
  const std::string_view src = "mod x { fn f() -> () = () }";
  const AST ast = {ast_forest({{ASTTag::Module, ASTTag::Assignment, ASTTag::PatternIdent},
                               {ASTTag::Module, ASTTag::Assignment, ASTTag::Fn, ASTTag::PatternTuple},
                               {ASTTag::Module, ASTTag::Assignment, ASTTag::Fn, ASTTag::ExprTuple}}),
                   as_invalid_refs({{11, 12}, {12, 14}, {23, 25}, {12, 25}, {8, 25}, {4, 5}}),
                   {Type{-1}, Type{-1}, Type{0}, Type{-1}, Type{-1}, Type{-1}},
                   make_tg({TypeTag::Tuple}, {{}})};
  check_pass_roots(parse, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(mod_fn_adj) {
  const std::string_view src = "fn f() -> () = () mod x {}";
  const AST ast = {
    ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::PatternTuple},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::ExprTuple},
                {ASTTag::Module}}),
    as_invalid_refs({{3, 4}, {4, 6}, {15, 17}, {4, 17}, {0, 17}, {22, 23}}),
    {Type{-1}, Type{-1}, Type{0}, Type{-1}, Type{-1}, Type{-1}},
    make_tg({TypeTag::Tuple}, {{}})};
  check_pass_roots(parse, ast, {}, src);
}

BOOST_AUTO_TEST_CASE(ast_empty) {
  const auto res = check_result(parse({}, {}, ""));
  check_eq("parsed", decltype(res){}, res);
}

BOOST_AUTO_TEST_CASE(ast_fn) {
  const std::string_view src = "fn f() -> T = x";
  const AST exp_ast = {
    ast_forest({{ASTTag::Assignment, ASTTag::PatternIdent},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::PatternTuple},
                {ASTTag::Assignment, ASTTag::Fn, ASTTag::ExprIdent}}),
    as_invalid_refs({{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}}),
    {Type{-1}, Type{-1}, Type{0}, Type{-1}, Type{-1}},
    make_tg({TypeTag::Leaf})};
  const auto [pr, act_ast] = check_result(parse({}, {}, src));
  check_eq("ast", exp_ast, act_ast);
  check_eq("type_srcs", type_refs(src, {{0, "T"}}), pr.type_srcs);
  check_range(act_ast.forest.root_ids(), pr.parsed);
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
  const AST exp_ast = {
    std::move(f),
    as_invalid_refs({{3, 4}, {4, 6}, {14, 15}, {4, 15}, {0, 15}, {19, 20}, {20, 22}, {30, 31}, {20, 31}, {16, 31}}),
    {Type{-1}, Type{-1}, Type{0}, Type{-1}, Type{-1}, Type{-1}, Type{-1}, Type{1}, Type{-1}, Type{-1}},
    make_tg({TypeTag::Leaf, TypeTag::Leaf}, {{}, {}})};

  const std::vector<std::pair<Type, SrcRef>> exp_type_srcs = {
    {Type{0}, SrcRef{SrcID{}, {10, 11}}}, {Type{1}, SrcRef{SrcID{}, {26, 27}}}};

  const auto [pr, act_ast] = check_result(parse({}, {}, src));
  check_eq("ast", exp_ast, act_ast);
  check_eq("type_srcs", exp_type_srcs, pr.type_srcs);
  check_range(act_ast.forest.root_ids(), pr.parsed);
}

BOOST_AUTO_TEST_CASE(repl_expr) {
  const std::string_view src = "x";

  const auto [expr_pr, exp_ast] = check_result(parse_expr({}, {}, src));
  const auto [act_pr, act_ast] = check_result(parse_repl({}, {}, src));

  check_eq("ast", exp_ast, act_ast);
  check_eq("pr", expr_pr, act_pr);
}

BOOST_AUTO_TEST_CASE(repl_assignment) {
  const std::string_view src = "let x: T = y";

  const auto [expr_pr, exp_ast] = check_result(parse_assignment({}, {}, src));
  const auto [act_pr, act_ast] = check_result(parse_repl({}, {}, src));

  check_eq("ast", exp_ast, act_ast);
  check_eq("pr", expr_pr, act_pr);
}

BOOST_AUTO_TEST_CASE(parse_consecutive) {
  AST ast;
  ParserResult<std::vector<ASTID>> pr1;
  ParserResult<std::vector<ASTID>> pr2;
  std::tie(pr1, ast) = check_result(parse({}, SrcID{0}, "fn f() -> T = x"));
  std::tie(pr2, ast) = check_result(parse(std::move(ast), SrcID{0}, "fn g() -> T = y"));

  auto [exp_pd, exp_ast] = check_result(parse({}, SrcID{0}, "fn f() -> T = x fn g() -> T = y"));

  BOOST_CHECK_EQUAL(1, pr1.parsed.size());
  BOOST_CHECK_EQUAL(1, pr2.parsed.size());
  check_eq("roots", to_vec(std::move(pr2.parsed), std::move(pr1.parsed)), exp_pd.parsed);

  check_eq("forest", exp_ast.forest, ast.forest);
  check_eq("tg", exp_ast.tg, ast.tg);
}

BOOST_AUTO_TEST_CASE(no_fn) { check_single_error(parse, {{{}, {0, 1}}, "expected 'fn'"}, "f"); }
BOOST_AUTO_TEST_CASE(no_fn2) { check_single_error(parse, {{{}, {0, 1}}, "expected 'fn'"}, "a"); }
BOOST_AUTO_TEST_CASE(no_fn3) { check_single_error(parse, {{{}, {0, 1}}, "expected 'fn'"}, ")"); }

BOOST_AUTO_TEST_CASE(bad_paren) { check_single_error(parse, {{{}, {2, 3}}, "expected token" /* ? */}, "fn)"); }

BOOST_AUTO_TEST_CASE(no_expr) { check_single_error(parse_expr, {{{}, {1, 2}}, "expected 'let'" /* ? */}, "{}"); }

BOOST_AUTO_TEST_CASE(no_return_type) {
  check_single_error(parse_fn, {{{}, {6, 7}}, "expected '&'" /* ? */}, "() -> { 1 }");
}

BOOST_AUTO_TEST_CASE(fn_no_tupl) { check_single_error(parse_type, {{{}, {3, 4}}, "expected '('"}, "fn T -> T"); }

BOOST_AUTO_TEST_CASE(no_return) { check_single_error(parse, {{{}, {7, 8}}, "expected '->'" /* ? */}, "fn f() { 1 }"); }

BOOST_AUTO_TEST_CASE(no_params) {
  check_single_error(parse, {{{}, {5, 7}}, "expected '('" /* ? */}, "fn f -> T { 1 }");
}

BOOST_AUTO_TEST_CASE(no_fn_name) {
  check_single_error(parse, {{{}, {3, 4}}, "expected token" /* ? */}, "fn () -> T { 1 }");
}

BOOST_AUTO_TEST_CASE(bad_fn_name) {
  check_single_error(parse, {{{}, {3, 4}}, "expected token" /* ? */}, "fn 1() -> T { 1 }");
}

BOOST_AUTO_TEST_CASE(no_fn_keyword) { check_single_error(parse, {{{}, {0, 1}}, "expected 'fn'"}, "f() -> T { 1 }"); }

BOOST_AUTO_TEST_CASE(no_scope) { check_single_error(parse, {{{}, {11, 11}}, "expected '='"}, "fn f() -> T"); }

BOOST_AUTO_TEST_CASE(unclosed_paren) { check_single_error(parse, {{{}, {6, 8}}, "expected ')'"}, "fn f( -> T { 1 }"); }

BOOST_AUTO_TEST_CASE(unopened_paren) { check_single_error(parse, {{{}, {4, 5}}, "expected '('"}, "fn f) -> T { 1 }"); }

BOOST_AUTO_TEST_CASE(untyped_paren2) {
  check_single_error(parse, {{{}, {9, 10}}, "expected '&'" /* ? */}, "fn f(a : ) -> T { 1 }");
}

BOOST_AUTO_TEST_CASE(bad_type_paren) {
  check_single_error(parse, {{{}, {9, 10}}, "expected '&'" /* ? */}, "fn f(a : 1) -> T { 1 }");
}

BOOST_AUTO_TEST_CASE(expr_unclosed) {
  check_single_error(parse, {{{}, {17, 18}}, "expected ')'"}, "fn f() -> T { a( }");
}

BOOST_AUTO_TEST_CASE(expr_unopened) {
  check_single_error(parse, {{{}, {15, 16}}, "expected '}'"}, "fn f() -> T { a) }");
}

BOOST_AUTO_TEST_CASE(expr_bad_comma) {
  check_single_error(parse, {{{}, {16, 17}}, "expected ')'"}, "fn f() -> T { a(,) }");
}

BOOST_AUTO_TEST_CASE(bad_chain) { check_single_error(parse, {{{}, {18, 19}}, "expected '('"}, "fn f() -> T { a.b }"); }

BOOST_AUTO_TEST_CASE(bad_chain2) { check_single_error(parse, {{{}, {18, 19}}, "expected '('"}, "fn f() -> T { a.1 }"); }

BOOST_AUTO_TEST_CASE(bad_assignment) {
  check_single_error(parse, {{{}, {18, 19}}, "expected token" /* ? */}, "fn f() -> T { let }");
}

BOOST_AUTO_TEST_CASE(let_no_var) {
  check_single_error(parse, {{{}, {18, 19}}, "expected token" /* ? */}, "fn f() -> T { let = 1; }");
}

BOOST_AUTO_TEST_CASE(let_no_expr) {
  check_single_error(parse, {{{}, {22, 23}}, "expected literal"}, "fn f() -> T { let x = }");
}

BOOST_AUTO_TEST_CASE(assignment_no_expr) {
  check_single_error(parse, {{{}, {25, 26}}, "expected 'let'"}, "fn f() -> T { let x = 0; }");
}

BOOST_AUTO_TEST_CASE(bad_second_fn) {
  check_single_error(parse, {{{}, {20, 20}}, "expected token"}, "fn f() -> T { 1 } fn");
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
