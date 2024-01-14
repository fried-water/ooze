#include "test.h"

#include "frontend_helpers.h"
#include "parser.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

NativeTypeInfo basic_types() {
  return {{{"f32", type_id(knot::Type<f32>{})},
           {"i32", type_id(knot::Type<i32>{})},
           {"string", type_id(knot::Type<std::string>{})}}};
}

using EnvValues = std::vector<std::pair<std::string, std::string>>;

std::tuple<std::string, AST, TypeGraph> create_env(const EnvValues& values, const NativeTypeInfo& types) {
  std::string src;
  AST ast;
  TypeGraph tg;

  std::vector<std::pair<Type, SrcRef>> type_srcs;

  for(const auto& [name, type] : values) {
    std::tie(type_srcs, ast, tg) = check_result(parse_type(std::move(ast), std::move(tg), SrcID{0}, type));
    tg = check_result(type_name_resolution(make_sv_array(type), types.names, type_srcs, std::move(tg)));
    add_global(ast, SrcRef{SrcID{0}, append_src(src, name)}, Type{tg.num_nodes() - 1});
  }

  return std::tuple(std::move(src), std::move(ast), std::move(tg));
}

auto run_nr(const NativeTypeInfo& types, std::string_view src) {
  return parse_and_name_resolution(parse_function, make_sv_array(src), types.names, {}, {}, SrcID{0})
    .map_state([](AST, TypeGraph tg) { return std::get<2>(std::move(tg).decompose()); });
}

template <typename Result>
void check_sema(Result sema_result,
                const Map<int, std::vector<int>>& exp_cg,
                const std::vector<int>& exp_fn_ordering_indices,
                const std::vector<int>& exp_binding_of_indices) {
  const CallGraphData act = std::move(std::get<0>(sema_result));
  const AST ast = std::move(std::get<1>(sema_result));

  const std::vector<ASTID> fns =
    transform_to_vec(ast.forest.root_ids(), [&](ASTID root) { return *ast.forest.first_child(root); });

  const std::vector<ASTID> expr_idents =
    filter_to_vec(ast.forest.ids(), [&](ASTID id) { return ast.forest[id] == ASTTag::ExprIdent; });

  const std::vector<ASTID> patterns = filter_to_vec(ast.forest.ids(), [&](ASTID id) {
    return (ast.forest[id] == ASTTag::PatternIdent || ast.forest[id] == ASTTag::PatternWildCard);
  });

  const auto to_fn = [&](int fn_idx) {
    BOOST_REQUIRE(fn_idx < fns.size());
    return fns[fn_idx];
  };

  const std::vector<ASTID> exp_fn_ordering = transform_to_vec(exp_fn_ordering_indices, to_fn);
  check_eq("fn_ordering", exp_fn_ordering, act.topographical_fn_ordering);

  std::vector<std::vector<ASTID>> exp_fanout(ast.forest.size());
  for(const auto& [fn, fanout] : exp_cg) {
    exp_fanout[to_fn(fn).get()] = transform_to_vec(fanout, to_fn);
  }
  check_eq("call_graph", Graph<ASTID>(exp_fanout), act.call_graph);

  BOOST_REQUIRE(exp_binding_of_indices.size() == expr_idents.size());
  Map<ASTID, ASTID> exp_binding_of;
  for(int i = 0; i < expr_idents.size(); i++) {
    const int pat_idx = exp_binding_of_indices[i];
    BOOST_REQUIRE(pat_idx < patterns.size());
    exp_binding_of.emplace(expr_idents[i], patterns[pat_idx]);
  }
  check_eq("binding_of", exp_binding_of, act.binding_of);
}

auto run_sema(const EnvValues& functions, const NativeTypeInfo& native_types, std::string_view src) {
  auto [fn_src, ast, tg] = create_env(functions, native_types);
  TypeCache tc = create_type_cache(tg);

  const auto srcs = make_sv_array(fn_src, src);
  return parse_and_name_resolution(parse, srcs, native_types.names, std::move(ast), std::move(tg), SrcID{1})
    .and_then([&](AST ast, TypeGraph tg) { return sema(srcs, tc, native_types, std::move(ast), std::move(tg)); });
}

} // namespace

BOOST_AUTO_TEST_SUITE(nr)

const TypeID I = type_id(knot::Type<i32>{});
const TypeID F = type_id(knot::Type<f32>{});
const TypeID S = type_id(knot::Type<std::string>{});

BOOST_AUTO_TEST_CASE(fn_return) {
  const auto act = check_result(run_nr(basic_types(), "() -> i32 = x"));
  check_eq("types", (std::vector<TypeID>{I}), act);
}

BOOST_AUTO_TEST_CASE(arg) {
  const auto act = check_result(run_nr(basic_types(), "(x: i32) -> () = ()"));
  check_eq("types", (std::vector<TypeID>{I, TypeID::Invalid()}), act);
}

BOOST_AUTO_TEST_CASE(let) {
  const auto act = check_result(run_nr(basic_types(), "() -> () { let x : i32 = y; x }"));
  check_eq("types", (std::vector<TypeID>{TypeID::Invalid(), I}), act);
}

BOOST_AUTO_TEST_CASE(multi) {
  const auto act = check_result(run_nr(basic_types(), "(x: i32) -> f32 { let x : string = y; x }"));
  check_eq("types", (std::vector<TypeID>{I, F, S}), act);
}

BOOST_AUTO_TEST_CASE(undefined_return) {
  const auto act = check_error(run_nr({}, "(x) -> T = x"));
  const std::vector<ContextualError> exp = {{{SrcID{0}, {7, 8}}, "undefined type"}};
  check_eq("errors", exp, act);
}

BOOST_AUTO_TEST_CASE(undefined_arg) {
  const auto act = check_error(run_nr({}, "(x: T) -> () = ()"));
  const std::vector<ContextualError> exp = {{{SrcID{0}, {4, 5}}, "undefined type"}};
  check_eq("errors", exp, act);
}

BOOST_AUTO_TEST_CASE(undefined_let) {
  const auto act = check_error(run_nr({}, "() -> () { let x: T = y; x }"));
  const std::vector<ContextualError> exp = {{{SrcID{0}, {18, 19}}, "undefined type"}};
  check_eq("errors", exp, act);
}

BOOST_AUTO_TEST_CASE(undefined_multi) {
  const auto act = check_error(run_nr({}, "(x: A) -> B { let x: C = y; x }"));
  const std::vector<ContextualError> exp = {
    {{SrcID{0}, {4, 5}}, "undefined type"},
    {{SrcID{0}, {10, 11}}, "undefined type"},
    {{SrcID{0}, {22, 23}}, "undefined type"}};
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(sm)

BOOST_AUTO_TEST_CASE(empty) { check_sema(check_result(run_sema({}, NativeTypeInfo{}, "")), {}, {}, {}); }

BOOST_AUTO_TEST_CASE(env_fn) {
  check_sema(check_result(run_sema({{"f", "fn() -> i32"}}, basic_types(), "")), {}, {0}, {});
}

BOOST_AUTO_TEST_CASE(constant_fn) {
  check_sema(check_result(run_sema({}, basic_types(), "fn f() -> i32 = 1")), {}, {0}, {});
}

BOOST_AUTO_TEST_CASE(identity) {
  check_sema(check_result(run_sema({}, basic_types(), "fn f(x: i32) -> i32 = x")), {}, {0}, {1});
}

BOOST_AUTO_TEST_CASE(call_fn) {
  check_sema(check_result(run_sema({},
                                   basic_types(),
                                   "fn f() -> i32 = 1"
                                   "fn g() -> i32 = f()")),
             {{1, {0}}},
             {0, 1},
             {0});
}

BOOST_AUTO_TEST_CASE(call_env_fn) {
  check_sema(
    check_result(run_sema({{"f", "fn() -> i32"}}, basic_types(), "fn g() -> i32 = f()")), {{1, {0}}}, {0, 1}, {0});
}

BOOST_AUTO_TEST_CASE(call_env_identity_fn) {
  check_sema(check_result(run_sema({{"f", "fn(i32) -> i32"}}, basic_types(), "fn g(x: i32) -> i32 = f(x)")),
             {{1, {0}}}, // g -> f
             {0, 1},
             {0, 2}); // f(), x
}

BOOST_AUTO_TEST_CASE(nested_fn) {
  check_sema(check_result(run_sema({{"f", "fn(i32) -> i32"}}, basic_types(), "fn g(x: i32) -> i32 = f(f(x))")),
             {{1, {0}}}, // g -> f
             {0, 1},     // f(), g
             {0, 0, 2}); // f(), f(), x
}

BOOST_AUTO_TEST_CASE(fn_overload) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(f32) -> f32"}};

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: i32) -> i32 = f(x)")),
             {{2, {0}}}, // g -> f(i32)
             {1, 0, 2},  // f(f32), f(i32), g
             {0, 3});    // f(i32), x

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: f32) -> f32 = f(x)")),
             {{2, {1}}}, // g -> f(f32)
             {1, 2, 0},  // f(f32), g, f(i32)
             {1, 3});    // f(f32), x

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: i32, y: f32) -> _ = (f(x), f(y))")),
             {{2, {0, 1}}}, // g -> f(i32), f(f32)
             {1, 0, 2},     // f(f32), f(i32), g
             {0, 3, 1, 4}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(param_borrow) {
  check_sema(check_result(run_sema({{"f", "fn(&i32) -> ()"}}, basic_types(), "fn g(x: i32) -> () = f(&x)")),
             {{1, {0}}}, // g -> f
             {0, 1},     // f, g
             {0, 2});    // f, x
}

BOOST_AUTO_TEST_CASE(fn_overload_borrow) {
  const EnvValues fns = {{"f", "fn(i32) -> ()"}, {"f", "fn(&i32) -> ()"}};
  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: &i32) -> () = f(x)")),
             {{2, {1}}}, // g -> f(&i32)
             {1, 2, 0},  // f(&i32), g, f(i32)
             {1, 3});    // f(&i32), x

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: i32) -> () = f(x)")),
             {{2, {0}}}, // g -> f(i32)
             {1, 0, 2},  // f(&i32), f(i32), g
             {0, 3});    // f(i32), x
}

BOOST_AUTO_TEST_CASE(fn_overload_input) {
  const EnvValues fns = {{"f", "fn(i32) -> ()"}, {"f", "fn(f32) -> ()"}};

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: i32) -> () = f(x)")),
             {{2, {0}}}, // g -> f(i32)
             {1, 0, 2},  // f(f32), f(i32), g
             {0, 3});    // f(i32), x

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: f32) -> () = f(x)")),
             {{2, {1}}}, // g -> f(f32)
             {1, 2, 0},  // f(f32), g, f(i32)
             {1, 3});    // f(f32), x

  check_sema(check_result(run_sema(fns, basic_types(), "fn g(x: i32, y: f32) -> _ = (f(x), f(y))")),
             {{2, {0, 1}}}, // g -> f(i32), f(f32)
             {1, 0, 2},     // f(f32), f(i32), g
             {0, 3, 1, 4}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(fn_overload_output) {
  const EnvValues fns = {{"f", "fn() -> i32"}, {"f", "fn() -> f32"}};

  check_sema(check_result(run_sema(fns, basic_types(), "fn g() -> i32 = f()")),
             {{2, {0}}}, // g -> f i32
             {1, 0, 2},  // f f32), f i32, g
             {0});       // f i32

  check_sema(check_result(run_sema(fns, basic_types(), "fn g() -> f32 = f()")),
             {{2, {1}}}, // g -> f f32
             {1, 2, 0},  // f f32, g, f i32
             {1});       // f f32

  check_sema(check_result(run_sema(fns, basic_types(), "fn g() -> (i32, f32) = (f(), f())")),
             {{2, {0, 1}}}, // g -> f i32, f f32
             {1, 0, 2},     // f f32, f i32, g
             {0, 1});       // f i32 , f f32
}

BOOST_AUTO_TEST_CASE(nested_fn_overload) {
  const EnvValues fns = {
    {"f", "fn(i32) -> ()"},
    {"f", "fn(f32) -> ()"},
    {"g", "fn(i32) -> i32"},
    {"h", "fn() -> i32"},
    {"h", "fn() -> f32"}};

  check_sema(check_result(run_sema(fns, basic_types(), "fn x() -> () = f(g(h()))")),
             {{5, {0, 2, 3}}}, // x -> f(i32), g(i32), h() -> i32
             {4, 3, 2, 1, 0, 5},
             {0, 2, 3}); // f(i32), g(i32), h() -> i32
}

BOOST_AUTO_TEST_CASE(return_fn) {
  const EnvValues fns = {{"f", "fn() -> i32"}};
  check_sema(check_result(run_sema(fns, basic_types(), "fn g() -> _ = f")),
             {{1, {0}}}, // g -> f()
             {0, 1},
             {0}); // f()
}

BOOST_AUTO_TEST_CASE(fn_assign) {
  const EnvValues fns = {{"f", "fn() -> i32"}};
  check_sema(check_result(run_sema(fns, basic_types(), "fn g() -> i32 { let x = f; x() }")),
             {{1, {0}}}, // g -> f()
             {0, 1},
             {0, 2}); //  f(), x
}

BOOST_AUTO_TEST_CASE(unresolved_fn) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: _"}};
  check_eq("errors", exp_errors, check_error(run_sema({}, {}, "fn f(x) -> _ = x")));
}

BOOST_AUTO_TEST_CASE(unresolved_env_fn) {
  const std::vector<ContextualError> exp_errors = {
    {{SrcID{0}, {0, 1}}, "unable to fully deduce type, deduced: fn() -> _"}};
  check_eq("errors", exp_errors, check_error(run_sema({{"f", "fn() -> _"}}, {}, "")));
}

BOOST_AUTO_TEST_CASE(borrow_partial) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: _"}};
  check_eq("errors", exp_errors, check_error(run_sema({}, {}, "fn f(x) -> _ { let _ = &x; () }")));
}

BOOST_AUTO_TEST_CASE(tuple_partial) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: (_)"}};
  check_eq("errors", exp_errors, check_error(run_sema({}, {}, "fn f(x: (_)) -> _ = x")));

  const std::vector<ContextualError> exp_errors2 = {{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: (_)"}};
  check_eq("errors", exp_errors2, check_error(run_sema({}, {}, "fn f(x) -> (_) = x")));
}

BOOST_AUTO_TEST_CASE(undeclared_function) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {15, 22}}, "use of undeclared binding 'missing'"}};
  check_eq("errors", exp_errors, check_error(run_sema({}, {}, "fn f() -> () = missing()")));
}

BOOST_AUTO_TEST_CASE(recursion) {
  const std::string_view src =
    "fn f(x) -> _ = g(x)"
    "fn g(x) -> _ = f(x)";
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {3, 4}}, "detected recursive cycle"}};
  check_eq("errors", exp_errors, check_error(run_sema({}, {}, src)));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(ig)

BOOST_AUTO_TEST_CASE(null) { BOOST_CHECK(check_result(calculate_ident_graph({}, {})) == Graph<ASTID>{}); }

BOOST_AUTO_TEST_CASE(unbound) {
  const auto srcs = make_sv_array("x");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {0, 1}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(unbound_fn) {
  const auto srcs = make_sv_array("x()");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {0, 1}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(scope) {
  // x, 1, assign, x, with
  const auto srcs = make_sv_array("{ let x = 1; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));
  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(unused) {
  // x, 1, assign, 1, with
  const auto srcs = make_sv_array("{ let x = 1; 1}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(multiple_uses) {
  // x, 1, assign, x, x, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, x)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{3}, ASTID{4}}, {}, {}, {ASTID{0}}, {ASTID{0}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(scope_and_unbound) {
  // x, 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, y)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {17, 18}}, "use of undeclared binding 'y'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(scope_tuple) {
  // x, y, (), 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let (x, y) = 1; (x, y)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{5}}, {ASTID{6}}, {}, {}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(self_assign) {
  // x, x, assign, x, with
  const auto srcs = make_sv_array("{ let x = x; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {10, 11}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(nested) {
  // ((x, ((x, 1, assign), x, with), assign), x, assign)
  const auto srcs = make_sv_array("{ let x = { let x = 1; x }; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{7}}, {ASTID{4}}, {}, {}, {ASTID{1}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn) {
  // x, (), x, fn
  const auto srcs = make_sv_array("(x) -> T = x");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{2}}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn_tuple) {
  // x, y, (), x, y, (), fn
  const auto srcs = make_sv_array("(x, y) -> T = (x, y)");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {ASTID{4}}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn_unbound) {
  // (), x, fn
  const auto srcs = make_sv_array("() -> T = x");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {10, 11}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(fn_unused) {
  // x, (), 1, fn
  const auto srcs = make_sv_array("(x) -> T = 1");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(fn_nested) {
  // x, (), ((x, ((x, x, assign), x, with), assign), x, assign) fn
  const auto srcs = make_sv_array("(x) -> T { let x = { let x = x; x }; x}");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{
    {ASTID{4}}, {}, {ASTID{9}}, {ASTID{6}}, {ASTID{0}}, {}, {ASTID{3}}, {}, {}, {ASTID{2}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(call_global) {
  auto [env_src, ast, tg] = create_env({{"f", "fn() -> ()"}}, {});
  const auto srcs = make_sv_array(env_src, "fn g() -> _ = f()");
  ast = std::get<1>(check_result(parse(std::move(ast), std::move(tg), SrcID{1}, srcs[1])));

  std::vector<std::vector<ASTID>> fanout(10);
  fanout[0] = {ASTID{5}};
  fanout[5] = {ASTID{0}};

  check_eq("ident_graph", Graph<ASTID>(std::move(fanout)), check_result(calculate_ident_graph(srcs, ast)));
}

BOOST_AUTO_TEST_CASE(overloaded) {
  const auto srcs = make_sv_array("fn f(Y) -> _ = ()"
                                  "fn f(X) -> _ = ()"
                                  "fn g() -> _ = f()");
  const AST ast = std::get<1>(check_result(parse({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  std::vector<std::vector<ASTID>> fanout(19);
  fanout[0] = {ASTID{14}};
  fanout[6] = {ASTID{14}};
  fanout[14] = {ASTID{0}, ASTID{6}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(recursive) {
  const auto srcs = make_sv_array("fn f() -> _ = f()");
  const AST ast = std::get<1>(check_result(parse({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  std::vector<std::vector<ASTID>> fanout(7);
  fanout[0] = {ASTID{2}};
  fanout[2] = {ASTID{0}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(unused_fn) {
  auto [env_src, ast, tg] = create_env({{"f", "fn() -> ()"}}, {});
  const auto srcs = make_sv_array(env_src);
  auto fanout = std::vector<std::vector<ASTID>>(ast.forest.size());
  check_eq("ident_graph", Graph<ASTID>(std::move(fanout)), check_result(calculate_ident_graph(srcs, ast)));
}

BOOST_AUTO_TEST_CASE(unused_script_fn) {
  const auto srcs = make_sv_array("fn f() -> () = ()");
  const AST ast = std::get<1>(check_result(parse({}, {}, SrcID{0}, srcs[0])));
  auto fanout = std::vector<std::vector<ASTID>>(ast.forest.size());
  check_eq("ident_graph", Graph<ASTID>(std::move(fanout)), check_result(calculate_ident_graph(srcs, ast)));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
