#include "test.h"

#include "frontend_helpers.h"
#include "parser.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

auto run_nr(const NativeTypeInfo& types, std::string_view src) {
  return parse_and_name_resolution(parse_fn, make_sv_array(src), types.names, {}, SrcID{0})
    .map([](ASTID, AST ast) { return std::tuple(std::move(ast)); })
    .map_state([](AST ast) { return std::get<2>(std::move(ast.tg).decompose()); });
}

template <typename Result>
void check_sema(Result sema_result, const std::vector<int>& exp_overload_indices) {
  const Map<ASTID, ASTID>& act_overloads = std::move(std::get<0>(sema_result));
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

  BOOST_REQUIRE(exp_overload_indices.size() == expr_idents.size());
  Map<ASTID, ASTID> exp_overloads;
  for(int i = 0; i < expr_idents.size(); i++) {
    const int pat_idx = exp_overload_indices[i];
    BOOST_REQUIRE(pat_idx < patterns.size());
    exp_overloads.emplace(expr_idents[i], patterns[pat_idx]);
  }
  check_eq("overloads", exp_overloads, act_overloads);
}

template <typename Parser>
auto run_sema(Parser p, TestEnv env, std::string_view src) {
  return frontend(p, make_sv_array(env.src, src), env.types, std::move(env.ast), std::array{env.module})
    .map([&](SemaData s, AST ast) { return std::tuple(std::move(s.overloads), std::move(ast)); });
}

auto run_sema(TestEnv env, std::string_view src) { return run_sema(parse, std::move(env), src); }

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

BOOST_AUTO_TEST_CASE(empty) { check_sema(check_result(run_sema({}, "")), {}); }

BOOST_AUTO_TEST_CASE(env_fn) { check_sema(check_result(run_sema(basic_test_env("f: fn() -> i32"), "")), {}); }

BOOST_AUTO_TEST_CASE(constant_fn) { check_sema(check_result(run_sema(basic_test_env(), "fn f() -> i32 = 1")), {}); }

BOOST_AUTO_TEST_CASE(identity) { check_sema(check_result(run_sema(basic_test_env(), "fn f(x: i32) -> i32 = x")), {1}); }

BOOST_AUTO_TEST_CASE(call_fn) {
  check_sema(check_result(run_sema(basic_test_env(),
                                   "fn f() -> i32 = 1"
                                   "fn g() -> i32 = f()")),
             {0});
}

BOOST_AUTO_TEST_CASE(call_env_fn) {
  check_sema(check_result(run_sema(basic_test_env("f: fn() -> i32"), "fn g() -> i32 = f()")), {0});
}

BOOST_AUTO_TEST_CASE(call_env_identity_fn) {
  check_sema(check_result(run_sema(basic_test_env("f: fn(i32) -> i32"), "fn g(x: i32) -> i32 = f(x)")),
             {0, 2}); // f(), x
}

BOOST_AUTO_TEST_CASE(nested_fn) {
  check_sema(check_result(run_sema(basic_test_env("f: fn(i32) -> i32"), "fn g(x: i32) -> i32 = f(f(x))")),
             {0, 0, 2}); // f(), f(), x
}

BOOST_AUTO_TEST_CASE(fn_overload) {
  const auto fns = make_sv_array("f: fn(i32) -> i32", "f: fn(f32) -> f32");

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: i32) -> i32 = f(x)")), {0, 3}); // f(i32), x

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: f32) -> f32 = f(x)")), {1, 3}); // f(f32), x

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: i32, y: f32) -> _ = (f(x), f(y))")),
             {0, 3, 1, 4}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(param_borrow) {
  check_sema(check_result(run_sema(basic_test_env("f: fn(&i32) -> ()"), "fn g(x: i32) -> () = f(&x)")), {0, 2}); // f, x
}

BOOST_AUTO_TEST_CASE(fn_overload_borrow) {
  const auto fns = make_sv_array("f: fn(i32) -> ()", "f: fn(&i32) -> ()");
  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: &i32) -> () = f(x)")), {1, 3}); // f(&i32), x

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: i32) -> () = f(x)")), {0, 3}); // f(i32), x
}

BOOST_AUTO_TEST_CASE(fn_overload_input) {
  const auto fns = make_sv_array("f: fn(i32) -> ()", "f: fn(f32) -> ()");

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: i32) -> () = f(x)")), {0, 3}); // f(i32), x

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: f32) -> () = f(x)")), {1, 3}); // f(f32), x

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g(x: i32, y: f32) -> _ = (f(x), f(y))")),
             {0, 3, 1, 4}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(fn_overload_output) {
  const auto fns = make_sv_array("f: fn() -> i32", "f: fn() -> f32");

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g() -> i32 = f()")), {0}); // f i32

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g() -> f32 = f()")), {1}); // f f32

  check_sema(check_result(run_sema(basic_test_env(fns), "fn g() -> (i32, f32) = (f(), f())")), {0, 1}); // f i32 , f f32
}

BOOST_AUTO_TEST_CASE(nested_fn_overload) {
  const auto fns =
    make_sv_array("f: fn(i32) -> ()", "f: fn(f32) -> ()", "g: fn(i32) -> i32", "h: fn() -> i32", "h: fn() -> f32");

  check_sema(check_result(run_sema(basic_test_env(fns), "fn x() -> () = f(g(h()))")),
             {0, 2, 3}); // f(i32), g(i32), h() -> i32
}

BOOST_AUTO_TEST_CASE(return_fn) {
  check_sema(check_result(run_sema(basic_test_env("f: fn() -> i32"), "fn g() -> _ = f")), {0}); // f()
}

BOOST_AUTO_TEST_CASE(fn_assign) {
  check_sema(check_result(run_sema(basic_test_env("f: fn() -> i32"), "fn g() -> i32 { let x = f; x() }")),
             {0, 2}); //  f(), x
}

BOOST_AUTO_TEST_CASE(mod_fn) {
  check_sema(check_result(run_sema(basic_test_env(), "mod m { fn g() -> _ = 1 } fn f() -> _ = m::g()")), {0});
}

BOOST_AUTO_TEST_CASE(env_function_inside_mod) {
  check_sema(check_result(run_sema(basic_test_env("f: fn() -> i32"), "mod m { fn g() -> _ = f() }")), {0});
}

BOOST_AUTO_TEST_CASE(partial) {
  TestEnv env = create_test_env({}, make_sv_array("f: fn() -> ()"));

  const std::string_view src1 = "fn g() -> _ = f()";
  const std::string_view src2 = "fn h() -> _ = ()";

  auto [s1, ast2] =
    check_result(frontend(parse, make_sv_array(env.src, src1), env.types, std::move(env.ast), std::array{env.module}));
  auto [s2, ast3] =
    check_result(frontend(parse, make_sv_array(env.src, src2), env.types, std::move(ast2), std::array{env.module}));

  // Bindings from g() should be ignored in the second call
  BOOST_CHECK_EQUAL(1, s1.overloads.size());
  BOOST_CHECK_EQUAL(0, s2.overloads.size());
}

BOOST_AUTO_TEST_CASE(ignore_unresolved_env_fn) {
  check_sema(check_result(run_sema(create_test_env({}, make_sv_array("f: fn() -> _")), "")), {});
}

BOOST_AUTO_TEST_CASE(unresolved_fn) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: _"}};
  check_eq("errors", exp_errors, check_error(run_sema(parse_fn, {}, "(x) -> _ = x")));
}

BOOST_AUTO_TEST_CASE(ambiguous_overload) {
  const TestEnv env = basic_test_env("f: fn() -> i32", "f: fn() -> f32", "g: fn(i32) -> ()", "g: fn(f32) -> ()");

  const std::vector<ContextualError> exp_errors = {
    {{SrcID{1}, {17, 18}},
     "ambiguous overload",
     {"deduced fn() -> _ [2 candidate(s)]", "  fn() -> i32", "  fn() -> f32"}}};

  check_eq("errors", exp_errors, check_error(run_sema(env, "fn h() -> () = g(f())")));
}

BOOST_AUTO_TEST_CASE(no_overload) {
  const TestEnv env = basic_test_env("f: fn() -> f32", "f: fn() -> string");

  const std::vector<ContextualError> exp_errors = {
    {{SrcID{1}, {16, 17}},
     "no matching overload found",
     {"deduced fn() -> i32 [2 candidate(s)]", "  fn() -> f32", "  fn() -> string"}},
  };

  check_eq("errors", exp_errors, check_error(run_sema(env, "fn g() -> i32 = f()")));
}

BOOST_AUTO_TEST_CASE(borrow_partial) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: _"}};
  check_eq("errors", exp_errors, check_error(run_sema(parse_fn, {}, "(x) -> _ { let _ = &x; () }")));
}

BOOST_AUTO_TEST_CASE(tuple_partial) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: (_)"}};
  check_eq("errors", exp_errors, check_error(run_sema(parse_fn, {}, "(x: (_)) -> _ = x")));

  const std::vector<ContextualError> exp_errors2 = {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: (_)"}};
  check_eq("errors", exp_errors2, check_error(run_sema(parse_fn, {}, "(x) -> (_) = x")));
}

BOOST_AUTO_TEST_CASE(undeclared_function) {
  const std::vector<ContextualError> exp_errors = {{{SrcID{1}, {15, 22}}, "undeclared binding 'missing'"}};
  check_eq("errors", exp_errors, check_error(run_sema(create_test_env(), "fn f() -> () = missing()")));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(ig)

BOOST_AUTO_TEST_CASE(null) { check_eq("graph", Graph<ASTID>{}, check_result(calculate_ident_graph({}, {}, {}))); }

BOOST_AUTO_TEST_CASE(unbound) {
  const auto srcs = make_sv_array("x");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const auto errors = check_error(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {0, 1}}, "undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(unbound_fn) {
  const auto srcs = make_sv_array("x()");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const auto errors = check_error(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {0, 1}}, "undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(scope) {
  // x, 1, assign, x, with
  const auto srcs = make_sv_array("{ let x = 1; x}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(unused) {
  // x, 1, assign, 1, with
  const auto srcs = make_sv_array("{ let x = 1; 1}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(multiple_uses) {
  // x, 1, assign, x, x, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, x)}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{3}, ASTID{4}}, {}, {}, {ASTID{0}}, {ASTID{0}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(scope_and_unbound) {
  // x, 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, y)}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const auto errors = check_error(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {17, 18}}, "undeclared binding 'y'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(scope_tuple) {
  // x, y, (), 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let (x, y) = 1; (x, y)}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{5}}, {ASTID{6}}, {}, {}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(self_assign) {
  // x, x, assign, x, with
  const auto srcs = make_sv_array("{ let x = x; x}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const auto errors = check_error(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {10, 11}}, "undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(nested) {
  // ((x, ((x, 1, assign), x, with), assign), x, assign)
  const auto srcs = make_sv_array("{ let x = { let x = 1; x }; x}");
  const auto [pr, ast] = check_result(parse_expr({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{7}}, {ASTID{4}}, {}, {}, {ASTID{1}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn) {
  // x, (), x, fn
  const auto srcs = make_sv_array("(x) -> T = x");
  const auto [pr, ast] = check_result(parse_fn({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{2}}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn_tuple) {
  // x, y, (), x, y, (), fn
  const auto srcs = make_sv_array("(x, y) -> T = (x, y)");
  const auto [pr, ast] = check_result(parse_fn({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {ASTID{4}}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(fn_unbound) {
  // (), x, fn
  const auto srcs = make_sv_array("() -> T = x");
  const auto [pr, ast] = check_result(parse_fn({}, SrcID{0}, srcs[0]));
  const auto errors = check_error(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {10, 11}}, "undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(fn_unused) {
  // x, (), 1, fn
  const auto srcs = make_sv_array("(x) -> T = 1");
  const auto [pr, ast] = check_result(parse_fn({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(fn_nested) {
  // x, (), ((x, ((x, x, assign), x, with), assign), x, assign) fn
  const auto srcs = make_sv_array("(x) -> T { let x = { let x = x; x }; x}");
  const auto [pr, ast] = check_result(parse_fn({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, as_span(pr.parsed)));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{
    {ASTID{4}}, {}, {ASTID{9}}, {ASTID{6}}, {ASTID{0}}, {}, {ASTID{3}}, {}, {}, {ASTID{2}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(call_global) {
  auto [types, env_src, ast, module] = create_test_env({}, make_sv_array("f: fn() -> ()"));
  const auto srcs = make_sv_array(env_src, "fn g() -> _ = f()");
  ParserResult<std::vector<ASTID>> pr;
  std::tie(pr, ast) = check_result(parse(std::move(ast), SrcID{1}, srcs[1]));

  std::vector<std::vector<ASTID>> fanout(11);
  fanout[0] = {ASTID{6}};
  fanout[6] = {ASTID{0}};

  check_eq(
    "ident_graph", Graph<ASTID>(fanout), check_result(calculate_ident_graph(srcs, ast, pr.parsed, std::array{module})));
}

BOOST_AUTO_TEST_CASE(overloaded) {
  const auto srcs = make_sv_array("fn f(Y) -> _ = ()"
                                  "fn f(X) -> _ = ()"
                                  "fn g() -> _ = f()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, pr.parsed));

  std::vector<std::vector<ASTID>> fanout(19);
  fanout[0] = {ASTID{14}};
  fanout[6] = {ASTID{14}};
  fanout[14] = {ASTID{0}, ASTID{6}};

  check_eq("ident_graph", Graph<ASTID>(fanout), ident_graph);
}

BOOST_AUTO_TEST_CASE(partial) {
  const auto srcs = make_sv_array("fn f() -> _ = g()", "fn g() -> _ = f()");
  const auto [pr1, ast1] = check_result(parse({}, SrcID{0}, srcs[0]));
  const auto [pr2, ast] = check_result(parse(std::move(ast1), SrcID{1}, srcs[1]));

  const Graph<ASTID> ident_graph1 = check_result(calculate_ident_graph(srcs, ast, pr1.parsed));
  const Graph<ASTID> ident_graph2 = check_result(calculate_ident_graph(srcs, ast, pr2.parsed));

  std::vector<std::vector<ASTID>> fanout1(14);
  fanout1[2] = {ASTID{7}};
  fanout1[7] = {ASTID{2}};
  check_eq("ident_graph", Graph<ASTID>(fanout1), ident_graph1);

  std::vector<std::vector<ASTID>> fanout2(14);
  fanout2[0] = {ASTID{9}};
  fanout2[9] = {ASTID{0}};
  check_eq("ident_graph", Graph<ASTID>(fanout2), ident_graph2);
}

BOOST_AUTO_TEST_CASE(partial_error) {
  // h() doesnt exist, but shouldnt be checked
  const auto srcs = make_sv_array("fn f() -> _ = h()", "fn g() -> _ = ()");
  const auto [pr1, ast1] = check_result(parse({}, SrcID{0}, srcs[0]));
  const auto [pr2, ast] = check_result(parse(std::move(ast1), SrcID{1}, srcs[1]));

  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, pr2.parsed));
  std::vector<std::vector<ASTID>> fanout(ast.forest.size());
  check_eq("ident_graph", Graph<ASTID>(fanout), ident_graph);
}

BOOST_AUTO_TEST_CASE(recursive) {
  const auto srcs = make_sv_array("fn f() -> _ = f()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast, pr.parsed));

  std::vector<std::vector<ASTID>> fanout(7);
  fanout[0] = {ASTID{2}};
  fanout[2] = {ASTID{0}};

  check_eq("ident_graph", Graph<ASTID>(fanout), ident_graph);
}

BOOST_AUTO_TEST_CASE(unused_fn) {
  auto [types, env_src, ast, module] = create_test_env({}, make_sv_array("f: fn() -> ()"));
  const auto srcs = make_sv_array(env_src);
  const auto fanout = std::vector<std::vector<ASTID>>(ast.forest.size());
  check_eq("ident_graph", Graph<ASTID>(fanout), check_result(calculate_ident_graph(srcs, ast, {})));
}

BOOST_AUTO_TEST_CASE(unused_script_fn) {
  const auto srcs = make_sv_array("fn f() -> () = ()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const auto fanout = std::vector<std::vector<ASTID>>(ast.forest.size());
  check_eq("ident_graph", Graph<ASTID>(fanout), check_result(calculate_ident_graph(srcs, ast, pr.parsed)));
}

BOOST_AUTO_TEST_CASE(mod_fn) {
  const auto srcs = make_sv_array("mod m { fn g() -> _ = 1 } fn f() -> _ = m::g()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  auto fanout = std::vector<std::vector<ASTID>>(ast.forest.size());

  fanout[0] = {ASTID{9}};
  fanout[9] = {ASTID{0}};

  check_eq("ident_graph", Graph<ASTID>(fanout), check_result(calculate_ident_graph(srcs, ast, pr.parsed)));
  check_eq("ident_graph",
           Graph<ASTID>(fanout),
           check_result(calculate_ident_graph(srcs, ast, std::array{ast.forest.root_ids().get<1>()})));
}

BOOST_AUTO_TEST_CASE(inaccessible_inner_mod_fn) {
  const auto srcs = make_sv_array("mod m { fn g() -> _ = 1 } fn f() -> () = g()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {41, 42}}, "undeclared binding 'g'"}};
  check_eq("ig_errors", exp_errors, check_error(calculate_ident_graph(srcs, ast, pr.parsed)));
}

BOOST_AUTO_TEST_CASE(inaccessible_outer_mod_fn) {
  const auto srcs = make_sv_array("fn f() -> _ = 1 mod m { fn g() -> _ = f() } ");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {38, 39}}, "undeclared binding 'f'"}};
  check_eq("ig_errors", exp_errors, check_error(calculate_ident_graph(srcs, ast, pr.parsed)));
}

// TODO change error to specify can't find a module
BOOST_AUTO_TEST_CASE(nonexistant_mod) {
  const auto srcs = make_sv_array("mod m { fn g() -> _ = 1 } fn f() -> () = n::g()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {41, 45}}, "undeclared binding 'n::g'"}};
  check_eq("ig_errors", exp_errors, check_error(calculate_ident_graph(srcs, ast, pr.parsed)));
}

BOOST_AUTO_TEST_CASE(nonexistant_fn_in_mod) {
  const auto srcs = make_sv_array("mod m { fn g() -> _ = 1 } fn f() -> () = m::h()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {44, 45}}, "undeclared binding 'h'"}};
  check_eq("ig_errors", exp_errors, check_error(calculate_ident_graph(srcs, ast, pr.parsed)));
}

BOOST_AUTO_TEST_CASE(nonexistant_fn_as_mod) {
  const auto srcs = make_sv_array("fn g() -> _ = 1 fn f() -> () = g::f()");
  const auto [pr, ast] = check_result(parse({}, SrcID{0}, srcs[0]));
  const std::vector<ContextualError> exp_errors = {{{SrcID{0}, {31, 35}}, "undeclared binding 'g::f'"}};
  check_eq("ig_errors", exp_errors, check_error(calculate_ident_graph(srcs, ast, pr.parsed)));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
