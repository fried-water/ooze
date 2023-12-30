#include "test.h"

#include "parser.h"
#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

void test_inferred_inputs(const Env& e,
                          std::string_view expr,
                          std::string_view exp_pattern,
                          std::vector<Slice> exp_refs,
                          Set<std::string> active = {}) {
  const auto expr_result = parse_expr(expr).and_then([&](const auto& s) { return type_name_resolution(e, s); });

  int next_ref = 0;
  const auto pattern_result =
    parse_pattern(exp_pattern)
      .and_then([&](const auto& p) { return type_name_resolution(e, p); })
      .map([&](TypedPattern p) {
        knot::preorder(p, [](Slice& ref) { ref = {}; });
        knot::preorder(p, [&](TypedPattern& p) {
          knot::visit(p.v,
                      Overloaded{[&](const ast::Ident&) {
                                   BOOST_REQUIRE(next_ref < exp_refs.size());
                                   p.ref = exp_refs[next_ref++];
                                 },
                                 [&](const std::vector<TypedPattern>& v) {
                                   p.type = tuple_type(std::vector<Type<TypeID>>(v.size(), floating_type<TypeID>()));
                                 }});
        });

        return p;
      });

  BOOST_REQUIRE(pattern_result.has_value());

  if(!expr_result.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(expr, expr_result.error())));
    BOOST_CHECK(expr_result.has_value());
  }

  const auto actual = inferred_inputs(expr_result.value(), std::move(active));

  if(actual != pattern_result.value()) {
    fmt::print("E {}\n", knot::debug(pattern_result.value()));
    fmt::print("A {}\n", knot::debug(actual));
    BOOST_CHECK(actual == pattern_result.value());
  }
}

void test_nr(Env e, std::string_view src, const std::vector<TypeID>& exp) {
  e.sm.push_back({"", std::string(src)});
  std::tie(e.ast, e.tg) = check_result(parse_function2({}, {}, SrcID{1}, src).and_then([&](AST ast, TypeGraph tg) {
    return type_name_resolution(e.sm, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
      return std::tuple(std::move(ast), std::move(tg));
    });
  }));

  const auto [g, tags, srcs, types] = std::move(e.tg).decompose();

  if(exp != types) {
    fmt::print("E {}\n", knot::debug(exp));
    fmt::print("A {}\n", knot::debug(types));
    BOOST_CHECK(exp == types);
  }
}

void test_nr_error(Env e, std::string_view src, const std::vector<ContextualError2>& expected_errors) {
  e.sm.push_back({"", std::string(src)});
  const auto errors =
    check_error(parse_function2({}, std::move(e.tg), SrcID{0}, src).and_then([&](AST ast, TypeGraph tg) {
      return type_name_resolution(e.sm, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    }));

  if(expected_errors != errors) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(errors));
    BOOST_CHECK(expected_errors == errors);
  }
}

void test_or(const Env& e, const std::vector<EnvFunctionRef>& overloads, std::string_view f) {
  const auto result =
    parse_function(f)
      .and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); })
      .and_then([&](TypedFunction f) { return type_check(e, std::move(f)); })
      .and_then([&](TypedFunction f) { return overload_resolution(e, f); });

  if(!result.has_value()) {
    fmt::print("Error: {}", knot::debug(contextualize(f, result.error())));
    BOOST_REQUIRE(result.has_value());
  }

  int f_idx = 0;
  knot::preorder(result.value(), [&](const EnvFunctionRef& ref) {
    BOOST_REQUIRE(f_idx < overloads.size());
    BOOST_CHECK(overloads[f_idx++] == ref);
  });
  BOOST_CHECK(f_idx == overloads.size());
}

void test_or_error(const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors) {
  const auto fr = parse_function(f)
                    .and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); })
                    .and_then([&](const TypedFunction& f) { return type_check(e, f); });
  BOOST_REQUIRE(fr.has_value());

  const auto result = overload_resolution(e, fr.value());

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(result.error()));
  }
  BOOST_CHECK(expected_errors == result.error());
}

auto run_sema(Env e, std::string_view src) {
  e.sm.push_back({"src", std::string(src)});
  return parse2(std::move(e.ast), std::move(e.tg), SrcID{1}, src).and_then([&](AST ast, TypeGraph tg) {
    return sema(e.sm, e.type_cache, e.type_ids, e.copy_types, std::move(ast), std::move(tg));
  });
}

} // namespace

BOOST_AUTO_TEST_CASE(infer_empty) { test_inferred_inputs(Env{}, "()", "()", {{}}); }

BOOST_AUTO_TEST_CASE(infer_return_int) { test_inferred_inputs(Env{}, "0", "()", {}); }

BOOST_AUTO_TEST_CASE(infer_single_arg) { test_inferred_inputs(create_primative_env(), "a", "(a) ", {{0, 1}}); }

BOOST_AUTO_TEST_CASE(infer_active) { test_inferred_inputs(create_primative_env(), "a", "()", {}, {"a"}); }

BOOST_AUTO_TEST_CASE(infer_multi_arg) {
  test_inferred_inputs(
    create_primative_env(), "{ (a, ((b, c), d)) }", "(a, b, c, d)", {{3, 4}, {8, 9}, {11, 12}, {15, 16}});
}

BOOST_AUTO_TEST_CASE(infer_with_let) {
  test_inferred_inputs(create_primative_env(), "{ let x = a; x }", "(a)", {{10, 11}});
}

BOOST_AUTO_TEST_CASE(infer_multi_let) {
  test_inferred_inputs(create_primative_env(), "{ let x = a; let y = x; y }", "(a)", {{10, 11}});
}

BOOST_AUTO_TEST_CASE(nr_return) { test_nr(create_primative_env(), "() -> i32 = x", {type_id(knot::Type<i32>{})}); }

BOOST_AUTO_TEST_CASE(nr_arg) {
  test_nr(create_primative_env(), "(x: i32) -> () = ()", {type_id(knot::Type<i32>{}), TypeID::Invalid()});
}

BOOST_AUTO_TEST_CASE(nr_let) {
  test_nr(create_primative_env(), "() -> () { let x : i32 = y; x }", {TypeID::Invalid(), type_id(knot::Type<i32>{})});
}

BOOST_AUTO_TEST_CASE(nr_multi) {
  test_nr(create_primative_env(),
          "(x: i32) -> f32 { let x : string = y; x }",
          {type_id(knot::Type<i32>{}), type_id(knot::Type<f32>{}), type_id(knot::Type<std::string>{})});
}

BOOST_AUTO_TEST_CASE(nr_undefined_return) {
  test_nr_error(create_primative_env(), "() -> abc = x", {{{SrcID{0}, {6, 9}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_arg) {
  test_nr_error(create_primative_env(), "(x: abc) -> () = ()", {{{SrcID{0}, {4, 7}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_let) {
  test_nr_error(create_primative_env(), "() -> () { let x : abc = y; x }", {{{SrcID{0}, {19, 22}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_multi) {
  test_nr_error(create_primative_env(),
                "(x: a) -> b { let x : c = y; x }",
                {{{SrcID{0}, {4, 5}}, "undefined type"},
                 {{SrcID{0}, {10, 11}}, "undefined type"},
                 {{SrcID{0}, {22, 23}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(or_function_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(or_function_nested) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_or(e, {{"f", 0}, {"f", 0}}, "(x: i32) -> i32 = f(f(x))");
}

BOOST_AUTO_TEST_CASE(or_function_scope_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(or_function_assign) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 { let x: i32 = f(x); x }");
}

BOOST_AUTO_TEST_CASE(or_function_param) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});
  test_or(e, {{"f", 0}}, "(x: i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(or_function_multi) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(or_fn_overload_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) {});
  e.add_function("f", [](i32) {});
  test_or(e, {{"f", 0}}, "(x: &i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(or_fn_overload_input) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 = f(x)");
  test_or(e, {{"f", 1}}, "(x: f32) -> i32 = f(x)");
  test_or(e, {{"f", 0}, {"f", 1}}, "(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))");
}

BOOST_AUTO_TEST_CASE(or_fn_overload_output) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });
  test_or(e, {{"f", 0}}, "(x: i32) -> i32 = f(x)");
  test_or(e, {{"f", 1}}, "(x: i32) -> f32 = f(x)");
  test_or(e, {{"f", 0}, {"f", 1}}, "(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))");
}

BOOST_AUTO_TEST_CASE(or_param_borrow) {
  Env e = create_primative_env();
  e.add_function("ref", [](const i32& x) {});
  test_or(e, {{"ref"}}, "(x: i32) -> () = ref(&x)");
}

BOOST_AUTO_TEST_CASE(or_nested_fn_overload) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  e.add_function("g", [](i32) { return i32(); });

  e.add_function("h", []() { return i32(); });
  e.add_function("h", []() { return f32(); });

  test_or(e, {{"f", 0}, {"g", 0}, {"h", 0}}, "() -> i32 = f(g(h()))");
}

BOOST_AUTO_TEST_CASE(or_return_fn) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });
  test_or(e, {{"f", 0}}, "() -> fn() -> i32 = f");
}

BOOST_AUTO_TEST_CASE(or_fn_assign) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });
  test_or(e, {{"f", 0}}, "() -> i32 = { let x : fn() -> i32 = f; x() }");
}

BOOST_AUTO_TEST_CASE(or_partial) {
  test_or_error(create_primative_env(), "(x) -> _ = x", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(or_borrow_partial) {
  test_or_error(
    create_primative_env(), "(x) -> _ { let _ = &x; () }", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(or_tuple_partial) {
  test_or_error(create_primative_env(), "(x: (_)) -> _ = x", {{{1, 2}, "unable to fully deduce type, deduced: (_)"}});
  test_or_error(create_primative_env(), "(x) -> (_) = x", {{{1, 2}, "unable to fully deduce type, deduced: (_)"}});
}

BOOST_AUTO_TEST_CASE(or_undeclared_function) {
  test_or_error(create_primative_env(), "() -> () = missing()", {{{11, 18}, "use of undeclared binding 'missing'"}});
}

BOOST_AUTO_TEST_CASE(or_undeclared_binding) {
  test_or_error({}, "() -> _ = x", {{{10, 11}, "use of undeclared binding 'x'"}});
  test_or_error({}, "() -> _ = (x, 1)", {{{11, 12}, "use of undeclared binding 'x'"}});
  test_or_error({}, "() -> _ { let y = x; y }", {{{18, 19}, "use of undeclared binding 'x'"}});

  test_or_error({},
                "() -> () { let _y = x; z }",
                {{{20, 21}, "use of undeclared binding 'x'"}, {{23, 24}, "use of undeclared binding 'z'"}});
}

BOOST_AUTO_TEST_CASE(or_only_undeclared_error) {
  Env e = create_primative_env();
  e.add_function("f", [](int) {});

  test_or_error(e, "() -> _ = f(missing())", {{{12, 19}, "use of undeclared binding 'missing'"}});
}

BOOST_AUTO_TEST_CASE(ig_null) { BOOST_CHECK(calculate_ident_graph({}, {}) == Graph<ASTID>{}); }

BOOST_AUTO_TEST_CASE(ig_unbound) {
  const SrcMap sm = {{"", "x"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_scope) {
  // x, 1, assign, x, with
  const SrcMap sm = {{"", "{ let x = 1; x}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);
  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_unused) {
  // x, 1, assign, 1, with
  const SrcMap sm = {{"", "{ let x = 1; 1}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_multiple_uses) {
  // x, 1, assign, x, x, tuple, with
  const SrcMap sm = {{"", "{ let x = 1; (x, x)}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{3}, ASTID{4}}, {}, {}, {ASTID{0}}, {ASTID{0}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_scope_and_unbound) {
  // x, 1, assign, x, y, tuple, with
  const SrcMap sm = {{"", "{ let x = 1; (x, y)}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_scope_tuple) {
  // x, y, (), 1, assign, x, y, tuple, with
  const SrcMap sm = {{"", "{ let (x, y) = 1; (x, y)}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{5}}, {ASTID{6}}, {}, {}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_self_assign) {
  // x, x, assign, x, with
  const SrcMap sm = {{"", "{ let x = x; x}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_nested) {
  // ((x, ((x, x, assign), x, with), assign), x, assign)
  const SrcMap sm = {{"", "{ let x = { let x = x; x }; x}"}};
  const auto [ast, _] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{7}}, {ASTID{4}}, {}, {}, {ASTID{1}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn) {
  // x, (), x, fn
  const SrcMap sm = {{"", "(x) -> T = x"}};
  const auto [ast, _] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{2}}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn_tuple) {
  // x, y, (), x, y, (), fn
  const SrcMap sm = {{"", "(x, y) -> T = (x, y)"}};
  const auto [ast, _] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {ASTID{4}}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn_unbound) {
  // (), x, fn
  const SrcMap sm = {{"", "() -> T = x"}};
  const auto [ast, _] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_fn_unused) {
  // x, (), 1, fn
  const SrcMap sm = {{"", "(x) -> T = 1"}};
  const auto [ast, _] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_fn_nested) {
  // x, (), ((x, ((x, x, assign), x, with), assign), x, assign) fn
  const SrcMap sm = {{"", "(x) -> T { let x = { let x = x; x }; x}"}};
  const auto [ast, _] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{
    {ASTID{4}}, {}, {ASTID{9}}, {ASTID{6}}, {ASTID{0}}, {}, {ASTID{3}}, {}, {}, {ASTID{2}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_call_global) {
  Env e;
  e.add_function("f", []() {});
  e.sm.push_back({"", "fn g() -> _ = f()"});
  const auto [ast, tg] = check_result(parse2(std::move(e.ast), std::move(e.tg), SrcID{1}, e.sm[1].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(e.sm, ast);

  std::vector<std::vector<ASTID>> fanout(10);
  fanout[0] = {ASTID{5}};
  fanout[5] = {ASTID{0}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_overloaded) {
  const SrcMap sm = {{"",
                      "fn f(Y) -> _ = ()"
                      "fn f(X) -> _ = ()"
                      "fn g() -> _ = f()"}};
  const auto [ast, tg] = check_result(parse2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  std::vector<std::vector<ASTID>> fanout(19);
  fanout[0] = {ASTID{14}};
  fanout[6] = {ASTID{14}};
  fanout[14] = {ASTID{0}, ASTID{6}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_recursive) {
  const SrcMap sm = {{"", "fn f() -> _ = f()"}};
  const auto [ast, tg] = check_result(parse2({}, {}, SrcID{0}, sm[0].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(sm, ast);

  std::vector<std::vector<ASTID>> fanout(7);
  fanout[0] = {ASTID{2}};
  fanout[2] = {ASTID{0}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_unused_global) {
  Env e = create_primative_env();
  e.sm.push_back({"", "fn f() -> _ = ()"});

  const auto [ast, tg] = check_result(parse2(std::move(e.ast), std::move(e.tg), SrcID{1}, e.sm[1].src));
  const Graph<ASTID> ident_graph = calculate_ident_graph(e.sm, ast);

  BOOST_CHECK(Graph<ASTID>(std::vector<std::vector<ASTID>>(ast.forest.size())) == ident_graph);
}

BOOST_AUTO_TEST_CASE(sema_unresolved_fn) {
  const auto exp_errors =
    std::vector<ContextualError2>{{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: _"}};
  BOOST_CHECK(exp_errors == check_error(run_sema(create_primative_env(), "fn f(x) -> _ = x")));
}

BOOST_AUTO_TEST_CASE(sema_cg) {
  Env e;
  e.type_cache = create_type_cache(e.tg);

  e.add_type<i32>("i32");
  e.add_type<f32>("f32");

  constexpr std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn f(x: f32) -> f32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n"
    "fn g(x: f32) -> f32 = f(x)\n"
    "fn h(x: i32, y: f32) -> (i32, f32) = (g(x), g(y))\n";

  const auto [cg, ast_, tg] = check_result(run_sema(e, src));
  AST ast = std::move(ast_);

  e.sm.push_back({"file", std::string(src)});

  // clone(i32), clone(f32), f(i32), f(f32), g(i32), g(f32), h(i32, f32)
  const std::vector<ASTID> fns =
    transform_to_vec(ast.forest.root_ids(), [&](ASTID root) { return *ast.forest.first_child(root); });

  BOOST_REQUIRE_EQUAL(7, fns.size());

  // x, x, f, x, f, x, g, x, g, y
  const std::vector<ASTID> idents = transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) {
    return ast.forest[id] == ASTTag::ExprIdent ? std::optional(id) : std::nullopt;
  });

  BOOST_REQUIRE_EQUAL(10, idents.size());

  // clone, clone, f(), x, f(), x, g(), x, g(), x, h(), x, y
  const std::vector<ASTID> patterns = transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) {
    return ast.forest[id] == ASTTag::PatternIdent ? std::optional(id) : std::nullopt;
  });

  BOOST_REQUIRE_EQUAL(13, patterns.size());

  std::vector<std::vector<ASTID>> exp_call_graph(ast.forest.size());
  exp_call_graph[fns[4].get()] = {fns[2]};
  exp_call_graph[fns[5].get()] = {fns[3]};
  exp_call_graph[fns[6].get()] = {fns[4], fns[5]};

  BOOST_CHECK(Graph<ASTID>(exp_call_graph) == cg.call_graph);
  BOOST_CHECK(invert(cg.call_graph) == cg.inverted_call_graph);

  const Map<ASTID, ASTID> exp_overloads = {
    {idents[0], patterns[3]},
    {idents[1], patterns[5]},
    {idents[2], fns[2]},
    {idents[3], patterns[7]},
    {idents[4], fns[3]},
    {idents[5], patterns[9]},
    {idents[6], fns[4]},
    {idents[7], patterns[11]},
    {idents[8], fns[5]},
    {idents[9], patterns[12]}};
  BOOST_CHECK(exp_overloads == cg.binding_of);

  const std::vector<ASTID> exp_leaf_fns = {fns[0], fns[1], fns[2], fns[3]};
  BOOST_CHECK(exp_leaf_fns == cg.leaf_fns);
}

} // namespace ooze
