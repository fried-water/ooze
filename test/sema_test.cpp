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

void test_nr(const Env& e, std::string_view src, const std::vector<TypeID>& exp) {
  const SrcMap sm = {{"", std::string(src)}};
  const auto result =
    parse_function2({}, {}, SrcID{0}, src).and_then(applied([&](const AST& ast, const UnresolvedTypes& types) {
      return type_name_resolution(sm, e, types.graph);
    }));

  BOOST_REQUIRE(result.has_value());

  if(exp != result.value()) {
    fmt::print("E {}\n", knot::debug(exp));
    fmt::print("A {}\n", knot::debug(result.value()));
    BOOST_CHECK(exp == result.value());
  }
}

void test_nr_error(const Env& e, std::string_view src, const std::vector<ContextualError2>& expected_errors) {
  const SrcMap sm = {{"", std::string(src)}};
  const auto errors = check_error(
    parse_function2({}, {}, SrcID{0}, src).and_then(applied([&](const AST& ast, const UnresolvedTypes& types) {
      return type_name_resolution(sm, e, types.graph);
    })));

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

BOOST_AUTO_TEST_CASE(ig_null) {
  const auto [ident_graph, unbound] = calculate_ident_graph({}, {});
  BOOST_CHECK(ident_graph == Graph<ASTID>{});
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_unbound) {
  const SrcMap sm = {{"", "x"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
  check_range(std::array{ASTID{0}}, unbound);
}

BOOST_AUTO_TEST_CASE(ig_scope) {
  // x, 1, assign, x, with
  const SrcMap sm = {{"", "{ let x = 1; x}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);
  BOOST_CHECK(unbound.empty());

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_unused) {
  // x, 1, assign, 1, with
  const SrcMap sm = {{"", "{ let x = 1; 1}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_multiple_uses) {
  // x, 1, assign, x, x, tuple, with
  const SrcMap sm = {{"", "{ let x = 1; (x, x)}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);
  BOOST_CHECK(unbound.empty());

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{3}, ASTID{4}}, {}, {}, {ASTID{0}}, {ASTID{0}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_scope_and_unbound) {
  // x, 1, assign, x, y, tuple, with
  const SrcMap sm = {{"", "{ let x = 1; (x, y)}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
  check_range(std::array{ASTID{4}}, unbound);
}

BOOST_AUTO_TEST_CASE(ig_scope_tuple) {
  // x, y, (), 1, assign, x, y, tuple, with
  const SrcMap sm = {{"", "{ let (x, y) = 1; (x, y)}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{5}}, {ASTID{6}}, {}, {}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_self_assign) {
  // x, x, assign, x, with
  const SrcMap sm = {{"", "{ let x = x; x}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
  check_range(std::array{ASTID{1}}, unbound);
}

BOOST_AUTO_TEST_CASE(ig_nested) {
  // ((x, ((x, x, assign), x, with), assign), x, assign)
  const SrcMap sm = {{"", "{ let x = { let x = x; x }; x}"}};
  const auto [ast, types] = check_result(parse_expr2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{7}}, {ASTID{4}}, {}, {}, {ASTID{1}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
  check_range(std::array{ASTID{2}}, unbound);
}

BOOST_AUTO_TEST_CASE(ig_fn) {
  // x, (), x, fn
  const SrcMap sm = {{"", "(x) -> T = x"}};
  const auto [ast, types] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{2}}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_fn_tuple) {
  // x, y, (), x, y, (), fn
  const SrcMap sm = {{"", "(x, y) -> T = (x, y)"}};
  const auto [ast, types] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {ASTID{4}}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_fn_unbound) {
  // (), x, fn
  const SrcMap sm = {{"", "() -> T = x"}};
  const auto [ast, types] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
  check_range(std::array{ASTID{1}}, unbound);
}

BOOST_AUTO_TEST_CASE(ig_fn_unused) {
  // x, (), 1, fn
  const SrcMap sm = {{"", "(x) -> T = 1"}};
  const auto [ast, types] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
  BOOST_CHECK(unbound.empty());
}

BOOST_AUTO_TEST_CASE(ig_fn_nested) {
  // x, (), ((x, ((x, x, assign), x, with), assign), x, assign) fn
  const SrcMap sm = {{"", "(x) -> T { let x = { let x = x; x }; x}"}};
  const auto [ast, types] = check_result(parse_function2({}, {}, SrcID{0}, sm[0].src));
  const auto [ident_graph, unbound] = calculate_ident_graph(sm, ast);

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{
    {ASTID{4}}, {}, {ASTID{9}}, {ASTID{6}}, {ASTID{0}}, {}, {ASTID{3}}, {}, {}, {ASTID{2}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
  BOOST_CHECK(unbound.empty());
}

} // namespace ooze
