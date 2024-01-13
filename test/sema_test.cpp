#include "test.h"

#include "frontend_helpers.h"
#include "parser.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

void test_nr(Env e, std::string_view src, const std::vector<TypeID>& exp) {
  auto [ast, tg] = check_result(
    parse_and_name_resolution(parse_function, make_sv_array(e.src, src), e.native_types.names, {}, {}, SrcID{1}));
  const auto act_types = std::get<2>(std::move(tg).decompose());

  if(exp != act_types) {
    fmt::print("E {}\n", knot::debug(exp));
    fmt::print("A {}\n", knot::debug(act_types));
    BOOST_CHECK(exp == act_types);
  }
}

void test_nr_error(Env e, std::string_view src, const std::vector<ContextualError2>& expected_errors) {
  const auto errors = check_error(
    parse_and_name_resolution(parse_function, make_sv_array(e.src, src), e.native_types.names, {}, {}, SrcID{1}));

  if(expected_errors != errors) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(errors));
    BOOST_CHECK(expected_errors == errors);
  }
}

template <typename Result>
void check_sema(Result sema_result,
                const Map<int, std::vector<int>>& exp_cg,
                const std::vector<int>& exp_leaf_fns_indices,
                const std::vector<int>& exp_binding_of_indices) {
  const CallGraphData act = std::move(std::get<0>(sema_result));
  const AST ast = std::move(std::get<1>(sema_result));

  const std::vector<ASTID> fns =
    transform_to_vec(ast.forest.root_ids(), [&](ASTID root) { return *ast.forest.first_child(root); });

  const std::vector<ASTID> expr_idents = transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) {
    return ast.forest[id] == ASTTag::ExprIdent ? std::optional(id) : std::nullopt;
  });

  const std::vector<ASTID> patterns = transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) {
    return (ast.forest[id] == ASTTag::PatternIdent || ast.forest[id] == ASTTag::PatternWildCard)
             ? std::optional(id)
             : std::nullopt;
  });

  const auto to_fn = [&](int fn_idx) {
    BOOST_REQUIRE(fn_idx < fns.size());
    return fns[fn_idx];
  };

  const std::vector<ASTID> exp_leaf_fns = transform_to_vec(exp_leaf_fns_indices, to_fn);
  BOOST_CHECK(exp_leaf_fns == act.leaf_fns);

  std::vector<std::vector<ASTID>> exp_fanout(ast.forest.size());
  for(const auto& [fn, fanout] : exp_cg) {
    exp_fanout[to_fn(fn).get()] = transform_to_vec(fanout, to_fn);
  }
  BOOST_CHECK(Graph<ASTID>(exp_fanout) == act.call_graph);

  BOOST_REQUIRE(exp_binding_of_indices.size() == expr_idents.size());
  Map<ASTID, ASTID> exp_binding_of;
  for(int i = 0; i < expr_idents.size(); i++) {
    const int pat_idx = exp_binding_of_indices[i];
    BOOST_REQUIRE(pat_idx < patterns.size());
    exp_binding_of.emplace(expr_idents[i], patterns[pat_idx]);
  }
  BOOST_CHECK(exp_binding_of == act.binding_of);
}

auto run_sema(Env e, std::string_view src) {
  const auto srcs = make_sv_array(e.src, src);
  return parse_and_name_resolution(parse, srcs, e.native_types.names, std::move(e.ast), std::move(e.tg), SrcID{1})
    .and_then([&](AST ast, TypeGraph tg) {
      return sema(srcs, e.type_cache, e.native_types, std::move(ast), std::move(tg));
    });
}

void test_sema_error(Env e, std::string_view src, const std::vector<ContextualError2>& expected_errors) {
  const auto srcs = make_sv_array(e.src, src);
  const auto [errors, ast, type] = check_error_state(
    parse_and_name_resolution(parse_function, srcs, e.native_types.names, std::move(e.ast), std::move(e.tg), SrcID{1})
      .and_then([&](AST ast, TypeGraph tg) {
        return sema(srcs, e.type_cache, e.native_types, std::move(ast), std::move(tg));
      }));

  if(expected_errors != errors) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(errors));
  }
  BOOST_CHECK(expected_errors == errors);
}

} // namespace

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
  test_nr_error(create_primative_env(), "() -> abc = x", {{{SrcID{1}, {6, 9}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_arg) {
  test_nr_error(create_primative_env(), "(x: abc) -> () = ()", {{{SrcID{1}, {4, 7}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_let) {
  test_nr_error(create_primative_env(), "() -> () { let x : abc = y; x }", {{{SrcID{1}, {19, 22}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_multi) {
  test_nr_error(create_primative_env(),
                "(x: a) -> b { let x : c = y; x }",
                {{{SrcID{1}, {4, 5}}, "undefined type"},
                 {{SrcID{1}, {10, 11}}, "undefined type"},
                 {{SrcID{1}, {22, 23}}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(sema_function_return) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", [](i32 x) { return x; });
  check_sema(check_result(run_sema(std::move(e), "fn g(x: i32) -> i32 = f(x)")),
             {{2, {1}}}, // g -> f
             {0, 1},     // clone, f
             {1, 3});    // f(), x
}

BOOST_AUTO_TEST_CASE(sema_function_nested) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", [](i32 x) { return x; });
  check_sema(check_result(run_sema(std::move(e), "fn g(x: i32) -> i32 = f(f(x))")),
             {{2, {1}}}, // g -> f
             {0, 1},     // clone, f
             {1, 1, 3}); // f(), f(), x
}

BOOST_AUTO_TEST_CASE(sema_function_param) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", [](i32 x) {});
  check_sema(check_result(run_sema(std::move(e), "fn g(x: i32) -> () = f(x)")),
             {{2, {1}}}, // g -> f
             {0, 1},     // clone, f
             {1, 3});    // f(), x
}

BOOST_AUTO_TEST_CASE(sema_function_overload) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });
  check_sema(check_result(run_sema(e, "fn g(x: i32) -> i32 = f(x)")),
             {{4, {2}}},   // g -> f(i32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {2, 5});      // f(i32), x

  check_sema(check_result(run_sema(e, "fn g(x: f32) -> f32 = f(x)")),
             {{4, {3}}},   // g -> f(f32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {3, 5});      // f(f32), x

  check_sema(check_result(run_sema(e, "fn g(x: i32, y: f32) -> _ = (f(x), f(y))")),
             {{4, {2, 3}}}, // g -> f(i32), f(f32)
             {0, 1, 2, 3},  // clone, clone, f, f
             {2, 5, 3, 6}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(sema_param_borrow) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", [](const i32& x) {});
  check_sema(check_result(run_sema(std::move(e), "fn g(x: i32) -> () = f(&x)")),
             {{2, {1}}}, // g -> f
             {0, 1},     // clone, f
             {1, 3});    // f(), x
}

BOOST_AUTO_TEST_CASE(sema_fn_overload_borrow) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", [](const i32&) {});
  e.add_function("f", [](i32) {});
  check_sema(check_result(run_sema(std::move(e), "fn g(x: &i32) -> () = f(x)")),
             {{3, {1}}}, // g -> f(i32)
             {0, 1, 2},  // clone, f, f
             {1, 4});    // f(i32), x
}

BOOST_AUTO_TEST_CASE(sema_function_overload_input) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");
  e.add_function("f", [](i32 x) { return 0; });
  e.add_function("f", [](f32 x) { return 0; });

  check_sema(check_result(run_sema(e, "fn g(x: i32) -> i32 = f(x)")),
             {{4, {2}}},   // g -> f(i32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {2, 5});      // f(i32), x

  check_sema(check_result(run_sema(e, "fn g(x: f32) -> i32 = f(x)")),
             {{4, {3}}},   // g -> f(f32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {3, 5});      // f(f32), x

  check_sema(check_result(run_sema(e, "fn g(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))")),
             {{4, {2, 3}}}, // g -> f(i32), f(f32)
             {0, 1, 2, 3},  // clone, clone, f, f
             {2, 5, 3, 6}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(sema_fn_overload_output) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });

  check_sema(check_result(run_sema(e, "fn g(x: i32) -> i32 = f(x)")),
             {{4, {2}}},   // g -> f(i32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {2, 5});      // f(i32), x

  check_sema(check_result(run_sema(e, "fn g(x: i32) -> f32 = f(x)")),
             {{4, {3}}},   // g -> f(f32)
             {0, 1, 2, 3}, // clone, clone, f, f
             {3, 5});      // f(f32), x

  check_sema(check_result(run_sema(e, "fn g(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))")),
             {{4, {2, 3}}}, // g -> f(i32), f(f32)
             {0, 1, 2, 3},  // clone, clone, f, f
             {2, 5, 3, 6}); // f(i32), x, f(f32), y
}

BOOST_AUTO_TEST_CASE(sema_nested_fn_overload) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");

  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  e.add_function("g", [](i32) { return i32(); });

  e.add_function("h", []() { return i32(); });
  e.add_function("h", []() { return f32(); });

  check_sema(check_result(run_sema(e, "fn g() -> _ = f(g(h()))")),
             {{7, {2, 4, 5}}},      // g -> f(i32), g(i32), h() -> i32
             {0, 1, 2, 3, 4, 5, 6}, // clone, clone, f, f, g, h
             {2, 4, 5});            // f(i32), g(i32), h() -> i32
}

BOOST_AUTO_TEST_CASE(sema_return_fn) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", []() { return 1; });
  check_sema(check_result(run_sema(e, "fn g() -> _ = f")),
             {{2, {1}}}, // g -> f()
             {0, 1},     // clone, f
             {1});       // f()
}

BOOST_AUTO_TEST_CASE(sema_fn_assign) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_function("f", []() { return 1; });
  check_sema(check_result(run_sema(e, "fn g() -> i32 = { let x = f; x() }")),
             {{2, {1}}}, // g -> f()
             {0, 1},     // clone, f
             {1, 3});    //  f(), x
}

BOOST_AUTO_TEST_CASE(sema_partial) {
  test_sema_error(
    create_primative_env(), "(x) -> _ = x", {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(sema_borrow_partial) {
  test_sema_error(create_primative_env(),
                  "(x) -> _ { let _ = &x; () }",
                  {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(sema_tuple_partial) {
  test_sema_error(
    create_primative_env(), "(x: (_)) -> _ = x", {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: (_)"}});
  test_sema_error(
    create_primative_env(), "(x) -> (_) = x", {{{SrcID{1}, {1, 2}}, "unable to fully deduce type, deduced: (_)"}});
}

BOOST_AUTO_TEST_CASE(sema_undeclared_function) {
  test_sema_error(
    create_primative_env(), "() -> () = missing()", {{{SrcID{1}, {11, 18}}, "use of undeclared binding 'missing'"}});
}

BOOST_AUTO_TEST_CASE(sema_unresolved_fn) {
  const auto exp_errors =
    std::vector<ContextualError2>{{{SrcID{1}, {5, 6}}, "unable to fully deduce type, deduced: _"}};
  BOOST_CHECK(exp_errors == check_error(run_sema(create_primative_env(), "fn f(x) -> _ = x")));
}

BOOST_AUTO_TEST_CASE(ig_null) { BOOST_CHECK(check_result(calculate_ident_graph({}, {})) == Graph<ASTID>{}); }

BOOST_AUTO_TEST_CASE(ig_unbound) {
  const auto srcs = make_sv_array("x");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError2> exp_errors = {{{SrcID{0}, {0, 1}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(ig_unbound_fn) {
  const auto srcs = make_sv_array("x()");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError2> exp_errors = {{{SrcID{0}, {0, 1}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(ig_scope) {
  // x, 1, assign, x, with
  const auto srcs = make_sv_array("{ let x = 1; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));
  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_unused) {
  // x, 1, assign, 1, with
  const auto srcs = make_sv_array("{ let x = 1; 1}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));
  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_multiple_uses) {
  // x, 1, assign, x, x, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, x)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{3}, ASTID{4}}, {}, {}, {ASTID{0}}, {ASTID{0}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_scope_and_unbound) {
  // x, 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let x = 1; (x, y)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError2> exp_errors = {{{SrcID{0}, {17, 18}}, "use of undeclared binding 'y'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(ig_scope_tuple) {
  // x, y, (), 1, assign, x, y, tuple, with
  const auto srcs = make_sv_array("{ let (x, y) = 1; (x, y)}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{5}}, {ASTID{6}}, {}, {}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_self_assign) {
  // x, x, assign, x, with
  const auto srcs = make_sv_array("{ let x = x; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError2> exp_errors = {{{SrcID{0}, {10, 11}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(ig_nested) {
  // ((x, ((x, 1, assign), x, with), assign), x, assign)
  const auto srcs = make_sv_array("{ let x = { let x = 1; x }; x}");
  const AST ast = std::get<1>(check_result(parse_expr({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp =
    std::vector<std::vector<ASTID>>{{ASTID{7}}, {ASTID{4}}, {}, {}, {ASTID{1}}, {}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn) {
  // x, (), x, fn
  const auto srcs = make_sv_array("(x) -> T = x");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{2}}, {}, {ASTID{0}}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn_tuple) {
  // x, y, (), x, y, (), fn
  const auto srcs = make_sv_array("(x, y) -> T = (x, y)");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{{ASTID{3}}, {ASTID{4}}, {}, {ASTID{0}}, {ASTID{1}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_fn_unbound) {
  // (), x, fn
  const auto srcs = make_sv_array("() -> T = x");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const auto errors = check_error(calculate_ident_graph(srcs, ast));
  const std::vector<ContextualError2> exp_errors = {{{SrcID{0}, {10, 11}}, "use of undeclared binding 'x'"}};
  BOOST_CHECK(exp_errors == errors);
}

BOOST_AUTO_TEST_CASE(ig_fn_unused) {
  // x, (), 1, fn
  const auto srcs = make_sv_array("(x) -> T = 1");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  BOOST_CHECK_EQUAL(0, ident_graph.num_edges());
}

BOOST_AUTO_TEST_CASE(ig_fn_nested) {
  // x, (), ((x, ((x, x, assign), x, with), assign), x, assign) fn
  const auto srcs = make_sv_array("(x) -> T { let x = { let x = x; x }; x}");
  const AST ast = std::get<1>(check_result(parse_function({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  const Graph<ASTID> exp = std::vector<std::vector<ASTID>>{
    {ASTID{4}}, {}, {ASTID{9}}, {ASTID{6}}, {ASTID{0}}, {}, {ASTID{3}}, {}, {}, {ASTID{2}}, {}, {}};
  BOOST_CHECK(exp == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_call_global) {
  Env e;
  e.add_function("f", []() {});
  const auto srcs = make_sv_array(e.src, "fn g() -> _ = f()");
  const AST ast = std::get<1>(check_result(parse(std::move(e.ast), std::move(e.tg), SrcID{1}, srcs[1])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  std::vector<std::vector<ASTID>> fanout(10);
  fanout[0] = {ASTID{5}};
  fanout[5] = {ASTID{0}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_overloaded) {
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

BOOST_AUTO_TEST_CASE(ig_recursive) {
  const auto srcs = make_sv_array("fn f() -> _ = f()");
  const AST ast = std::get<1>(check_result(parse({}, {}, SrcID{0}, srcs[0])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  std::vector<std::vector<ASTID>> fanout(7);
  fanout[0] = {ASTID{2}};
  fanout[2] = {ASTID{0}};

  BOOST_CHECK(Graph<ASTID>(std::move(fanout)) == ident_graph);
}

BOOST_AUTO_TEST_CASE(ig_unused_global) {
  Env e = create_primative_env();
  const auto srcs = make_sv_array(e.src, "fn f() -> _ = ()");
  const AST ast = std::get<1>(check_result(parse(std::move(e.ast), std::move(e.tg), SrcID{1}, srcs[1])));
  const Graph<ASTID> ident_graph = check_result(calculate_ident_graph(srcs, ast));

  BOOST_CHECK(Graph<ASTID>(std::vector<std::vector<ASTID>>(ast.forest.size())) == ident_graph);
}

} // namespace ooze
