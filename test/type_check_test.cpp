#include "test.h"

#include "parser.h"
#include "parser_flat.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"
#include "user_msg.h"

#include <cctype>

namespace ooze {

namespace {

Graph<TypeRef, TypeTag, TypeID> remove_refs(TypeGraph full_graph) {
  auto&& [g, tags, refs, ids] = std::move(full_graph).decompose();
  return std::move(g).append_column(std::move(tags), std::move(ids));
};

auto type_checking(const Env& e, std::string_view src, bool debug = false) {
  const SrcMap sm = {{"src", std::string(src)}};
  return parse_function2({}, {}, SrcID{0}, src)
    .and_then(
      applied([&](AST ast, TypeGraph tg) { return type_name_resolution(sm, e, std::move(ast), std::move(tg)); }))
    .map(applied([&](AST ast, TypeGraph tg) {
      auto [ident_graph, undeclared_bindings] = calculate_ident_graph(sm, ast);
      return std::tuple(std::move(ast), std::move(tg), std::move(ident_graph), std::move(undeclared_bindings));
    }))
    .and_then(applied([&](AST ast, TypeGraph tg, auto ident_graph, auto undeclared_bindings) {
      return type_check(sm, e, ident_graph, undeclared_bindings, std::move(ast), std::move(tg), debug);
    }));
}

void test_tc(const Env& e, std::string_view src, std::string_view exp, bool debug = false) {
  auto [act_ast, act_tg] = check_result(type_checking(e, src, debug));
  auto [exp_ast, exp_tg] = check_result(type_checking(e, exp));

  BOOST_REQUIRE(act_ast.forest == exp_ast.forest);

  const auto exp_tg_no_refs = remove_refs(exp_tg);
  const auto act_tg_no_refs = remove_refs(act_tg);

  for(ASTID id : exp_ast.forest.ids()) {
    const TypeRef exp_type = exp_ast.types[id.get()];
    const TypeRef act_type = act_ast.types[id.get()];

    const bool same_types = compare_dags(exp_tg_no_refs, act_tg_no_refs, exp_type, act_type);

    if(!same_types) {
      fmt::print("{:<3} E: {} A: {}\n", id.get(), pretty_print(e, exp_tg, exp_type), pretty_print(e, act_tg, act_type));
    }

    BOOST_CHECK(same_types);
  }
}

void test_tc_error(
  const Env& e, std::string_view src, const std::vector<ContextualError2>& expected_errors, bool debug = false) {
  const auto errors = check_error(type_checking(e, src, debug));
  if(expected_errors != errors) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(errors));
    BOOST_CHECK(expected_errors == errors);
  }
}

auto type_graph_of(const Env& e, std::string_view src) {
  const SrcMap sm = {{"src", std::string(src)}};
  return std::get<1>(check_result(parse_type2({}, {}, SrcID{0}, src).and_then(applied([&](AST ast, TypeGraph tg) {
    return type_name_resolution(sm, e, std::move(ast), std::move(tg));
  }))));
};

void test_unify(std::string_view exp, std::string_view x, std::string_view y, bool recurse = true) {
  const Env e = create_primative_env();

  auto g = type_graph_of(e, x);

  const TypeRef xid{g.num_nodes() - 1};

  g.add_graph(type_graph_of(e, y));

  const TypeRef yid{g.num_nodes() - 1};

  const auto gexp = type_graph_of(e, exp);

  const TypeRef unified_type = unify(g, xid, yid, recurse);
  const TypeRef exp_type = TypeRef{gexp.num_nodes() - 1};

  BOOST_REQUIRE(unified_type != TypeRef::Invalid());
  BOOST_CHECK(compare_dags(remove_refs(std::move(gexp)), remove_refs(std::move(g)), exp_type, unified_type));
}

void test_unify_error(std::string_view x, std::string_view y, bool recurse = true) {
  const Env e = create_primative_env();

  auto g = type_graph_of(e, x);

  const TypeRef xid{g.num_nodes() - 1};

  g.add_graph(type_graph_of(e, y));

  const TypeRef yid{g.num_nodes() - 1};

  BOOST_REQUIRE(unify(g, xid, yid, recurse) == TypeRef::Invalid());
}

void test_alr_pass(std::string_view src,
                   std::vector<std::string> exp,
                   ContextualResult2<std::tuple<AST, TypeGraph>> parse_result) {
  const Env e = create_primative_env();
  auto [ast, tg] = check_result(type_name_resolution({{"src", std::string(src)}}, e, std::move(parse_result))
                                  .and_then(applied([&](AST ast, TypeGraph tg) {
                                    return apply_language_rules(e, std::move(ast), std::move(tg));
                                  })));

  BOOST_REQUIRE_EQUAL(exp.size(), ast.types.size());
  for(int i = 0; i < exp.size(); i++) {
    BOOST_CHECK_EQUAL(exp[i], pretty_print(e, tg, ast.types[i]));
  }
}

void test_alr_expr(std::string_view src, std::vector<std::string> exp) {
  return test_alr_pass(src, exp, parse_expr2({}, {}, SrcID{0}, src));
}

void test_alr_assign(std::string_view src, std::vector<std::string> exp) {
  return test_alr_pass(src, exp, parse_assignment2({}, {}, SrcID{0}, src));
}

void test_alr_fn(std::string_view src, std::vector<std::string> exp) {
  return test_alr_pass(src, exp, parse_function2({}, {}, SrcID{0}, src));
}

void test_alr_expr_error(std::string_view src, std::vector<ContextualError2> exp) {
  const Env e = create_primative_env();
  const SrcMap sm = {{"src", std::string(src)}};
  auto [ast, types] = check_result(type_name_resolution(sm, e, parse_expr2({}, {}, SrcID{0}, src)));
  const auto act = check_error(apply_language_rules(e, std::move(ast), std::move(types)));
  BOOST_CHECK(exp == act);
}

void test_alr_assign_error(std::string_view src, std::vector<ContextualError2> exp) {
  const Env e = create_primative_env();
  const SrcMap sm = {{"src", std::string(src)}};
  auto [ast, types] = check_result(type_name_resolution(sm, e, parse_assignment2({}, {}, SrcID{0}, src)));
  const auto act = check_error(apply_language_rules(e, ast, std::move(types)));
  BOOST_CHECK(exp == act);
}

} // namespace

BOOST_AUTO_TEST_SUITE(un)

BOOST_AUTO_TEST_CASE(floating) {
  test_unify("_", "_", "_");
  test_unify("()", "()", "_");
  test_unify("()", "_", "()");
  test_unify("i32", "i32", "_");
  test_unify("&_", "&_", "_");
  test_unify("fn() -> ()", "fn() -> ()", "_");
}

BOOST_AUTO_TEST_CASE(primitives) {
  test_unify("i32", "i32", "i32");
  test_unify("string", "string", "string");
  test_unify_error("i32", "f32");
}

BOOST_AUTO_TEST_CASE(borrow) {
  test_unify("&i32", "&i32", "&i32");
  test_unify("&i32", "&i32", "&_");
  test_unify("&_", "&i32", "&f32", false);
  test_unify_error("&i32", "&f32");
}

BOOST_AUTO_TEST_CASE(tuple) {
  test_unify("()", "()", "()");
  test_unify("(i32)", "(i32)", "(i32)");
  test_unify("(i32)", "(i32)", "(_)");
  test_unify("(i32, f32)", "(i32, _)", "(_, f32)");
  test_unify("(i32, _)", "(i32, _)", "(i32, _)");
  test_unify("(_)", "(i32)", "(f32)", false);

  test_unify_error("(i32)", "(f32)");
  test_unify_error("()", "(_)");
  test_unify_error("(_)", "()");
  test_unify_error("(_, _)", "(_)");
  test_unify_error("()", "(_)", false);
}

BOOST_AUTO_TEST_CASE(fn) {
  test_unify("fn() -> ()", "fn() -> ()", "fn() -> ()");
  test_unify("fn(i32) -> ()", "fn(i32) -> ()", "fn(i32) -> ()");
  test_unify("fn(i32) -> ()", "fn(_) -> ()", "fn(i32) -> _");

  test_unify_error("fn(i32) -> f32", "fn(i32) -> i32");
  test_unify_error("fn(f32) -> i32", "fn(i32) -> i32");
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(alr)

BOOST_AUTO_TEST_CASE(expr) {
  test_alr_expr("x", {"_"});
  test_alr_expr("1", {"i32"});
  test_alr_expr("&x", {"_", "&_"});
  test_alr_expr("()", {"()"});
  test_alr_expr("(1)", {"i32", "(_)"});
  test_alr_expr("f()", {"fn _ -> _", "()", "_"});

  test_alr_expr("select x { y } else { z }", {"bool", "_", "_", "_"});

  test_alr_expr_error("1()", {{{SrcID{0}, {0, 1}}, "expected fn _ -> _, given i32"}});
}

BOOST_AUTO_TEST_CASE(assign) {
  test_alr_assign("let x = y", {"_", "_", "()"});
  test_alr_assign("let x: _ = y", {"_", "_", "()"});
  test_alr_assign("let x: i32 = y", {"i32", "_", "()"});
  test_alr_assign("let (x): (i32) = y", {"_", "(i32)", "_", "()"});

  test_alr_assign_error("let (): (i32) = y", {{{SrcID{0}, {4, 6}}, "expected (), given (i32)"}});
  test_alr_assign_error("let (x): () = y", {{{SrcID{0}, {4, 7}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(tc)

BOOST_AUTO_TEST_CASE(empty) { test_tc(create_primative_env(), "() -> () = ()", "() -> () = ()"); }

BOOST_AUTO_TEST_CASE(return_literal) { test_tc(create_primative_env(), "() -> _ = 5", "() -> i32 = 5"); }

BOOST_AUTO_TEST_CASE(return_binding) { test_tc(create_primative_env(), "(x: i32) -> _ = x", "(x: i32) -> i32 = x"); }

BOOST_AUTO_TEST_CASE(return_tuple) {
  test_tc(create_primative_env(), "(x: i32) -> _ = (x)", "(x: i32) -> (i32) = (x)");
  test_tc(create_primative_env(), "(x: i32) -> (_) = (x)", "(x: i32) -> (i32) = (x)");
}

BOOST_AUTO_TEST_CASE(return_tuple_nested) {
  test_tc(create_primative_env(), "(x: i32) -> _ = (x, (x))", "(x: i32) -> (i32, (i32)) = (x, (x))");
}

BOOST_AUTO_TEST_CASE(param_up) {
  test_tc(create_primative_env(), "(x) -> i32 = x", "(x: i32) -> i32 = x");
  test_tc(create_primative_env(), "(x, y) -> (i32, f32) = (x, y)", "(x: i32, y: f32) -> (i32, f32) = (x, y)");
}

BOOST_AUTO_TEST_CASE(param_down) {
  test_tc(create_primative_env(), "(x: i32) -> _ = x", "(x: i32) -> i32 = x");
  test_tc(create_primative_env(), "(x: i32, y: f32) -> _ = (x, y)", "(x: i32, y: f32) -> (i32, f32) = (x, y)");
}

BOOST_AUTO_TEST_CASE(param_across) {
  test_tc(create_primative_env(), "(x) -> (i32, _) = (x, x)", "(x: i32) -> (i32, i32) = (x, x)");
}

BOOST_AUTO_TEST_CASE(through_assignment) {
  test_tc(create_primative_env(), "(x: i32) -> _ { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
  test_tc(create_primative_env(), "(x) -> i32 { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(assignment_hint) {
  test_tc(create_primative_env(), "(x) -> _ { let y : i32 = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(assignment_tuple) {
  test_tc(create_primative_env(), "() -> _ { let x = (1); x }", "() -> (i32) { let x : (i32) = (1); x }");
}

BOOST_AUTO_TEST_CASE(assignment_literal) {
  test_tc(create_primative_env(), "() -> _ { let y = 5; y }", "() -> i32 { let y : i32 = 5; y }");
}

BOOST_AUTO_TEST_CASE(apply_fn) {
  test_tc(create_primative_env(), "(f: fn(i32) -> i32, x) -> _ = f(x)", "(f: fn(i32) -> i32, x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(apply_fn_deduce_arg) {
  test_tc(create_primative_env(), "(f: fn(_) -> i32, x:i32) -> _ = f(x)", "(f: fn(i32) -> i32, x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(apply_fn_deduce_arg_ref) {
  test_tc(
    create_primative_env(), "(f: fn(_) -> i32, x:i32) -> _ = f(&x)", "(f: fn(&i32) -> i32, x: i32) -> i32 = f(&x)");
}

BOOST_AUTO_TEST_CASE(global_fn) { test_tc(create_primative_env(), "() -> _ = f()", "() -> _ = f()"); }

BOOST_AUTO_TEST_CASE(global_fn_deduce) {
  test_tc(create_primative_env(),
          "(x: i32) -> i32 { let g = f; g(x) }",
          "(x: i32) -> i32 { let g: fn(i32) -> i32 = f; g(x) }");
}

BOOST_AUTO_TEST_CASE(parameter_tuple) {
  test_tc(create_primative_env(), "((x): (_)) -> i32 = x", "((x): (i32)) -> i32 = x");
  test_tc(create_primative_env(), "((x)) -> i32 = x", "((x): (i32)) -> i32 = x");

  test_tc(create_primative_env(), "(x) -> (i32) = (x)", "(x: i32) -> (i32) = (x)");
  test_tc(create_primative_env(), "(x, y) -> (i32, (i32)) = (x, (y))", "(x: i32, y: i32) -> (i32, (i32)) = (x, (y))");
}

BOOST_AUTO_TEST_CASE(unpack_tuple_up) {
  test_tc(create_primative_env(),
          "(tuple) -> (i32, i32) { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(unpack_tuple_down) {
  test_tc(create_primative_env(),
          "(tuple: (i32, i32)) -> _ { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(nested_tuple) {
  test_tc(create_primative_env(),
          "(x: i32) -> _ = (x, (x, (x), x))",
          "(x: i32) -> (i32, (i32, (i32), i32)) = (x, (x, (x), x))");
}

BOOST_AUTO_TEST_CASE(fn_identity) {
  test_tc(create_primative_env(), "(x: fn(i32) -> i32) -> _ = x", "(x: fn(i32) -> i32) -> fn(i32) -> i32 = x");
}

// BOOST_AUTO_TEST_CASE(return_fn) {
//   Env e = create_primative_env();
//   e.add_function("f", []() { return 1; });
//   test_tc(e, "() -> _ = f", "() -> fn() -> i32 = f");
// }

BOOST_AUTO_TEST_CASE(fn_arg) { test_tc(create_primative_env(), "(f) -> i32 = f()", "(f : fn() -> i32) -> i32 = f()"); }

BOOST_AUTO_TEST_CASE(fn_call) { test_tc(create_primative_env(), "(f) -> i32 = f()", "(f: fn() -> i32) -> i32 = f()"); }

BOOST_AUTO_TEST_CASE(fn_select_from_return) {
  test_tc(create_primative_env(),
          "(a, b, c) -> i32 = select a { b } else { c }",
          "(a: bool, b: i32, c: i32) -> i32 = select a { b } else { c }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_arg) {
  test_tc(create_primative_env(),
          "(a, b: i32, c) -> _ = select a { b } else { c }",
          "(a: bool, b: i32, c: i32) -> i32 = select a { b } else { c }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_constant) {
  test_tc(create_primative_env(),
          "(a, b) -> _ = select a { b } else { 1 }",
          "(a: bool, b: i32) -> i32 = select a { b } else { 1 }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_cond) {
  test_tc(create_primative_env(),
          "(a, b) -> _ = select a { a } else { b }",
          "(a: bool, b: bool) -> bool = select a { a } else { b }");
}

// BOOST_AUTO_TEST_CASE(fn_assign) {
//   Env e = create_primative_env();
//   e.add_function("f", []() { return 1; });

//   test_tc(e, "() -> i32 = { let x = f; x() }", "() -> i32 = { let x : fn() -> i32 = f; x() }");
// }

// BOOST_AUTO_TEST_CASE(fn_tuple) {
//   Env e = create_primative_env();
//   e.add_function("f", []() { return 1; });

//   test_tc(create_primative_env(),
//           "(f) -> i32 = { let (x) = f; x() }",
//           "(f : (fn() -> i32)) -> i32 = { let (x) : (fn() -> i32) = f; x() }");
// }

BOOST_AUTO_TEST_CASE(scope) {
  constexpr std::string_view input =
    "(a: i32) -> _ {"
    "  let b = {"
    "    let c = a;"
    "    let a = \"abc\";"
    "    (a, c)"
    "  };"
    "  (a, b)"
    "}";

  constexpr std::string_view output =
    "(a: i32) -> (i32, (string, i32)) {"
    "  let b : (string, i32) = {"
    "    let c : i32 = a;"
    "    let a : string = \"abc\";"
    "    (a, c)"
    "  };"
    "  (a, b)"
    "}";

  test_tc(create_primative_env(), input, output);
}

BOOST_AUTO_TEST_CASE(function_identity) { test_tc(create_primative_env(), "(x) -> _ = x", "(x) -> _ = x"); }

// BOOST_AUTO_TEST_CASE(function_return) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
// }

// BOOST_AUTO_TEST_CASE(function_nested) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   test_tc(e, "(x: i32) -> i32 = f(f(x))", "(x: i32) -> i32 = f(f(x))");
// }

// BOOST_AUTO_TEST_CASE(function_scope_return) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
// }

// BOOST_AUTO_TEST_CASE(function_assign) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   test_tc(e, "(x: i32) -> i32 { let x: i32 = f(x); x }", "(x: i32) -> i32 { let x: i32 = f(x); x }");
// }

// BOOST_AUTO_TEST_CASE(function_param) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) {});
//   test_tc(e, "(x: i32) -> () = f(x)", "(x: i32) -> () = f(x)");
// }

// BOOST_AUTO_TEST_CASE(function_multi) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   e.add_function("f", [](f32 x) { return x; });
//   test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
// }

// BOOST_AUTO_TEST_CASE(prop_single_function) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32 x) { return x; });
//   test_tc(e, "(x) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
// }

// BOOST_AUTO_TEST_CASE(fn_overload_borrow) {
//   Env e = create_primative_env();
//   e.add_function("f", [](const i32&) {});
//   e.add_function("f", [](i32) {});
//   test_tc(e, "(x: &i32) -> () = f(x)", "(x: &i32) -> () = f(x)");
// }

// BOOST_AUTO_TEST_CASE(fn_overload_input) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) { return i32(); });
//   e.add_function("f", [](f32) { return i32(); });
//   test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
//   test_tc(e, "(x: f32) -> i32 = f(x)", "(x: f32) -> i32 = f(x)");
//   test_tc(e, "(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))", "(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))");
// }

// BOOST_AUTO_TEST_CASE(fn_overload_output) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) { return i32(); });
//   e.add_function("f", [](i32) { return f32(); });
//   test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
//   test_tc(e, "(x: i32) -> f32 = f(x)", "(x: i32) -> f32 = f(x)");
//   test_tc(e, "(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))", "(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))");
// }

BOOST_AUTO_TEST_CASE(constant) {
  test_tc(create_primative_env(), "() -> _ = 1", "() -> i32 = 1");
  test_tc(create_primative_env(), "() -> _ = 'abc'", "() -> string = 'abc'");
}

// BOOST_AUTO_TEST_CASE(fn_up) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) { return i32(); });
//   e.add_function("f", [](f32) { return f32(); });

//   test_tc(e, "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
//   test_tc(e, "(x) -> f32 = f(x)", "(x: f32) -> f32 = f(x)");
//   test_tc(e, "(x, y) -> (i32, f32) { (f(x), f(y)) }", "(x: i32, y: f32) -> (i32, f32) { (f(x), f(y)) }");
// }

// BOOST_AUTO_TEST_CASE(fn_down) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) { return i32(); });
//   e.add_function("f", [](f32) { return f32(); });

//   test_tc(e, "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
//   test_tc(e, "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
//   test_tc(e, "(x: i32, y: f32) -> _ = (f(x), f(y))", "(x: i32, y: f32) -> (i32, f32) = (f(x), f(y))");
// }

BOOST_AUTO_TEST_CASE(borrow) {
  test_tc(create_primative_env(), "(x: i32) -> _ { let _ = &x; () }", "(x : i32) -> () { let _ : &i32 = &x; () }");
}

// BOOST_AUTO_TEST_CASE(param_borrow) {
//   Env e = create_primative_env();
//   e.add_function("ref", [](const i32& x) {});
//   test_tc(e, "(x) -> () = ref(&x)", "(x: i32) -> () = ref(&x)");
// }

// BOOST_AUTO_TEST_CASE(nested_fn_overload) {
//   Env e = create_primative_env();
//   e.add_function("f", [](i32) { return i32(); });
//   e.add_function("f", [](f32) { return i32(); });
//   e.add_function("g", [](i32) { return i32(); });
//   e.add_function("h", []() { return i32(); });
//   e.add_function("h", []() { return f32(); });

//   test_tc(e, "() -> i32 = f(g(h()))", "() -> i32 = f(g(h()))");
// }

BOOST_AUTO_TEST_CASE(invalid_borrow_expr) {
  test_tc_error(
    create_primative_env(), "() -> _ { let _ = &(1, 1); 1 }", {{{SrcID{0}, {18, 25}}, "cannot borrow a tuple"}});
  test_tc_error(
    create_primative_env(), "() -> _ { let _ = &&1; 1 }", {{{SrcID{0}, {18, 21}}, "cannot borrow a borrow"}});
  test_tc_error(
    create_primative_env(), "(x : (i32)) -> _ { let _ = &x; 1 }", {{{SrcID{0}, {27, 29}}, "cannot borrow a tuple"}});
}

BOOST_AUTO_TEST_CASE(invalid_borrow_pattern) {
  // TODO update error to highlight type instead of pattern
  test_tc_error(create_primative_env(), "(_ : &&i32) -> _ = 1", {{{SrcID{0}, {1, 2}}, "cannot borrow a borrow"}});
}

BOOST_AUTO_TEST_CASE(return_borrow) {
  test_tc_error(create_primative_env(), "() -> _ = &1", {{{SrcID{0}, {10, 12}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "() -> _ = (&1)", {{{SrcID{0}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(),
                "() -> _ = (&1, &2)",
                {{{SrcID{0}, {11, 13}}, "cannot return a borrowed value"},
                 {{SrcID{0}, {15, 17}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_borrow_ident) {
  test_tc_error(
    create_primative_env(), "(x : &i32) -> _ = x", {{{SrcID{0}, {18, 19}}, "cannot return a borrowed value"}});
  test_tc_error(
    create_primative_env(), "(x : &i32) -> _ = (x)", {{{SrcID{0}, {19, 20}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(),
                "(x : &i32) -> _ = (x, x)",
                {{{SrcID{0}, {19, 20}}, "cannot return a borrowed value"},
                 {{SrcID{0}, {22, 23}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_floating_borrow) {
  test_tc_error(create_primative_env(), "(x) -> _ = &x", {{{SrcID{0}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(
    create_primative_env(), "(x : &_) -> _ = x", {{{SrcID{0}, {16, 17}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "(x) -> _ = (&x)", {{{SrcID{0}, {12, 14}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(pattern_mismatch) {
  test_tc_error(
    create_primative_env(), "() -> () { let () = (1); () }", {{{SrcID{0}, {15, 17}}, "expected (), given (i32)"}});
  test_tc_error({}, "() -> () { let (x) = (); () }", {{{SrcID{0}, {15, 18}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_type_mismatch) {
  test_tc_error(create_primative_env(), "() -> () = 1", {{{SrcID{0}, {11, 12}}, "expected i32, given ()"}});
  test_tc_error(create_primative_env(), "() -> () = (1)", {{{SrcID{0}, {11, 14}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_arity_mismatch) {
  test_tc_error(create_primative_env(),
                "() -> (i32, i32) = (1, 1, 1)",
                {{{SrcID{0}, {19, 28}}, "expected (_, _, _), given (i32, i32)"}});
}

BOOST_AUTO_TEST_CASE(unused_binding) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ = 1",
                {{{SrcID{0}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; 1 }",
                {{{SrcID{0}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = 1; x }",
                {{{SrcID{0}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; let x = 1; x }",
                {{{SrcID{0}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_tuple) {
  test_tc_error(create_primative_env(),
                "((x): (i32)) -> _ = 1",
                {{{SrcID{0}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let (x) = (1); 1 }",
                {{{SrcID{0}, {15, 16}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_self_assign) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = x; 1 }",
                {{{SrcID{0}, {20, 21}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_scope) {
  test_tc_error(create_primative_env(),
                "() -> _ { { let x = 1; 1 } }",
                {{{SrcID{0}, {16, 17}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_ignore) {
  test_tc(create_primative_env(), "(_x: i32) -> i32 = 1", "(_x: i32) -> i32 = 1");
}

BOOST_AUTO_TEST_CASE(binding_reuse) {
  test_tc_error(
    create_primative_env(), "(x: string) -> _ = (x, x)", {{{SrcID{0}, {1, 2}}, "binding 'x' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_reuse_floating) {
  test_tc(create_primative_env(), "(x) -> _ = (x, x)", "(x) -> _ = (x, x)");
}

BOOST_AUTO_TEST_CASE(binding_reuse_both) {
  test_tc_error(create_primative_env(),
                "(x: string, y: i32) -> _ { let z = (x, y); (z, z) }",
                {{{SrcID{0}, {31, 32}}, "binding 'z' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_ref_reuse) {
  test_tc(create_primative_env(), "(x: &_) -> _ = f(x, x)", "(x: &_) -> _ = f(x, x)");
}

BOOST_AUTO_TEST_CASE(binding_ref_value_reuse) {
  test_tc(create_primative_env(), "(x: _) -> _ = f(&x, x)", "(x: _) -> _ = f(&x, x)");
}

BOOST_AUTO_TEST_CASE(binding_reuse_copy) {
  test_tc(create_primative_env(), "(x: i32) -> _ = (x, x)", "(x: i32) -> (i32, i32) = (x, x)");
}

BOOST_AUTO_TEST_CASE(function_ident_reuse) {
  test_tc_error(create_primative_env(),
                "(x: i32, x: i32) -> _ = x",
                {{{SrcID{0}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, x): (i32, i32)) -> _ = x",
                {{{SrcID{0}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, (x)): (i32, (i32))) -> _ = x",
                {{{SrcID{0}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

// BOOST_AUTO_TEST_CASE(wrong_arg_count) {
//   Env env = create_primative_env();
//   env.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(env, "() -> i32 = identity()", {{{SrcID{0}, {20, 22}}, "expected (), given (i32)"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_arg) {
//   Env env = create_primative_env();
//   env.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(env, "(x: u32) -> i32 = identity(x)", {{{SrcID{0}, {27, 28}}, "expected u32, given i32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_bind_count) {
//   Env env = create_primative_env();
//   env.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(env, "(x: i32) -> i32 { let () = identity(x); x }", {{{SrcID{0}, {27, 38}}, "expected (), given
//   i32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_return_count) {
//   Env env = create_primative_env();
//   env.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(env, "(x: i32) -> () = identity(x)", {{{SrcID{0}, {17, 28}}, "expected (), given i32"}});
// }

// BOOST_AUTO_TEST_CASE(multi_overload_match) {
//   Env env = create_primative_env();
//   env.add_function("f", []() { return i32(); });
//   env.add_function("f", []() { return f32(); });

//   test_tc_error(env, "() -> (i32, f32) { let x = f(); (x, x) }", {{{SrcID{0}, {23, 24}}, "expected i32, given
//   f32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_arg_type) {
//   Env e = create_primative_env();
//   e.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(e, "(x: u32) -> i32 = identity(x)", {{{SrcID{0}, {27, 28}}, "expected u32, given i32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_bind_type) {
//   Env e = create_primative_env();
//   e.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(e, "(x: i32) -> i32 { let x: u32 = identity(x); x }", {{{SrcID{0}, {22, 23}}, "expected u32, given
//   i32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_return_type) {
//   Env e = create_primative_env();
//   e.add_function("identity", [](i32 x) { return x; });

//   test_tc_error(e, "(x: i32) -> u32 = identity(x)", {{{SrcID{0}, {18, 29}}, "expected u32, given i32"}});
// }

// BOOST_AUTO_TEST_CASE(wrong_value_type) {
//   Env e = create_primative_env();
//   e.add_function("val", [](i32 x) {});
//   test_tc_error(e, "(x: &i32) -> () = val(x)", {{{SrcID{0}, {22, 23}}, "expected &i32, given i32"}});
// }

// BOOST_AUTO_TEST_CASE(empty_tuple_as_arg) {
//   Env e = create_primative_env();
//   e.add_function("take", [](i32) {});
//   test_tc_error(e, "() -> () = take(())", {{{SrcID{0}, {16, 18}}, "expected (), given i32"}});
// }

BOOST_AUTO_TEST_CASE(wrong_type) {
  Env e = create_primative_env();

  test_tc_error(e, "(x: i32) -> f32 = x", {{{SrcID{0}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y = x; y }", {{{SrcID{0}, {22, 23}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: i32 = x; y }", {{{SrcID{0}, {34, 35}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> (f32) { let y: i32 = x; (y) }", {{{SrcID{0}, {37, 38}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: f32 = x; y }", {{{SrcID{0}, {22, 23}}, "expected f32, given i32"}});
  test_tc_error(e, "() -> f32 = 1", {{{SrcID{0}, {12, 13}}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x = 1; x }", {{{SrcID{0}, {16, 17}}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x: f32 = 1; x }", {{{SrcID{0}, {16, 17}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(return_wrong_type_tuple_arg) {
  test_tc_error(create_primative_env(), "((x): (i32)) -> f32 = x", {{{SrcID{0}, {2, 3}}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(missized_pattern) {
  Env e = create_primative_env();

  test_tc_error(e, "(() : (_)) -> _ = 1", {{{SrcID{0}, {1, 3}}, "expected (), given (_)"}});
  test_tc_error(e, "((x) : ()) -> _ = 1", {{{SrcID{0}, {1, 4}}, "expected (_), given ()"}});
  test_tc_error(e, "((x) : (_, _)) -> _ = 1", {{{SrcID{0}, {1, 4}}, "expected (_), given (_, _)"}});

  test_tc_error(e, "() -> _ { let () = (1); 1 }", {{{SrcID{0}, {14, 16}}, "expected (), given (i32)"}});
  test_tc_error(e, "() -> _ { let (x) = (); 1 }", {{{SrcID{0}, {14, 17}}, "expected (_), given ()"}});
  test_tc_error(e, "() -> _ { let (x) = (1, 1); 1 }", {{{SrcID{0}, {14, 17}}, "expected (_), given (i32, i32)"}});

  test_tc_error(e, "(x) -> _ { let () : (_) = x; 1 }", {{{SrcID{0}, {15, 17}}, "expected (), given (_)"}});
  test_tc_error(e, "(x) -> _ { let (x) : () = x; 1 }", {{{SrcID{0}, {15, 18}}, "expected (_), given ()"}});
  test_tc_error(e, "(x) -> _ { let (x) : (_, _) = x; 1 }", {{{SrcID{0}, {15, 18}}, "expected (_), given (_, _)"}});
}

BOOST_AUTO_TEST_CASE(return_copy_ref_arg) {
  test_tc_error(create_primative_env(), "(x: &i32) -> i32 = x", {{{SrcID{0}, {1, 2}}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
