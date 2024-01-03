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

template <typename Parser>
auto run_tc(Parser p, Env e, std::string_view src, bool debug = false) {
  const auto srcs = make_sv_array(e.src, src);
  return p(std::move(e.ast), std::move(e.tg), SrcID{1}, src)
    .and_then([&](AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    })
    .and_then([&](AST ast, TypeGraph tg) {
      return apply_language_rules(srcs, e.type_cache, std::move(ast), std::move(tg));
    })
    .and_then([&](AST ast, TypeGraph tg) {
      return calculate_ident_graph(srcs, ast).append_state(std::move(ast), std::move(tg));
    })
    .map([&](Graph<ASTID> ig, AST ast, TypeGraph tg) {
      auto propagations = calculate_propagations(ig, ast.forest);
      return std::tuple(std::tuple(std::move(ig), std::move(propagations)), std::move(ast), std::move(tg));
    })
    .and_then(flattened([&](Graph<ASTID> ident_graph, auto propagations, AST ast, TypeGraph tg) {
      return constraint_propagation(
        srcs, e.type_cache, e.copy_types, ident_graph, propagations, std::move(ast), std::move(tg), debug);
    }));
}

template <typename R>
void compare_tc(R exp_result, R act_result) {
  auto [exp_ast, exp_tg] = std::move(exp_result);
  auto [act_ast, act_tg] = std::move(act_result);

  BOOST_REQUIRE(act_ast.forest == exp_ast.forest);

  const auto exp_tg_no_refs = remove_refs(exp_tg);
  const auto act_tg_no_refs = remove_refs(act_tg);

  for(ASTID id : exp_ast.forest.ids()) {
    const TypeRef exp_type = exp_ast.types[id.get()];
    const TypeRef act_type = act_ast.types[id.get()];

    if(!compare_dags(exp_tg_no_refs, act_tg_no_refs, exp_type, act_type)) {
      fmt::print("ID {}\n", id.get());
      fmt::print("E {}\n", knot::debug(exp_tg.template get<TypeTag>(exp_type)));
      fmt::print("A {}\n", knot::debug(act_tg.template get<TypeTag>(act_type)));
    }

    BOOST_CHECK(compare_dags(exp_tg_no_refs, act_tg_no_refs, exp_type, act_type));
  }
}

void test_tc(const Env& e, std::string_view src, std::string_view exp, bool debug = false) {
  compare_tc(check_result(run_tc(parse_function2, e, exp, debug)), check_result(run_tc(parse_function2, e, src)));
}

void test_tc_fns(const Env& e, std::string_view src, std::string_view exp, bool debug = false) {
  compare_tc(check_result(run_tc(parse2, e, exp, debug)), check_result(run_tc(parse2, e, src)));
}

void test_tc_error(
  const Env& e, std::string_view src, const std::vector<ContextualError2>& expected_errors, bool debug = false) {
  const auto errors = check_error(run_tc(parse_function2, e, src, debug));
  if(expected_errors != errors) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(errors));
    BOOST_CHECK(expected_errors == errors);
  }
}

TypeGraph type_graph_of(
  Span<std::string_view> srcs, const std::unordered_map<std::string, TypeID>& type_ids, SrcID src, TypeGraph tg) {
  return std::get<1>(
    check_result(parse_type2({}, std::move(tg), src, srcs[src.get()]).and_then([&](AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    })));
};

void test_unify(std::string_view exp, std::string_view x, std::string_view y, bool recurse = true) {
  Env e = create_primative_env();
  const auto srcs = make_sv_array(e.src, x, y, exp);

  e.tg = type_graph_of(srcs, e.type_ids, SrcID{1}, std::move(e.tg));
  const TypeRef xid{e.tg.num_nodes() - 1};

  e.tg = type_graph_of(srcs, e.type_ids, SrcID{2}, std::move(e.tg));
  const TypeRef yid{e.tg.num_nodes() - 1};

  e.tg = type_graph_of(srcs, e.type_ids, SrcID{3}, std::move(e.tg));
  const TypeRef exp_type = TypeRef{e.tg.num_nodes() - 1};

  const TypeRef unified_type = unify(e.type_cache, e.tg, xid, yid, recurse);
  BOOST_REQUIRE(unified_type != TypeRef::Invalid());
  BOOST_CHECK(compare_dags(remove_refs(e.tg), exp_type, unified_type));
}

void test_unify_error(std::string_view x, std::string_view y, bool recurse = true) {
  Env e = create_primative_env();
  const auto srcs = make_sv_array(e.src, x, y);

  e.tg = type_graph_of(srcs, e.type_ids, SrcID{1}, std::move(e.tg));
  const TypeRef xid{e.tg.num_nodes() - 1};

  e.tg = type_graph_of(srcs, e.type_ids, SrcID{2}, std::move(e.tg));
  const TypeRef yid{e.tg.num_nodes() - 1};

  BOOST_REQUIRE(unify(e.type_cache, e.tg, xid, yid, recurse) == TypeRef::Invalid());
}

template <typename Parser>
auto run_alr(Parser p, const Env& e, std::string_view src) {
  const auto srcs = make_sv_array(e.src, src);
  return p(std::move(e.ast), std::move(e.tg), SrcID{1}, src)
    .and_then([&](AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, e.type_ids, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    })
    .and_then([&](AST ast, TypeGraph tg) {
      return apply_language_rules(srcs, e.type_cache, std::move(ast), std::move(tg));
    });
}

template <typename Parser>
void test_alr(Parser p, std::string_view src, std::vector<std::string> exp) {
  Env e = create_primative_env();

  const size_t initial_ast_size = e.ast.forest.size();

  auto [ast, tg] = check_result(run_alr(p, e, src));

  BOOST_REQUIRE_EQUAL(exp.size(), ast.types.size() - initial_ast_size);
  for(int i = 0; i < exp.size(); i++) {
    BOOST_CHECK_EQUAL(exp[i], pretty_print(make_sv_array(e.src, src), tg, ast.types[i + initial_ast_size]));
  }
}

template <typename Parser>
void test_alr_error(Parser p, std::string_view src, std::vector<ContextualError2> exp) {
  BOOST_CHECK(exp == check_error(run_alr(p, create_primative_env(), src)));
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
  test_alr(parse_expr2, "x", {"_"});
  test_alr(parse_expr2, "1", {"i32"});
  test_alr(parse_expr2, "&x", {"_", "&_"});
  test_alr(parse_expr2, "()", {"()"});
  test_alr(parse_expr2, "(1)", {"i32", "(_)"});
  test_alr(parse_expr2, "f()", {"fn _ -> _", "()", "_"});

  test_alr(parse_expr2, "select x { y } else { z }", {"bool", "_", "_", "_"});

  test_alr_error(parse_expr2, "1()", {{{SrcID{1}, {0, 1}}, "expected fn _ -> _, given i32"}});
}

BOOST_AUTO_TEST_CASE(assign) {
  test_alr(parse_assignment2, "let x = y", {"_", "_", "()"});
  test_alr(parse_assignment2, "let x: _ = y", {"_", "_", "()"});
  test_alr(parse_assignment2, "let x: i32 = y", {"i32", "_", "()"});
  test_alr(parse_assignment2, "let (x): (i32) = y", {"_", "(i32)", "_", "()"});

  test_alr_error(parse_assignment2, "let (): (i32) = y", {{{SrcID{1}, {4, 6}}, "expected (), given (i32)"}});
  test_alr_error(parse_assignment2, "let (x): () = y", {{{SrcID{1}, {4, 7}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(tc)

BOOST_AUTO_TEST_CASE(empty) { test_tc_fns({}, "", ""); }

BOOST_AUTO_TEST_CASE(empty_fn) { test_tc(create_primative_env(), "() -> () = ()", "() -> () = ()"); }

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

BOOST_AUTO_TEST_CASE(global_fn) {
  Env e = create_empty_env();
  e.add_function("f", []() {});
  test_tc(std::move(e), "() -> () = f()", "() -> () = f()");
}

BOOST_AUTO_TEST_CASE(global_fn_deduce) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  test_tc_fns(std::move(e),
              "fn f(x) -> _ = x\n"
              "fn g(x: i32) -> i32 { let g = f; g(x) }",
              "fn f(x) -> _ = x\n"
              "fn g(x: i32) -> i32 { let g: fn(i32) -> i32 = f; g(x) }");
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

BOOST_AUTO_TEST_CASE(env) { test_tc_fns(create_primative_env(), "", ""); }

BOOST_AUTO_TEST_CASE(fn_return) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });
  test_tc(e, "() -> _ = f", "() -> fn() -> i32 = f");
}

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

BOOST_AUTO_TEST_CASE(fn_assign) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });

  test_tc(e, "() -> i32 = { let x = f; x() }", "() -> i32 = { let x : fn() -> i32 = f; x() }");
}

BOOST_AUTO_TEST_CASE(fn_tuple) {
  test_tc(create_primative_env(),
          "(f) -> i32 = { let (x) = f; x() }",
          "(f : (fn() -> i32)) -> i32 = { let (x) : (fn() -> i32) = f; x() }");
}

BOOST_AUTO_TEST_CASE(fn_recursive) {
  const std::string_view src = "fn f(x: i32) -> i32 = f(x)";
  auto [act_ast, act_tg] = check_result(run_tc(parse2, create_primative_env(), src));
}

BOOST_AUTO_TEST_CASE(fn_multi) {
  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x) -> _ = f(x)\n";
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  test_tc_fns(e, src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_prop_down) {
  const std::string_view src =
    "fn f(x: i32) -> _ = x\n"
    "fn g(x) -> _ = f(x)\n";
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  test_tc_fns(e, src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_dont_prop_up) {
  const std::string_view src =
    "fn f(x) -> _ = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  test_tc_fns(e, src, src);
}

BOOST_AUTO_TEST_CASE(fn_multi_or) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");

  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn f(x: f32) -> f32 = x\n"
    "fn g(x: i32) -> _ = f(x)\n"
    "fn g(x: f32) -> _ = f(x)\n";
  ;
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn f(x: f32) -> f32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n"
    "fn g(x: f32) -> f32 = f(x)\n";
  test_tc_fns(e, src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_or_global) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<f32>("f32");
  e.add_function("f", [](f32 x) { return x; });

  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> _ = f(x)\n"
    "fn g(x: f32) -> _ = f(x)\n";
  ;
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n"
    "fn g(x: f32) -> f32 = f(x)\n";

  test_tc_fns(e, src, exp);
}

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

BOOST_AUTO_TEST_CASE(function_return) {
  Env e = create_empty_env();
  e.add_function("f", []() {});
  test_tc(e, "() -> _ = f()", "() -> () = f()");
}

BOOST_AUTO_TEST_CASE(function_nested) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x) -> _ = f(f(x))", "(x: i32) -> i32 = f(f(x))");
}

BOOST_AUTO_TEST_CASE(function_arg) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});
  test_tc(e, "(x) -> () = f(x)", "(x: i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(function_overloaded) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });

  test_tc(e, "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");

  test_tc(e, "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
  test_tc(e, "(x) -> f32 = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(prop_single_function) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) {});
  e.add_function("f", [](i32) {});
  test_tc(e, "(x: &_) -> () = f(x)", "(x: &i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_input) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_tc(e, "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x: f32) -> _ = f(x)", "(x: f32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_output) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });
  test_tc(e, "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x) -> f32 = f(x)", "(x: i32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(constant) {
  test_tc(create_primative_env(), "() -> _ = 1", "() -> i32 = 1");
  test_tc(create_primative_env(), "() -> _ = 'abc'", "() -> string = 'abc'");
}

BOOST_AUTO_TEST_CASE(fn_up) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return f32(); });

  test_tc(e, "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x) -> f32 = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_down) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return f32(); });

  test_tc(e, "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(borrow) {
  test_tc(create_primative_env(), "(x: i32) -> _ { let _ = &x; () }", "(x : i32) -> () { let _ : &i32 = &x; () }");
}

BOOST_AUTO_TEST_CASE(param_borrow) {
  Env e = create_primative_env();
  e.add_function("ref", [](const i32& x) {});
  test_tc(e, "(x) -> () = ref(&x)", "(x: i32) -> () = ref(&x)");
}

BOOST_AUTO_TEST_CASE(nested_fn_overload) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  e.add_function("g", [](i32) { return i32(); });
  e.add_function("h", []() { return i32(); });
  e.add_function("h", []() { return f32(); });

  test_tc(e, "() -> _ = f(g(h()))", "() -> i32 = f(g(h()))");
}

BOOST_AUTO_TEST_CASE(invalid_borrow_expr) {
  test_tc_error(
    create_primative_env(), "() -> _ { let _ = &(1, 1); 1 }", {{{SrcID{1}, {18, 25}}, "cannot borrow a tuple"}});
  test_tc_error(
    create_primative_env(), "() -> _ { let _ = &&1; 1 }", {{{SrcID{1}, {18, 21}}, "cannot borrow a borrow"}});
  test_tc_error(
    create_primative_env(), "(x : (i32)) -> _ { let _ = &x; 1 }", {{{SrcID{1}, {27, 29}}, "cannot borrow a tuple"}});
}

BOOST_AUTO_TEST_CASE(invalid_borrow_pattern) {
  test_tc_error(create_primative_env(), "(_ : &&i32) -> _ = 1", {{{SrcID{1}, {1, 2}}, "cannot borrow a borrow"}});
}

BOOST_AUTO_TEST_CASE(return_borrow) {
  test_tc_error(create_primative_env(), "() -> _ = &1", {{{SrcID{1}, {10, 12}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "() -> _ = (&1)", {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(),
                "() -> _ = (&1, &2)",
                {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"},
                 {{SrcID{1}, {15, 17}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_borrow_ident) {
  test_tc_error(
    create_primative_env(), "(x : &i32) -> _ = x", {{{SrcID{1}, {18, 19}}, "cannot return a borrowed value"}});
  test_tc_error(
    create_primative_env(), "(x : &i32) -> _ = (x)", {{{SrcID{1}, {19, 20}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(),
                "(x : &i32) -> _ = (x, x)",
                {{{SrcID{1}, {19, 20}}, "cannot return a borrowed value"},
                 {{SrcID{1}, {22, 23}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_floating_borrow) {
  test_tc_error(create_primative_env(), "(x) -> _ = &x", {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(
    create_primative_env(), "(x : &_) -> _ = x", {{{SrcID{1}, {16, 17}}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "(x) -> _ = (&x)", {{{SrcID{1}, {12, 14}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(pattern_mismatch) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  test_tc_error(e, "() -> () { let () = (1); () }", {{{SrcID{1}, {15, 17}}, "expected (), given (i32)"}});
  test_tc_error(e, "() -> () { let (x) = (); () }", {{{SrcID{1}, {15, 18}}, "expected (()), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_type_mismatch) {
  test_tc_error(create_primative_env(), "() -> () = 1", {{{SrcID{1}, {11, 12}}, "expected i32, given ()"}});
  test_tc_error(create_primative_env(), "() -> () = (1)", {{{SrcID{1}, {11, 14}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_arity_mismatch) {
  test_tc_error(create_primative_env(),
                "() -> (i32, i32) = (1, 1, 1)",
                {{{SrcID{1}, {19, 28}}, "expected (_, _, _), given (i32, i32)"}});
}

BOOST_AUTO_TEST_CASE(unused_binding) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ = 1",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; 1 }",
                {{{SrcID{1}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = 1; x }",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; let x = 1; x }",
                {{{SrcID{1}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_tuple) {
  test_tc_error(create_primative_env(),
                "((x): (i32)) -> _ = 1",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let (x) = (1); 1 }",
                {{{SrcID{1}, {15, 16}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_self_assign) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = x; 1 }",
                {{{SrcID{1}, {20, 21}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_scope) {
  test_tc_error(create_primative_env(),
                "() -> _ { { let x = 1; 1 } }",
                {{{SrcID{1}, {16, 17}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_ignore) {
  test_tc(create_primative_env(), "(_x: i32) -> i32 = 1", "(_x: i32) -> i32 = 1");
}

BOOST_AUTO_TEST_CASE(binding_reuse) {
  test_tc_error(
    create_primative_env(), "(x: string) -> _ = (x, x)", {{{SrcID{1}, {1, 2}}, "binding 'x' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_reuse_floating) {
  test_tc(create_primative_env(), "(x) -> _ = (x, x)", "(x) -> _ = (x, x)");
}

BOOST_AUTO_TEST_CASE(binding_reuse_both) {
  test_tc_error(create_primative_env(),
                "(x: string, y: i32) -> _ { let z = (x, y); (z, z) }",
                {{{SrcID{1}, {31, 32}}, "binding 'z' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_ref_reuse) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&, const i32&) {});
  test_tc(std::move(e), "(x: &_) -> _ = f(x, x)", "(x: &_) -> _ = f(x, x)");
}

BOOST_AUTO_TEST_CASE(binding_ref_value_reuse) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&, i32) {});
  test_tc(std::move(e), "(x: _) -> _ = f(&x, x)", "(x: _) -> _ = f(&x, x)");
}

BOOST_AUTO_TEST_CASE(binding_reuse_copy) {
  test_tc(create_primative_env(), "(x: i32) -> _ = (x, x)", "(x: i32) -> (i32, i32) = (x, x)");
}

BOOST_AUTO_TEST_CASE(function_ident_reuse) {
  test_tc_error(create_primative_env(),
                "(x: i32, x: i32) -> _ = x",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, x): (i32, i32)) -> _ = x",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, (x)): (i32, (i32))) -> _ = x",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "() -> i32 = identity()", {{{SrcID{1}, {20, 22}}, "expected (), given (i32)"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: u32) -> i32 = identity(x)", {{{SrcID{1}, {1, 2}}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: i32) -> i32 { let () = identity(x); x }", {{{SrcID{1}, {22, 24}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: i32) -> () = identity(x)", {{{SrcID{1}, {17, 28}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(multi_overload_match) {
  Env env = create_primative_env();
  env.add_function("f", []() { return i32(); });
  env.add_function("f", []() { return f32(); });

  test_tc_error(env, "() -> (i32, f32) { let x = f(); (x, x) }", {{{SrcID{1}, {23, 24}}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(e, "(x: u32) -> i32 = identity(x)", {{{SrcID{1}, {1, 2}}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_type) {
  Env e = create_empty_env();
  e.add_type<i32>("i32");
  e.add_type<u32>("u32");
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(
    e, "(x: i32) -> i32 { let x: u32 = identity(x); x }", {{{SrcID{1}, {22, 23}}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(e, "(x: i32) -> u32 = identity(x)", {{{SrcID{1}, {18, 29}}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_value_type) {
  Env e = create_primative_env();
  e.add_function("val", [](i32 x) {});
  test_tc_error(e, "(x: &i32) -> () = val(x)", {{{SrcID{1}, {1, 2}}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_CASE(empty_tuple_as_arg) {
  Env e = create_primative_env();
  e.add_function("take", [](i32) {});
  test_tc_error(e, "() -> () = take(())", {{{SrcID{1}, {16, 18}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_type) {
  Env e = create_primative_env();

  test_tc_error(e, "(x: i32) -> f32 = x", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: i32 = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> (f32) { let y: i32 = x; (y) }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: f32 = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 = 1", {{{SrcID{1}, {12, 13}}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x = 1; x }", {{{SrcID{1}, {16, 17}}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x: f32 = 1; x }", {{{SrcID{1}, {16, 17}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(return_wrong_type_tuple_arg) {
  test_tc_error(create_primative_env(), "((x): (i32)) -> f32 = x", {{{SrcID{1}, {2, 3}}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(missized_pattern) {
  Env e = create_primative_env();

  test_tc_error(e, "(() : (_)) -> _ = 1", {{{SrcID{1}, {1, 3}}, "expected (), given (_)"}});
  test_tc_error(e, "((x) : ()) -> _ = 1", {{{SrcID{1}, {1, 4}}, "expected (_), given ()"}});
  test_tc_error(e, "((x) : (_, _)) -> _ = 1", {{{SrcID{1}, {1, 4}}, "expected (_), given (_, _)"}});

  test_tc_error(e, "() -> _ { let () = (1); 1 }", {{{SrcID{1}, {14, 16}}, "expected (), given (i32)"}});
  test_tc_error(e, "() -> _ { let (x) = (); 1 }", {{{SrcID{1}, {14, 17}}, "expected (()), given ()"}});
  test_tc_error(e, "() -> _ { let (x) = (1, 1); 1 }", {{{SrcID{1}, {14, 17}}, "expected (_), given (i32, i32)"}});

  test_tc_error(e, "(x) -> _ { let () : (_) = x; 1 }", {{{SrcID{1}, {15, 17}}, "expected (), given (_)"}});
  test_tc_error(e, "(x) -> _ { let (x) : () = x; 1 }", {{{SrcID{1}, {15, 18}}, "expected (_), given ()"}});
  test_tc_error(e, "(x) -> _ { let (x) : (_, _) = x; 1 }", {{{SrcID{1}, {15, 18}}, "expected (_), given (_, _)"}});
}

BOOST_AUTO_TEST_CASE(return_copy_ref_arg) {
  test_tc_error(create_primative_env(), "(x: &i32) -> i32 = x", {{{SrcID{1}, {1, 2}}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
