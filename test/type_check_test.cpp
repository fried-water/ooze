#include "test.h"

#include "frontend_helpers.h"
#include "parser.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"
#include "user_msg.h"

#include <cctype>

namespace ooze {

namespace {

template <typename Parser>
auto run_tc(Parser p, TestEnv env, std::string_view src, bool debug = false) {
  const TypeCache tc = create_type_cache(env.tg);

  const auto srcs = make_sv_array(env.src, src);
  return parse_and_name_resolution(p, srcs, env.types.names, std::move(env.ast), std::move(env.tg), SrcID{1})
    .and_then([&](AST ast, TypeGraph tg) {
      return apply_language_rules(srcs, tc, env.types.names, std::move(ast), std::move(tg));
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
        srcs, tc, env.types, ident_graph, propagations, std::move(ast), std::move(tg), debug);
    }));
}

template <typename R>
void compare_tc(R exp_result, R act_result) {
  auto [exp_ast, exp_tg] = std::move(exp_result);
  auto [act_ast, act_tg] = std::move(act_result);

  BOOST_REQUIRE(act_ast.forest == exp_ast.forest);

  for(ASTID id : exp_ast.forest.ids()) {
    const Type exp_type = exp_ast.types[id.get()];
    const Type act_type = act_ast.types[id.get()];

    if(!compare_dags(exp_tg, act_tg, exp_type, act_type)) {
      fmt::print("ID {}\n", id.get());
      fmt::print("E {}\n", knot::debug(exp_tg.template get<TypeTag>(exp_type)));
      fmt::print("A {}\n", knot::debug(act_tg.template get<TypeTag>(act_type)));
    }

    BOOST_CHECK(compare_dags(exp_tg, act_tg, exp_type, act_type));
  }
}

void test_tc(TestEnv env, std::string_view src, std::string_view exp, bool debug = false) {
  compare_tc(check_result(run_tc(parse_function, env, exp, debug)), check_result(run_tc(parse_function, env, src)));
}

void test_tc_fns(TestEnv env, std::string_view src, std::string_view exp, bool debug = false) {
  compare_tc(check_result(run_tc(parse, env, exp, debug)), check_result(run_tc(parse, env, src)));
}

template <typename Parser>
void test_tc_error(Parser parser,
                   TestEnv env,
                   std::string_view src,
                   const std::vector<ContextualError>& expected_errors,
                   bool debug = false) {
  const auto errors = check_error(run_tc(parser, std::move(env), src, debug));
  check_eq("error", expected_errors, errors);
}

void test_tc_error(
  TestEnv env, std::string_view src, const std::vector<ContextualError>& expected_errors, bool debug = false) {
  test_tc_error(parse_function, std::move(env), src, expected_errors, debug);
}

TypeGraph type_graph_of(Span<std::string_view> srcs, const TypeNames& names, SrcID src, TypeGraph tg) {
  return std::get<1>(check_result(parse_and_name_resolution(parse_type, srcs, names, {}, std::move(tg), src)));
};

void test_unify(std::string_view exp, std::string_view x, std::string_view y, bool recurse = true) {
  TestEnv env = basic_test_env();
  const TypeCache tc = create_type_cache(env.tg);
  const auto srcs = make_sv_array(env.src, x, y, exp);

  env.tg = type_graph_of(srcs, env.types.names, SrcID{1}, std::move(env.tg));
  const Type xid{env.tg.num_nodes() - 1};

  env.tg = type_graph_of(srcs, env.types.names, SrcID{2}, std::move(env.tg));
  const Type yid{env.tg.num_nodes() - 1};

  env.tg = type_graph_of(srcs, env.types.names, SrcID{3}, std::move(env.tg));
  const Type exp_type = Type{env.tg.num_nodes() - 1};

  const Type unified_type = unify(tc, env.tg, xid, yid, recurse);
  BOOST_REQUIRE(unified_type != Type::Invalid());
  BOOST_CHECK(compare_dags(env.tg, exp_type, unified_type));
}

void test_unify_error(std::string_view x, std::string_view y, bool recurse = true) {
  TestEnv env = basic_test_env();
  const TypeCache tc = create_type_cache(env.tg);
  const auto srcs = make_sv_array(env.src, x, y);

  env.tg = type_graph_of(srcs, env.types.names, SrcID{1}, std::move(env.tg));
  const Type xid{env.tg.num_nodes() - 1};

  env.tg = type_graph_of(srcs, env.types.names, SrcID{2}, std::move(env.tg));
  const Type yid{env.tg.num_nodes() - 1};

  BOOST_REQUIRE(unify(tc, env.tg, xid, yid, recurse) == Type::Invalid());
}

template <typename Parser>
auto run_alr(Parser p, Span<std::string_view> srcs, const NativeTypeInfo& types, AST ast, TypeGraph tg) {
  return parse_and_name_resolution(p, srcs, types.names, std::move(ast), std::move(tg), SrcID{1})
    .and_then([&](AST ast, TypeGraph tg) {
      const TypeCache tc = create_type_cache(tg);
      return apply_language_rules(srcs, tc, types.names, std::move(ast), std::move(tg));
    });
}

template <typename Parser>
void test_alr(Parser p, std::string_view src, std::vector<std::string> exp) {
  auto [types, env_src, ast, tg] = basic_test_env();
  const auto srcs = make_sv_array(env_src, src);

  const size_t initial_ast_size = ast.forest.size();

  std::tie(ast, tg) = check_result(run_alr(p, srcs, types, std::move(ast), std::move(tg)));

  BOOST_REQUIRE_EQUAL(exp.size(), ast.types.size() - initial_ast_size);
  for(int i = 0; i < exp.size(); i++) {
    BOOST_CHECK_EQUAL(exp[i], pretty_print(srcs, tg, types.names, ast.types[i + initial_ast_size]));
  }
}

template <typename Parser>
void test_alr_error(Parser p, std::string_view src, std::vector<ContextualError> exp) {
  auto [types, env_src, ast, tg] = basic_test_env();
  const auto srcs = make_sv_array(env_src, src);

  check_eq("error", exp, check_error(run_alr(p, srcs, types, std::move(ast), std::move(tg))));
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
  test_alr(parse_expr, "x", {"_"});
  test_alr(parse_expr, "1", {"i32"});
  test_alr(parse_expr, "&x", {"_", "&_"});
  test_alr(parse_expr, "()", {"()"});
  test_alr(parse_expr, "(1)", {"i32", "(_)"});
  test_alr(parse_expr, "f()", {"fn _ -> _", "()", "_"});

  test_alr(parse_expr, "select x { y } else { z }", {"bool", "_", "_", "_"});

  test_alr_error(parse_expr, "1()", {{{SrcID{1}, {0, 1}}, "expected fn _ -> _, given i32"}});
}

BOOST_AUTO_TEST_CASE(assign) {
  test_alr(parse_assignment, "let x = y", {"_", "_", "()"});
  test_alr(parse_assignment, "let x: _ = y", {"_", "_", "()"});
  test_alr(parse_assignment, "let x: i32 = y", {"i32", "_", "()"});
  test_alr(parse_assignment, "let (x): (i32) = y", {"_", "(i32)", "_", "()"});

  test_alr_error(parse_assignment, "let (): (i32) = y", {{{SrcID{1}, {4, 6}}, "expected (), given (i32)"}});
  test_alr_error(parse_assignment, "let (x): () = y", {{{SrcID{1}, {4, 7}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(tc)

BOOST_AUTO_TEST_CASE(empty) { test_tc_fns({}, "", ""); }

BOOST_AUTO_TEST_CASE(empty_fn) { test_tc({}, "() -> () = ()", "() -> () = ()"); }

BOOST_AUTO_TEST_CASE(return_literal) { test_tc(basic_test_env(), "() -> _ = 5", "() -> i32 = 5"); }

BOOST_AUTO_TEST_CASE(return_binding) { test_tc(basic_test_env(), "(x: i32) -> _ = x", "(x: i32) -> i32 = x"); }

BOOST_AUTO_TEST_CASE(return_tuple) {
  test_tc(basic_test_env(), "(x: i32) -> _ = (x)", "(x: i32) -> (i32) = (x)");
  test_tc(basic_test_env(), "(x: i32) -> (_) = (x)", "(x: i32) -> (i32) = (x)");
}

BOOST_AUTO_TEST_CASE(return_tuple_nested) {
  test_tc(basic_test_env(), "(x: i32) -> _ = (x, (x))", "(x: i32) -> (i32, (i32)) = (x, (x))");
}

BOOST_AUTO_TEST_CASE(param_up) {
  test_tc(basic_test_env(), "(x) -> i32 = x", "(x: i32) -> i32 = x");
  test_tc(basic_test_env(), "(x, y) -> (i32, f32) = (x, y)", "(x: i32, y: f32) -> (i32, f32) = (x, y)");
}

BOOST_AUTO_TEST_CASE(param_down) {
  test_tc(basic_test_env(), "(x: i32) -> _ = x", "(x: i32) -> i32 = x");
  test_tc(basic_test_env(), "(x: i32, y: f32) -> _ = (x, y)", "(x: i32, y: f32) -> (i32, f32) = (x, y)");
}

BOOST_AUTO_TEST_CASE(param_across) {
  test_tc(basic_test_env(), "(x) -> (i32, _) = (x, x)", "(x: i32) -> (i32, i32) = (x, x)");
}

BOOST_AUTO_TEST_CASE(through_assignment) {
  test_tc(basic_test_env(), "(x: i32) -> _ { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
  test_tc(basic_test_env(), "(x) -> i32 { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(assignment_hint) {
  test_tc(basic_test_env(), "(x) -> _ { let y : i32 = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(assignment_tuple) {
  test_tc(basic_test_env(), "() -> _ { let x = (1); x }", "() -> (i32) { let x : (i32) = (1); x }");
}

BOOST_AUTO_TEST_CASE(assignment_literal) {
  test_tc(basic_test_env(), "() -> _ { let y = 5; y }", "() -> i32 { let y : i32 = 5; y }");
}

BOOST_AUTO_TEST_CASE(apply_fn) {
  test_tc(basic_test_env(), "(f: fn(i32) -> i32, x) -> _ = f(x)", "(f: fn(i32) -> i32, x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(apply_fn_deduce_arg) {
  test_tc(basic_test_env(), "(f: fn(_) -> i32, x:i32) -> _ = f(x)", "(f: fn(i32) -> i32, x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(apply_fn_deduce_arg_ref) {
  test_tc(basic_test_env(), "(f: fn(_) -> i32, x:i32) -> _ = f(&x)", "(f: fn(&i32) -> i32, x: i32) -> i32 = f(&x)");
}

BOOST_AUTO_TEST_CASE(global_fn) {
  test_tc(create_test_env({}, {{"f", "fn() -> ()"}}), "() -> () = f()", "() -> () = f()");
}

BOOST_AUTO_TEST_CASE(global_fn_deduce) {
  test_tc_fns(basic_test_env(),
              "fn f(x) -> _ = x\n"
              "fn g(x: i32) -> i32 { let g = f; g(x) }",
              "fn f(x) -> _ = x\n"
              "fn g(x: i32) -> i32 { let g: fn(i32) -> i32 = f; g(x) }");
}

BOOST_AUTO_TEST_CASE(parameter_tuple) {
  test_tc(basic_test_env(), "((x): (_)) -> i32 = x", "((x): (i32)) -> i32 = x");
  test_tc(basic_test_env(), "((x)) -> i32 = x", "((x): (i32)) -> i32 = x");

  test_tc(basic_test_env(), "(x) -> (i32) = (x)", "(x: i32) -> (i32) = (x)");
  test_tc(basic_test_env(), "(x, y) -> (i32, (i32)) = (x, (y))", "(x: i32, y: i32) -> (i32, (i32)) = (x, (y))");
}

BOOST_AUTO_TEST_CASE(unpack_tuple_up) {
  test_tc(basic_test_env(),
          "(tuple) -> (i32, i32) { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(unpack_tuple_down) {
  test_tc(basic_test_env(),
          "(tuple: (i32, i32)) -> _ { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(nested_tuple) {
  test_tc(
    basic_test_env(), "(x: i32) -> _ = (x, (x, (x), x))", "(x: i32) -> (i32, (i32, (i32), i32)) = (x, (x, (x), x))");
}

BOOST_AUTO_TEST_CASE(fn_identity) {
  test_tc(basic_test_env(), "(x: fn(i32) -> i32) -> _ = x", "(x: fn(i32) -> i32) -> fn(i32) -> i32 = x");
}

BOOST_AUTO_TEST_CASE(fn_return) {
  test_tc(basic_test_env({{"f", "fn() -> i32"}}), "() -> _ = f", "() -> fn() -> i32 = f");
}

BOOST_AUTO_TEST_CASE(fn_arg) { test_tc(basic_test_env(), "(f) -> i32 = f()", "(f : fn() -> i32) -> i32 = f()"); }

BOOST_AUTO_TEST_CASE(fn_call) { test_tc(basic_test_env(), "(f) -> i32 = f()", "(f: fn() -> i32) -> i32 = f()"); }

BOOST_AUTO_TEST_CASE(fn_select_from_return) {
  test_tc(basic_test_env(),
          "(a, b, c) -> i32 = select a { b } else { c }",
          "(a: bool, b: i32, c: i32) -> i32 = select a { b } else { c }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_arg) {
  test_tc(basic_test_env(),
          "(a, b: i32, c) -> _ = select a { b } else { c }",
          "(a: bool, b: i32, c: i32) -> i32 = select a { b } else { c }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_constant) {
  test_tc(basic_test_env(),
          "(a, b) -> _ = select a { b } else { 1 }",
          "(a: bool, b: i32) -> i32 = select a { b } else { 1 }");
}

BOOST_AUTO_TEST_CASE(fn_select_from_cond) {
  test_tc(basic_test_env(),
          "(a, b) -> _ = select a { a } else { b }",
          "(a: bool, b: bool) -> bool = select a { a } else { b }");
}

BOOST_AUTO_TEST_CASE(fn_assign) {
  test_tc(basic_test_env({{"f", "fn() -> i32"}}),
          "() -> i32 = { let x = f; x() }",
          "() -> i32 = { let x : fn() -> i32 = f; x() }");
}

BOOST_AUTO_TEST_CASE(fn_tuple) {
  test_tc(basic_test_env(),
          "(f) -> i32 = { let (x) = f; x() }",
          "(f : (fn() -> i32)) -> i32 = { let (x) : (fn() -> i32) = f; x() }");
}

BOOST_AUTO_TEST_CASE(fn_recursive) {
  test_tc_fns(basic_test_env(), "fn f(x: i32) -> i32 = f(x)", "fn f(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_multi) {
  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x) -> _ = f(x)\n";
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  test_tc_fns(basic_test_env(), src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_prop_down) {
  const std::string_view src =
    "fn f(x: i32) -> _ = x\n"
    "fn g(x) -> _ = f(x)\n";
  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  test_tc_fns(basic_test_env(), src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_dont_prop_up) {
  const std::string_view src =
    "fn f(x) -> _ = x\n"
    "fn g(x: i32) -> i32 = f(x)\n";
  test_tc_fns(basic_test_env(), src, src);
}

BOOST_AUTO_TEST_CASE(fn_multi_or) {
  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn f(x: f32) -> f32 = x\n"
    "fn g(x: i32) -> _ = f(x)\n"
    "fn g(x: f32) -> _ = f(x)\n";

  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn f(x: f32) -> f32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n"
    "fn g(x: f32) -> f32 = f(x)\n";

  test_tc_fns(basic_test_env(), src, exp);
}

BOOST_AUTO_TEST_CASE(fn_multi_or_global) {
  const std::string_view src =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> _ = f(x)\n"
    "fn g(x: f32) -> _ = f(x)\n";

  const std::string_view exp =
    "fn f(x: i32) -> i32 = x\n"
    "fn g(x: i32) -> i32 = f(x)\n"
    "fn g(x: f32) -> f32 = f(x)\n";

  test_tc_fns(basic_test_env({{"f", "fn(f32) -> f32"}}), src, exp);
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

  test_tc(basic_test_env(), input, output);
}

BOOST_AUTO_TEST_CASE(function_identity) { test_tc(basic_test_env(), "(x) -> _ = x", "(x) -> _ = x"); }

BOOST_AUTO_TEST_CASE(function_call) {
  test_tc(create_test_env({}, {{"f", "fn() -> ()"}}), "() -> _ = f()", "() -> () = f()");
}

BOOST_AUTO_TEST_CASE(function_nested) {
  test_tc(basic_test_env({{"f", "fn(i32) -> i32"}}), "(x) -> _ = f(f(x))", "(x: i32) -> i32 = f(f(x))");
}

BOOST_AUTO_TEST_CASE(function_arg) {
  test_tc(basic_test_env({{"f", "fn(i32) -> ()"}}), "(x) -> () = f(x)", "(x: i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(function_overloaded) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(f32) -> f32"}};

  test_tc(basic_test_env(fns), "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(basic_test_env(fns), "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");

  test_tc(basic_test_env(fns), "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
  test_tc(basic_test_env(fns), "(x) -> f32 = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(prop_single_function) {
  test_tc(basic_test_env({{"f", "fn(i32) -> i32"}}), "(x) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_borrow) {
  const EnvValues fns = {{"f", "fn(&i32) -> ()"}, {"f", "fn(i32) -> ()"}};
  test_tc(basic_test_env(fns), "(x: &_) -> () = f(x)", "(x: &i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_input) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(f32) -> i32"}};
  test_tc(basic_test_env(fns), "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(basic_test_env(fns), "(x: f32) -> _ = f(x)", "(x: f32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_output) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(i32) -> f32"}};
  test_tc(basic_test_env(fns), "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(basic_test_env(fns), "(x) -> f32 = f(x)", "(x: i32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(constant) {
  test_tc(basic_test_env(), "() -> _ = 1", "() -> i32 = 1");
  test_tc(basic_test_env(), "() -> _ = 'abc'", "() -> string = 'abc'");
}

BOOST_AUTO_TEST_CASE(fn_up) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(f32) -> f32"}};

  test_tc(basic_test_env(fns), "(x) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(basic_test_env(fns), "(x) -> f32 = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_down) {
  const EnvValues fns = {{"f", "fn(i32) -> i32"}, {"f", "fn(f32) -> f32"}};

  test_tc(basic_test_env(fns), "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(basic_test_env(fns), "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
}

BOOST_AUTO_TEST_CASE(borrow) {
  test_tc(basic_test_env(), "(x: i32) -> _ { let _ = &x; () }", "(x : i32) -> () { let _ : &i32 = &x; () }");
}

BOOST_AUTO_TEST_CASE(param_borrow) {
  test_tc(basic_test_env({{"f", "fn(&i32) -> ()"}}), "(x) -> () = f(&x)", "(x: i32) -> () = f(&x)");
}

BOOST_AUTO_TEST_CASE(nested_fn_overload) {
  const EnvValues fns = {
    {"f", "fn(i32) -> ()"},
    {"f", "fn(f32) -> ()"},
    {"g", "fn(i32) -> i32"},
    {"h", "fn() -> i32"},
    {"h", "fn() -> f32"}};

  test_tc(basic_test_env(fns), "() -> _ = f(g(h()))", "() -> () = f(g(h()))");
}

BOOST_AUTO_TEST_CASE(invalid_borrow_expr) {
  test_tc_error(basic_test_env(), "() -> _ { let _ = &(1, 1); 1 }", {{{SrcID{1}, {18, 25}}, "cannot borrow a tuple"}});
  test_tc_error(basic_test_env(), "() -> _ { let _ = &&1; 1 }", {{{SrcID{1}, {18, 21}}, "cannot borrow a borrow"}});
  test_tc_error(
    basic_test_env(), "(x : (i32)) -> _ { let _ = &x; 1 }", {{{SrcID{1}, {27, 29}}, "cannot borrow a tuple"}});
}

BOOST_AUTO_TEST_CASE(invalid_borrow_pattern) {
  test_tc_error(basic_test_env(), "(_ : &&i32) -> _ = 1", {{{SrcID{1}, {1, 2}}, "cannot borrow a borrow"}});
}

BOOST_AUTO_TEST_CASE(return_borrow) {
  test_tc_error(basic_test_env(), "() -> _ = &1", {{{SrcID{1}, {10, 12}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(), "() -> _ = (&1)", {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(),
                "() -> _ = (&1, &2)",
                {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"},
                 {{SrcID{1}, {15, 17}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_borrow_root) {
  test_tc_error(parse_expr, basic_test_env(), "&1", {{{SrcID{1}, {0, 2}}, "cannot return a borrowed value"}});
  test_tc_error(parse_expr, basic_test_env(), "(&1)", {{{SrcID{1}, {1, 3}}, "cannot return a borrowed value"}});

  test_tc_error(
    parse_assignment, basic_test_env(), "let x = &1", {{{SrcID{1}, {8, 10}}, "cannot return a borrowed value"}});
  test_tc_error(
    parse_assignment, basic_test_env(), "let (x) = (&1)", {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_borrow_ident) {
  test_tc_error(basic_test_env(), "(x : &i32) -> _ = x", {{{SrcID{1}, {18, 19}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(), "(x : &i32) -> _ = (x)", {{{SrcID{1}, {19, 20}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(),
                "(x : &i32) -> _ = (x, x)",
                {{{SrcID{1}, {19, 20}}, "cannot return a borrowed value"},
                 {{SrcID{1}, {22, 23}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_floating_borrow) {
  test_tc_error(basic_test_env(), "(x) -> _ = &x", {{{SrcID{1}, {11, 13}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(), "(x : &_) -> _ = x", {{{SrcID{1}, {16, 17}}, "cannot return a borrowed value"}});
  test_tc_error(basic_test_env(), "(x) -> _ = (&x)", {{{SrcID{1}, {12, 14}}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(pattern_mismatch) {
  test_tc_error(
    basic_test_env(), "() -> () { let () = (1); () }", {{{SrcID{1}, {15, 17}}, "expected (), given (i32)"}});
  test_tc_error(basic_test_env(), "() -> () { let (x) = (); () }", {{{SrcID{1}, {15, 18}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_type_mismatch) {
  test_tc_error(basic_test_env(), "() -> () = 1", {{{SrcID{1}, {11, 12}}, "expected i32, given ()"}});
  test_tc_error(basic_test_env(), "() -> () = (1)", {{{SrcID{1}, {11, 14}}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_arity_mismatch) {
  test_tc_error(
    basic_test_env(), "() -> (i32, i32) = (1, 1, 1)", {{{SrcID{1}, {19, 28}}, "expected (_, _, _), given (i32, i32)"}});
}

BOOST_AUTO_TEST_CASE(unused_binding) {
  test_tc_error(basic_test_env(),
                "(x: i32) -> _ = 1",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(basic_test_env(),
                "() -> _ { let x = 1; 1 }",
                {{{SrcID{1}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(basic_test_env(),
                "(x: i32) -> _ { let x = 1; x }",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(basic_test_env(),
                "() -> _ { let x = 1; let x = 1; x }",
                {{{SrcID{1}, {14, 15}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_tuple) {
  test_tc_error(basic_test_env(),
                "((x): (i32)) -> _ = 1",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(basic_test_env(),
                "() -> _ { let (x) = (1); 1 }",
                {{{SrcID{1}, {15, 16}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_self_assign) {
  test_tc_error(basic_test_env(),
                "(x: i32) -> _ { let x = x; 1 }",
                {{{SrcID{1}, {20, 21}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_scope) {
  test_tc_error(basic_test_env(),
                "() -> _ { { let x = 1; 1 } }",
                {{{SrcID{1}, {16, 17}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_root_assign) {
  check_result(run_tc(parse_assignment, basic_test_env(), "let x = 1"));
  check_result(run_tc(parse_assignment, basic_test_env(), "let (x, y) = (1, 1)"));
}

BOOST_AUTO_TEST_CASE(unused_binding_ignore) {
  test_tc(basic_test_env(), "(_x: i32) -> i32 = 1", "(_x: i32) -> i32 = 1");
}

BOOST_AUTO_TEST_CASE(binding_reuse) {
  test_tc_error(basic_test_env(), "(x: string) -> _ = (x, x)", {{{SrcID{1}, {1, 2}}, "binding 'x' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_reuse_floating) { test_tc(basic_test_env(), "(x) -> _ = (x, x)", "(x) -> _ = (x, x)"); }

BOOST_AUTO_TEST_CASE(binding_reuse_both) {
  test_tc_error(basic_test_env(),
                "(x: string, y: i32) -> _ { let z = (x, y); (z, z) }",
                {{{SrcID{1}, {31, 32}}, "binding 'z' used 2 times"}});
}

BOOST_AUTO_TEST_CASE(binding_ref_reuse) {
  test_tc(basic_test_env({{"f", "fn(&i32, &i32) -> ()"}}), "(x: &_) -> _ = f(x, x)", "(x: &_) -> _ = f(x, x)");
}

BOOST_AUTO_TEST_CASE(binding_ref_value_reuse) {
  test_tc(basic_test_env({{"f", "fn(&i32, i32) -> ()"}}), "(x: _) -> _ = f(&x, x)", "(x: _) -> _ = f(&x, x)");
}

BOOST_AUTO_TEST_CASE(binding_reuse_copy) {
  test_tc(basic_test_env(), "(x: i32) -> _ = (x, x)", "(x: i32) -> (i32, i32) = (x, x)");
}

BOOST_AUTO_TEST_CASE(function_ident_reuse) {
  test_tc_error(basic_test_env(),
                "(x: i32, x: i32) -> _ = x",
                {{{SrcID{1}, {1, 2}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(basic_test_env(),
                "((x, x): (i32, i32)) -> _ = x",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(basic_test_env(),
                "((x, (x)): (i32, (i32))) -> _ = x",
                {{{SrcID{1}, {2, 3}}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_count) {
  test_tc_error(
    basic_test_env({{"f", "fn(i32) -> i32"}}), "() -> i32 = f()", {{{SrcID{1}, {13, 15}}, "expected (), given (i32)"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: f32) -> i32 = f(x)",
                {{{SrcID{1}, {1, 2}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_count) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: i32) -> i32 { let () = f(x); x }",
                {{{SrcID{1}, {22, 24}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_count) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: i32) -> () = f(x)",
                {{{SrcID{1}, {17, 21}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(multi_overload_match) {
  const EnvValues fns = {{"f", "fn() -> i32"}, {"f", "fn() -> f32"}};

  test_tc_error(basic_test_env(fns),
                "() -> (i32, f32) { let x = f(); (x, x) }",
                {{{SrcID{1}, {23, 24}}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_type) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: f32) -> i32 = f(x)",
                {{{SrcID{1}, {1, 2}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_type) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: i32) -> i32 { let x: f32 = f(x); x }",
                {{{SrcID{1}, {22, 23}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_type) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> i32"}}),
                "(x: i32) -> f32 = f(x)",
                {{{SrcID{1}, {18, 22}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_value_type) {
  test_tc_error(basic_test_env({{"f", "fn(i32) -> ()"}}),
                "(x: &i32) -> () = f(x)",
                {{{SrcID{1}, {1, 2}}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_CASE(empty_tuple_as_arg) {
  test_tc_error(
    basic_test_env({{"f", "fn(i32) -> ()"}}), "() -> () = f(())", {{{SrcID{1}, {13, 15}}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_type) {
  test_tc_error(basic_test_env(), "(x: i32) -> f32 = x", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(
    basic_test_env(), "(x: i32) -> f32 { let y = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(
    basic_test_env(), "(x: i32) -> f32 { let y: i32 = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(
    basic_test_env(), "(x: i32) -> (f32) { let y: i32 = x; (y) }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(
    basic_test_env(), "(x: i32) -> f32 { let y: f32 = x; y }", {{{SrcID{1}, {1, 2}}, "expected i32, given f32"}});
  test_tc_error(basic_test_env(), "() -> f32 = 1", {{{SrcID{1}, {12, 13}}, "expected i32, given f32"}});
  test_tc_error(basic_test_env(), "() -> f32 { let x = 1; x }", {{{SrcID{1}, {16, 17}}, "expected i32, given f32"}});
  test_tc_error(
    basic_test_env(), "() -> f32 { let x: f32 = 1; x }", {{{SrcID{1}, {16, 17}}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(return_wrong_type_tuple_arg) {
  test_tc_error(basic_test_env(), "((x): (i32)) -> f32 = x", {{{SrcID{1}, {2, 3}}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(missized_pattern) {
  test_tc_error(basic_test_env(), "(() : (_)) -> _ = 1", {{{SrcID{1}, {1, 3}}, "expected (), given (_)"}});
  test_tc_error(basic_test_env(), "((x) : ()) -> _ = 1", {{{SrcID{1}, {1, 4}}, "expected (_), given ()"}});
  test_tc_error(basic_test_env(), "((x) : (_, _)) -> _ = 1", {{{SrcID{1}, {1, 4}}, "expected (_), given (_, _)"}});

  test_tc_error(basic_test_env(), "() -> _ { let () = (1); 1 }", {{{SrcID{1}, {14, 16}}, "expected (), given (i32)"}});
  test_tc_error(basic_test_env(), "() -> _ { let (x) = (); 1 }", {{{SrcID{1}, {14, 17}}, "expected (_), given ()"}});
  test_tc_error(
    basic_test_env(), "() -> _ { let (x) = (1, 1); 1 }", {{{SrcID{1}, {14, 17}}, "expected (_), given (i32, i32)"}});

  test_tc_error(
    basic_test_env(), "(x) -> _ { let () : (_) = x; 1 }", {{{SrcID{1}, {15, 17}}, "expected (), given (_)"}});
  test_tc_error(
    basic_test_env(), "(x) -> _ { let (x) : () = x; 1 }", {{{SrcID{1}, {15, 18}}, "expected (_), given ()"}});
  test_tc_error(
    basic_test_env(), "(x) -> _ { let (x) : (_, _) = x; 1 }", {{{SrcID{1}, {15, 18}}, "expected (_), given (_, _)"}});
}

BOOST_AUTO_TEST_CASE(return_copy_ref_arg) {
  test_tc_error(basic_test_env(), "(x: &i32) -> i32 = x", {{{SrcID{1}, {1, 2}}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
