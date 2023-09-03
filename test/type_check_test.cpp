#include "test.h"

#include "parser.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"
#include "user_msg.h"

namespace ooze {

namespace {

constexpr auto clear_refs = [](auto f) {
  knot::preorder(f, [](Slice& ref) { ref = {}; });
  return f;
};

constexpr auto clear_types = [](auto f) {
  knot::preorder(f,
                 Overloaded{[](TypedPattern& p) { p.type = floating_type<TypeID>(); },
                            [](TypedExpr& e) { e.type = floating_type<TypeID>(); }});
  return f;
};

void test_tc(const Env& e, std::string_view f, std::string_view exp, bool debug = false) {
  const auto parsed_function =
    parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); }).map(clear_refs);

  BOOST_REQUIRE(parsed_function.has_value());

  const auto result = type_check(e, parsed_function.value(), {}, debug);

  if(!result.has_value()) {
    fmt::print("{}\n", knot::debug(contextualize(f, result.error())));
    BOOST_REQUIRE(result.has_value());
  }

  // type_check should only modify the types
  BOOST_CHECK(parsed_function.value() != result.value());
  BOOST_CHECK(clear_types(parsed_function.value()) == clear_types(result.value()));

  int f_idx = 0;
  const auto expected =
    parse_function(exp)
      .and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); })
      .and_then([&](TypedFunction f) { return type_check(e, std::move(f)); })
      .map(clear_refs);

  BOOST_REQUIRE(expected.has_value());

  if(expected.value() != result.value()) {
    fmt::print("E {}\n", pretty_print(e, expected.value()));
    fmt::print("A {}\n", pretty_print(e, result.value()));
    BOOST_CHECK(expected.value() == result.value());
  }

  if(type_of(expected.value()) != type_of(result.value())) {
    fmt::print("E {}\n", pretty_print(e, type_of(expected.value())));
    fmt::print("A {}\n", pretty_print(e, type_of(result.value())));
    BOOST_CHECK(type_of(expected.value()) == type_of(result.value()));
  }
}

void test_tc_error(
  const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors, bool debug = false) {
  auto fr = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });
  BOOST_REQUIRE(fr.has_value());

  const auto result = type_check(e, std::move(fr.value()));

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(result.error()));
    BOOST_CHECK(expected_errors == result.error());
  }
}

} // namespace

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
  test_tc(create_primative_env(), "(x) -> (i32, _) { (x, x) }", "(x: i32) -> (i32, i32) { (x, x) }");
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

BOOST_AUTO_TEST_CASE(nexted_tuple) {
  test_tc(create_primative_env(),
          "(x: i32) -> _ = (x, (x, (x), x))",
          "(x: i32) -> (i32, (i32, (i32), i32)) = (x, (x, (x), x))");
}

BOOST_AUTO_TEST_CASE(fn_identity) {
  test_tc(create_primative_env(), "(x: fn(i32) -> i32) -> _ = x", "(x: fn(i32) -> i32) -> fn(i32) -> i32 = x");
}

BOOST_AUTO_TEST_CASE(return_fn) {
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
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });

  test_tc(create_primative_env(),
          "(f) -> i32 = { let (x) = f; x() }",
          "(f : (fn() -> i32)) -> i32 = { let (x) : (fn() -> i32) = f; x() }");
}

BOOST_AUTO_TEST_CASE(scope) {
  constexpr std::string_view input =
    "(a: i32) -> _ {"
    "  let b = {"
    "    let c = a;"
    "    let a = 'abc';"
    "    (a, c)"
    "  };"
    "  (a, b)"
    "}";

  constexpr std::string_view output =
    "(a: i32) -> (i32, (string, i32)) {"
    "  let b : (string, i32) = {"
    "    let c : i32 = a;"
    "    let a : string = 'abc';"
    "    (a, c)"
    "  };"
    "  (a, b)"
    "}";

  test_tc(create_primative_env(), input, output);
}

BOOST_AUTO_TEST_CASE(function_identity) { test_tc(create_primative_env(), "(x) -> _ = x", "(x) -> _ = x"); }

BOOST_AUTO_TEST_CASE(function_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(function_nested) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x: i32) -> i32 = f(f(x))", "(x: i32) -> i32 = f(f(x))");
}

BOOST_AUTO_TEST_CASE(function_scope_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(function_assign) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  test_tc(e, "(x: i32) -> i32 { let x: i32 = f(x); x }", "(x: i32) -> i32 { let x: i32 = f(x); x }");
}

BOOST_AUTO_TEST_CASE(function_param) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});
  test_tc(e, "(x: i32) -> () = f(x)", "(x: i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(function_multi) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });
  test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
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
  test_tc(e, "(x: &i32) -> () = f(x)", "(x: &i32) -> () = f(x)");
}

BOOST_AUTO_TEST_CASE(fn_overload_input) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x: f32) -> i32 = f(x)", "(x: f32) -> i32 = f(x)");
  test_tc(e, "(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))", "(x: i32, y: f32) -> (i32, i32) = (f(x), f(y))");
}

BOOST_AUTO_TEST_CASE(fn_overload_output) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });
  test_tc(e, "(x: i32) -> i32 = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x: i32) -> f32 = f(x)", "(x: i32) -> f32 = f(x)");
  test_tc(e, "(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))", "(x: i32, y: i32) -> (i32, f32) = (f(x), f(y))");
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
  test_tc(e, "(x, y) -> (i32, f32) { (f(x), f(y)) }", "(x: i32, y: f32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(fn_down) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return f32(); });

  test_tc(e, "(x: i32) -> _ = f(x)", "(x: i32) -> i32 = f(x)");
  test_tc(e, "(x: f32) -> _ = f(x)", "(x: f32) -> f32 = f(x)");
  test_tc(e, "(x: i32, y: f32) -> _ = (f(x), f(y))", "(x: i32, y: f32) -> (i32, f32) = (f(x), f(y))");
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

  test_tc(e, "() -> i32 = f(g(h()))", "() -> i32 = f(g(h()))");
}

BOOST_AUTO_TEST_CASE(invalid_borrow_expr) {
  test_tc_error(create_primative_env(), "() -> _ { let _ = &(1, 1); 1 }", {{{18, 25}, "cannot borrow a tuple"}});
  test_tc_error(create_primative_env(), "() -> _ { let _ = &&1; 1 }", {{{18, 21}, "cannot borrow a borrow"}});
  test_tc_error(create_primative_env(), "(x : (i32)) -> _ { let _ = &x; 1 }", {{{27, 29}, "cannot borrow a tuple"}});
}

BOOST_AUTO_TEST_CASE(invalid_borrow_pattern) {
  // TODO update error to highlight type instead of pattern
  test_tc_error(create_primative_env(), "(_ : &&i32) -> _ = 1", {{{1, 2}, "cannot borrow a borrow"}});
}

BOOST_AUTO_TEST_CASE(return_borrow) {
  test_tc_error(create_primative_env(), "() -> _ = &1", {{{10, 12}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "(x : &i32) -> _ = x", {{{18, 19}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "() -> _ = (&1)", {{{11, 13}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(return_floating_borrow) {
  test_tc_error(create_primative_env(), "(x) -> _ = &x", {{{11, 13}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "(x : &_) -> _ = x", {{{16, 17}, "cannot return a borrowed value"}});
  test_tc_error(create_primative_env(), "(x) -> _ = (&x)", {{{12, 14}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(pattern_mismatch) {
  test_tc_error(create_primative_env(), "() -> () { let () = (1); () }", {{{15, 17}, "expected (), given (i32)"}});
  test_tc_error({}, "() -> () { let (x) = (); () }", {{{15, 18}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_type_mismatch) {
  test_tc_error(create_primative_env(), "() -> () = 1", {{{11, 12}, "expected i32, given ()"}});
  test_tc_error(create_primative_env(), "() -> () = (1)", {{{11, 14}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(return_arity_mismatch) {
  test_tc_error(
    create_primative_env(), "() -> (i32, i32) = (1, 1, 1)", {{{19, 28}, "expected (_, _, _), given (i32, i32)"}});
}

BOOST_AUTO_TEST_CASE(unused_binding) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ = 1",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; 1 }",
                {{{14, 15}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = 1; x }",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let x = 1; let x = 1; x }",
                {{{14, 15}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_tuple) {
  test_tc_error(create_primative_env(),
                "((x): (i32)) -> _ = 1",
                {{{2, 3}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_tc_error(create_primative_env(),
                "() -> _ { let (x) = (1); 1 }",
                {{{15, 16}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_self_assign) {
  test_tc_error(create_primative_env(),
                "(x: i32) -> _ { let x = x; 1 }",
                {{{20, 21}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_scope) {
  test_tc_error(create_primative_env(),
                "() -> _ { { let x = 1; 1 } }",
                {{{16, 17}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(unused_binding_ignore) {
  test_tc(create_primative_env(), "(_x: i32) -> i32 = 1", "(_x: i32) -> i32 = 1");
}

BOOST_AUTO_TEST_CASE(function_ident_reuse) {
  test_tc_error(create_primative_env(),
                "(x: i32, x: i32) -> _ = x",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, x): (i32, i32)) -> _ = x",
                {{{2, 3}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_tc_error(create_primative_env(),
                "((x, (x)): (i32, (i32))) -> _ = x",
                {{{2, 3}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "() -> i32 = identity()", {{{20, 22}, "expected (), given (i32)"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: u32) -> i32 = identity(x)", {{{27, 28}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: i32) -> i32 { let () = identity(x); x }", {{{27, 38}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_tc_error(env, "(x: i32) -> () = identity(x)", {{{17, 28}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(multi_overload_match) {
  Env env = create_primative_env();
  env.add_function("f", []() { return i32(); });
  env.add_function("f", []() { return f32(); });

  test_tc_error(env, "() -> (i32, f32) { let x = f(); (x, x) }", {{{23, 24}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(wrong_arg_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(e, "(x: u32) -> i32 = identity(x)", {{{27, 28}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_bind_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(e, "(x: i32) -> i32 { let x: u32 = identity(x); x }", {{{22, 23}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_return_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_tc_error(e, "(x: i32) -> u32 = identity(x)", {{{18, 29}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_value_type) {
  Env e = create_primative_env();
  e.add_function("val", [](i32 x) {});

  test_tc_error(e, "(x: &i32) -> () = val(x)", {{{22, 23}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_CASE(empty_tuple_as_arg) {
  Env e = create_primative_env();
  e.add_function("take", [](i32) {});

  test_tc_error(e, "() -> () = take(())", {{{16, 18}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(wrong_type) {
  Env e = create_primative_env();

  test_tc_error(e, "(x: i32) -> f32 = x", {{{1, 2}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y = x; y }", {{{22, 23}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: i32 = x; y }", {{{22, 23}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> (f32) { let y: i32 = x; (y) }", {{{37, 38}, "expected i32, given f32"}});
  test_tc_error(e, "(x: i32) -> f32 { let y: f32 = x; y }", {{{1, 2}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 = 1", {{{12, 13}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x = 1; x }", {{{16, 17}, "expected i32, given f32"}});
  test_tc_error(e, "() -> f32 { let x: f32 = 1; x }", {{{25, 26}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(return_wrong_type_tuple_arg) {
  test_tc_error(create_primative_env(), "((x): (i32)) -> f32 = x", {{{2, 3}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(missized_pattern) {
  Env e = create_primative_env();

  test_tc_error(e, "(() : (_)) -> _ = 1", {{{1, 3}, "expected (), given (_)"}});
  test_tc_error(e, "((x) : ()) -> _ = 1", {{{1, 4}, "expected (_), given ()"}});
  test_tc_error(e, "((x) : (_, _)) -> _ = 1", {{{1, 4}, "expected (_), given (_, _)"}});

  test_tc_error(e, "() -> _ { let () = (1); 1 }", {{{14, 16}, "expected (), given (i32)"}});
  test_tc_error(e, "() -> _ { let (x) = (); 1 }", {{{14, 17}, "expected (_), given ()"}});
  test_tc_error(e, "() -> _ { let (x) = (1, 1); 1 }", {{{14, 17}, "expected (_), given (i32, i32)"}});

  test_tc_error(e, "(x) -> _ { let () : (_) = x; 1 }", {{{15, 17}, "expected (), given (_)"}});
  test_tc_error(e, "(x) -> _ { let (x) : () = x; 1 }", {{{15, 18}, "expected (_), given ()"}});
  test_tc_error(e, "(x) -> _ { let (x) : (_, _) = x; 1 }", {{{15, 18}, "expected (_), given (_, _)"}});
}

BOOST_AUTO_TEST_CASE(return_copy_ref_arg) {
  test_tc_error(create_primative_env(), "(x: &i32) -> i32 = x", {{{1, 2}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
