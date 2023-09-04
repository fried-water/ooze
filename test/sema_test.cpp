#include "test.h"

#include "parser.h"
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

void test_nr_error(const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors) {
  const auto result = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(result.error()));
    BOOST_CHECK(expected_errors == result.error());
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

BOOST_AUTO_TEST_CASE(nr_undefined_return) {
  test_nr_error(create_primative_env(), "() -> abc = x", {{{6, 9}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_arg) {
  test_nr_error(create_primative_env(), "(x: abc) -> () = ()", {{{4, 7}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_let) {
  test_nr_error(create_primative_env(), "() -> () { let x : abc = y; x }", {{{19, 22}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(nr_undefined_multi) {
  test_nr_error(create_primative_env(),
                "(x: a) -> b { let x : c = y; x }",
                {{{4, 5}, "undefined type"}, {{10, 11}, "undefined type"}, {{22, 23}, "undefined type"}});
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

} // namespace ooze
