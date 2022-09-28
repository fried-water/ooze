#include "pch.h"

#include "overload_resolution.h"
#include "parser.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

namespace {

const TypeID I = anyf::type_id<i32>();
const TypeID F = anyf::type_id<f32>();

template <typename... Ts>
auto errors(Ts... ts) {
  return tl::unexpected{std::vector<std::string>{std::move(ts)...}};
}

void test_or(const Env& e, const std::vector<EnvFunctionRef>& overloads, std::string_view fstr) {
  const auto function_result = parse_function(fstr);
  BOOST_REQUIRE(function_result.has_value());

  const auto& f = function_result.value();

  int f_idx = 0;
  const auto expected = knot::map<TypedFunction>(std::tie(f.parameters, f.assignments, f.ret),
                                                 Overloaded{[&](const NamedFunction&) { return overloads[f_idx++]; },
                                                            [&](const NamedType& t) { return e.type_ids.at(t.name); }});

  BOOST_CHECK(expected == overload_resolution(e, function_result.value()));
}

void test_or_error(const Env& e, std::string_view f, const std::vector<std::string>& expected_errors) {
  const auto function_result = parse_function(f);
  BOOST_REQUIRE(function_result.has_value());

  const auto result = overload_resolution(e, function_result.value());

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("{}", knot::debug(result.error()));
  }
  BOOST_REQUIRE_EQUAL(expected_errors.size(), result.error().size());

  for(size_t i = 0; i < result.error().size(); i++) {
    BOOST_CHECK_EQUAL(expected_errors[i], result.error()[i]);
  }
}

void test_expr_or(const Env& e,
                  const std::vector<EnvFunctionRef>& overloads,
                  const std::vector<ast::Parameter<TypeID>>& parameters,
                  const std::unordered_map<std::string, TypeID>& bindings,
                  std::string_view expr_or_assign) {
  const auto expr_result = parse_repl(expr_or_assign);
  BOOST_REQUIRE(expr_result.has_value());

  int f_idx = 0;
  const auto expected_repl = knot::map<std::variant<TypedExpr, TypedAssignment>>(
    expr_result.value(),
    Overloaded{[&](const NamedFunction&) { return overloads[f_idx++]; },
               [&](const NamedType& t) { return e.type_ids.at(t.name); }});

  const TypedFunction expected = std::visit(Overloaded{[&](const TypedExpr& e) {
                                                         return TypedFunction{parameters, {}, {e}};
                                                       },
                                                       [&](const TypedAssignment& a) {
                                                         auto f = TypedFunction{parameters, {a}};
                                                         for(const auto& binding : a.bindings) {
                                                           f.ret.push_back(TypedExpr{binding.name});
                                                         }
                                                         return f;
                                                       }},
                                            expected_repl);

  BOOST_CHECK(expected == overload_resolution(e, *expr_result, bindings));
}

void test_expr_or_error(const Env& e,
                        std::string_view expr,
                        const std::vector<std::string>& expected_errors,
                        const std::unordered_map<std::string, TypeID>& bindings = {}) {
  const auto expr_result = parse_repl(expr);
  BOOST_REQUIRE(expr_result.has_value());

  const auto result = overload_resolution(e, expr_result.value(), bindings);

  BOOST_REQUIRE(!result.has_value());
  BOOST_REQUIRE_EQUAL(expected_errors.size(), result.error().size());

  for(size_t i = 0; i < result.error().size(); i++) {
    BOOST_CHECK_EQUAL(expected_errors[i], result.error()[i]);
  }
}

} // namespace

BOOST_AUTO_TEST_CASE(overload_resolution_basic) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x; });

  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}, {F, true}}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_undeclared) {
  Env e = create_primative_env();
  BOOST_CHECK(err("use of undeclared function 'f'") == overload_resolution(e, "f", {{I}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_match_return_type) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x) { return std::tuple(x, x); });

  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}, Span<TypeID>{I}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}}, Span<TypeID>{I, I}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_missing_args) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x + y; });

  const auto expected_errors1 = errors("no matching overload found for f() [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for g(i32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  const auto expected_errors3 =
    errors("no matching overload found for g(f32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "g", {{I, true}}));
  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{F, true}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_wrong_args) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x + y; });

  const auto expected_errors1 = errors("no matching overload found for f(f32) [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for g(i32, i32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  const auto expected_errors3 =
    errors("no matching overload found for g(f32, f32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {{F, true}}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "g", {{I, true}, {I, true}}));
  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{F, true}, {F, true}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_wrong_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x) { return std::tuple(x, x); });

  const auto expected_errors1 =
    errors("no matching overload found for f(i32) -> () [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for f(i32) -> (i32, i32) [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors3 =
    errors("no matching overload found for g(i32) -> () [1 candidate(s)]", "  g(i32) -> (i32, i32)");

  const auto expected_errors4 =
    errors("no matching overload found for g(i32) -> i32 [1 candidate(s)]", "  g(i32) -> (i32, i32)");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {{I, true}}, Span<TypeID>{}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "f", {{I, true}}, Span<TypeID>{I, I}));

  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{I, true}}, Span<TypeID>{}));
  BOOST_CHECK(expected_errors4 == overload_resolution(e, "g", {{I, true}}, Span<TypeID>{I}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](const i32& x) { return x; });

  const auto expected_errors = errors("no matching overload found for f(i32&) [1 candidate(s)]", "  f(i32) -> i32");

  BOOST_CHECK(expected_errors == overload_resolution(e, "f", {{I}}));
  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}).has_value());

  BOOST_CHECK(overload_resolution(e, "g", {{I}}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_ambiguous) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](const i32& x) { return x; });

  const auto expected_errors =
    errors("function call is ambiguous f(i32) [2 candidate(s)]", "  f(i32) -> i32", "  f(i32&) -> i32");

  BOOST_CHECK(expected_errors == overload_resolution(e, "f", {{I, true}}));
  BOOST_CHECK(overload_resolution(e, "f", {{I}}).has_value());
}

BOOST_AUTO_TEST_CASE(cp_basic) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });

  test_or(e, {{"f", 0}}, "fn t(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1}}, "fn t(x: f32) -> f32 { f(x) }");
  test_or(e, {{"f", 0}, {"f", 1}}, "fn t(x: i32, y: f32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_empty) { test_or(create_primative_env(), {}, "fn t() -> () { () }"); }

BOOST_AUTO_TEST_CASE(cp_unused_input) { test_or(create_primative_env(), {}, "fn t(x: i32) -> () { () }"); }

BOOST_AUTO_TEST_CASE(cp_unused_binding) { test_or(create_primative_env(), {}, "fn t() -> () { let x = 5 () }"); }

BOOST_AUTO_TEST_CASE(cp_literal_return) { test_or(create_primative_env(), {}, "fn t() -> i32 { 5 }"); }

BOOST_AUTO_TEST_CASE(cp_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) {});
  e.add_function("f", [](i32) {});

  test_or(e, {{"f", 0}}, "fn t(x: &i32) -> () { f(x) }");
}

BOOST_AUTO_TEST_CASE(cp_input) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  test_or(e, {{"f", 0}}, "fn t(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1}}, "fn t(x: f32) -> i32 { f(x) }");
  test_or(e, {{"f", 0}, {"f", 1}}, "fn t(x: i32, y: f32) -> (i32, i32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_output) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });

  test_or(e, {{"f", 0}}, "fn t(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1}}, "fn t(x: i32) -> f32 { f(x) }");
  test_or(e, {{"f", 0}, {"f", 1}}, "fn t(x: i32, y: i32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_binding_up) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  e.add_function("f", []() { return f32(); });

  test_or(e, {{"f", 0}}, "fn t() -> i32 { let x = f() x }");
  test_or(e, {{"f", 1}}, "fn t() -> f32 { let x = f() x }");
  test_or(e, {{"f", 0}, {"f", 1}}, "fn t() -> (i32, f32) { let x = f() let y = f() (x, y) }");
}

BOOST_AUTO_TEST_CASE(cp_binding_across) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  e.add_function("f", []() { return f32(); });

  e.add_function("g", [](i32) { return i32(); });
  e.add_function("g", [](f32) { return i32(); });

  test_or(e, {{"f", 0}, {"g", 0}}, "fn t() -> (i32, i32) { let x = f() (x, g(x)) }");
}

BOOST_AUTO_TEST_CASE(cp_binding_hint) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  e.add_function("f", []() { return f32(); });

  e.add_function("g", [](i32) { return i32(); });
  e.add_function("g", [](f32) { return i32(); });

  test_or(e, {{"f", 0}, {"g", 0}}, "fn t() -> i32 { let x: i32 = f() g(x) }");
}

BOOST_AUTO_TEST_CASE(cp_param_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) { return i32(); });
  e.add_function("g", [](i32) { return i32(); });

  test_or(
    e, {{"f", 0}, {"g", 0}, {"f", 0}, {"g", 0}}, "fn t(x: i32) -> (i32, i32, i32, i32) { (f(x), g(x), f(x), g(x)) }");
}

BOOST_AUTO_TEST_CASE(cp_param_borrow_return) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) { return i32(); });

  test_or(e, {{"f", 0}, {"f", 0}}, "fn t(x: &i32) -> i32 { f(f(x)) }");
}

BOOST_AUTO_TEST_CASE(cp_single_overload) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  e.add_function("g", [](i32) { return i32(); });

  e.add_function("h", []() { return i32(); });
  e.add_function("h", []() { return f32(); });

  test_or(e, {{"f", 0}, {"g", 0}, {"h", 0}}, "fn t() -> i32 { f(g(h())) }");
}

BOOST_AUTO_TEST_CASE(cp_undeclared_function) {
  Env env = create_primative_env();

  test_or_error(env,
                "fn f() -> () { missing() }",
                {
                  "use of undeclared function 'missing'",
                });
}

BOOST_AUTO_TEST_CASE(cp_undeclared_binding) {
  Env env = create_primative_env();
  env.add_function("f", [](i32) {});

  test_or_error(env,
                "fn f() -> () { f(x) }",
                {
                  "use of undeclared binding 'x'",
                });
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f() -> i32 { identity() }",
                {"no matching overload found, deduced identity() -> (i32) [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> i32 { let () = identity(x) x }",
                {"no matching overload found, deduced identity(i32) -> () [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> () { identity(x) }",
                {"no matching overload found, deduced identity(i32) -> () [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(
    env,
    "fn f(x: u32) -> i32 { identity(x) }",
    {"no matching overload found, deduced identity(u32) -> (i32) [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(
    env,
    "fn f(x: i32) -> i32 { let x: u32 = identity(x) x }",
    {"no matching overload found, deduced identity(i32) -> (u32) [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(
    env,
    "fn f(x: i32) -> u32 { identity(x) }",
    {"no matching overload found, deduced identity(i32) -> (u32) [1 candidate(s)]", "  identity(i32) -> i32"});
}

BOOST_AUTO_TEST_CASE(cp_wrong_value_type) {
  Env env = create_primative_env();
  env.add_function("ref", [](const i32& x) {});
  env.add_function("val", [](i32 x) {});

  test_or_error(env, "fn f(x: i32) -> () { ref(x) }", {"parameter 'x' only borrowed but taken by value"});
  test_or_error(env,
                "fn f(x: &i32) -> () { val(x) }",
                {"no matching overload found, deduced val(i32&) -> () [1 candidate(s)]", "  val(i32) -> ()"});
}

BOOST_AUTO_TEST_CASE(cp_expr_simple) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  test_expr_or(e, {{"f", 0}}, {{"x", I}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_extra_binding) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  test_expr_or(e, {{"f", 0}}, {{"x", I}}, {{"x", I}, {"y", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_no_args) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  test_expr_or(e, {{"f", 0}}, {}, {}, "f()");
}

BOOST_AUTO_TEST_CASE(cp_expr_unnamed_type) {
  struct A {};

  Env e = create_primative_env();
  e.add_function("f", [](A) {});
  test_expr_or(e, {{"f", 0}}, {{"x", anyf::type_id<A>()}}, {{"x", anyf::type_id<A>()}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_return) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  e.add_function("f", []() { return f32(); });
  test_expr_or(e, {{"f", 0}}, {}, {}, "let x: i32 = f()");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_binding) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_expr_or(e, {{"f", 0}}, {{"x", I}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_binding_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_expr_or(e, {{"f", 0}}, {{"x", I, true}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_undeclared_function) {
  Env env = create_primative_env();

  test_expr_or_error(env, "missing()", {"use of undeclared function 'missing'"});
  test_expr_or_error(env, "let x = missing()", {"use of undeclared function 'missing'"});
}

BOOST_AUTO_TEST_CASE(cp_expr_undeclared_binding) {
  Env env = create_primative_env();
  env.add_function("f", [](i32) {});

  test_expr_or_error(env, "f(x)", {"use of undeclared binding 'x'"});
  test_expr_or_error(env, "let y = f(x)", {"use of undeclared binding 'x'"});
}

} // namespace ooze
