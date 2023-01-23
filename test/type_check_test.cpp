#include "pch.h"

#include "parser.h"
#include "type_check.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

namespace {

const TypeID I = anyf::type_id<i32>();
const TypeID F = anyf::type_id<f32>();

template <typename... Ts>
auto errors(Ts... ts) {
  return tl::unexpected{std::vector<ContextualError>{std::move(ts)...}};
}

void test_or(const Env& e, const std::vector<EnvFunctionRef>& overloads, std::string_view f) {
  const auto function_result =
    parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });
  BOOST_REQUIRE(function_result.has_value());

  int f_idx = 0;
  const auto expected =
    knot::map<CheckedFunction>(function_result.value(), [&](const NamedFunction&) { return overloads[f_idx++]; });

  BOOST_CHECK(expected == overload_resolution(e, function_result.value()));
}

void test_or_error(const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors) {
  const auto function_result =
    parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });
  BOOST_REQUIRE(function_result.has_value());

  auto result = overload_resolution(e, function_result.value());

  BOOST_REQUIRE(!result.has_value());

  std::sort(result.error().begin(), result.error().end());

  if(expected_errors != result.error()) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(result.error()));
  }
  BOOST_CHECK(expected_errors == result.error());
}

void test_name_error(const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors) {
  const auto result = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("E {}\n", knot::debug(expected_errors));
    fmt::print("A {}\n", knot::debug(result.error()));
  }
  BOOST_CHECK(expected_errors == result.error());
}

void test_expr_or(const Env& e,
                  TypedHeader expected_header,
                  const std::vector<EnvFunctionRef>& overloads,
                  const std::unordered_map<std::string, TypeID>& bindings,
                  std::string_view expr_or_assign) {
  const auto scope_result = parse_repl(expr_or_assign).map(convert_to_scope).and_then([&](const UnTypedScope& b) {
    return type_name_resolution(e, b);
  });
  BOOST_REQUIRE(scope_result.has_value());

  int f_idx = 0;
  const CheckedFunction expected{
    std::move(expected_header),
    knot::map<CheckedScope>(scope_result.value(), [&](const NamedFunction&) { return overloads[f_idx++]; })};

  BOOST_CHECK(expected == overload_resolution(e, *scope_result, bindings));
}

void test_expr_or_error(const Env& e,
                        std::string_view expr_or_assign,
                        const std::vector<ContextualError>& expected_errors,
                        const std::unordered_map<std::string, TypeID>& bindings = {}) {
  const auto scope_result = parse_repl(expr_or_assign).map(convert_to_scope).and_then([&](const UnTypedScope& b) {
    return type_name_resolution(e, b);
  });
  BOOST_REQUIRE(scope_result.has_value());

  const auto result = overload_resolution(e, scope_result.value(), bindings);

  BOOST_REQUIRE(!result.has_value());

  if(expected_errors != result.error()) {
    fmt::print("{}\n", knot::debug(result.error()));
  }
  BOOST_CHECK(expected_errors == result.error());
}

} // namespace

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

BOOST_AUTO_TEST_CASE(cp_constant) {
  Env e = create_primative_env();

  test_or(e, {}, "fn t() -> i32 { 1 }");
  test_or(e, {}, "fn t() -> string { 'abc' }");
  test_or(e, {}, "fn t() -> i32 { let x = 5 x }");
  test_or(e, {}, "fn t() -> i32 { let x: i32 = 5 x }");
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
  e.add_function("ref", [](const i32& x) {});
  e.add_function("f", [](const i32&) { return i32(); });
  e.add_function("g", [](i32) { return i32(); });

  test_or(e, {{"ref", 0}}, "fn f(x: i32) -> () { ref(x) }");

  test_or(
    e, {{"f", 0}, {"g", 0}, {"f", 0}, {"g", 0}}, "fn t(x: i32) -> (i32, i32, i32, i32) { (f(x), g(x), f(x), g(x)) }");
}

BOOST_AUTO_TEST_CASE(cp_param_borrow_return) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) { return i32(); });

  test_or(e, {{"f", 0}, {"f", 0}}, "fn t(x: &i32) -> i32 { f(f(x)) }");
}

BOOST_AUTO_TEST_CASE(cp_multi_bind) {
  Env e = create_primative_env();
  e.add_function("f", []() { return std::tuple(i32(), f32()); });

  test_or(e, {{"f", 0}}, "fn t() -> (i32, f32) { let (x, y) = f() (x, y) }");
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
                {{
                  {15, 24},
                  "use of undeclared function 'missing'",
                }});
}

BOOST_AUTO_TEST_CASE(cp_undeclared_binding) {
  Env env = create_primative_env();
  env.add_function("f", [](i32) {});

  test_or_error(env, "fn f() -> () { f(x) }", {{{17, 18}, "use of undeclared binding 'x'"}});
}

BOOST_AUTO_TEST_CASE(cp_arity_mismatch) {
  Env env = create_primative_env();

  test_or_error(env, "fn f() -> (i32, i32) { (1, 1, 1) }", {{{4, 20}, "function expects 2 return values, given 3"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f() -> i32 { identity() }",
                {{{16, 26},
                  "no matching overload found",
                  {"deduced identity() -> (i32) [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> i32 { let () = identity(x) x }",
                {{{31, 42},
                  "no matching overload found",
                  {"deduced identity(i32) -> () [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> () { identity(x) }",
                {{{21, 32},
                  "no matching overload found",
                  {"deduced identity(i32) -> () [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: u32) -> i32 { identity(x) }",
                {{{22, 33},
                  "no matching overload found",
                  {"deduced identity(u32) -> (i32) [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> i32 { let x: u32 = identity(x) x }",
                {{{35, 46},
                  "no matching overload found",
                  {"deduced identity(i32) -> (u32) [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_type) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "fn f(x: i32) -> u32 { identity(x) }",
                {{{22, 33},
                  "no matching overload found",
                  {"deduced identity(i32) -> (u32) [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_value_type) {
  Env env = create_primative_env();
  env.add_function("val", [](i32 x) {});

  test_or_error(
    env,
    "fn f(x: &i32) -> () { val(x) }",
    {{{22, 28}, "no matching overload found", {"deduced val(&i32) -> () [1 candidate(s)]", "  val(i32) -> ()"}}});
}

BOOST_AUTO_TEST_CASE(cp_empty_tuple_as_arg) {
  Env env = create_primative_env();
  env.add_function("tup", []() { return std::tuple(); });
  env.add_function("take", [](i32) {});

  test_or_error(
    env, "fn f() -> () { take(tup()) }", {{{20, 25}, "call to function take takes an expr that returns a tuple"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_type_no_functions) {
  Env env = create_primative_env();

  test_or_error(env, "fn f(x: i32) -> f32 { x }", {{{22, 23}, "expected i32, found f32"}});
  test_or_error(env,
                "fn f(x: i32) -> f32 { let y = x y }",
                {{{30, 31}, "expected i32, found f32"}, {{32, 33}, "expected f32, found i32"}});
  test_or_error(env,
                "fn f(x: i32) -> f32 { let y: i32 = x y }",
                {{{35, 36}, "expected i32, found f32"}, {{37, 38}, "expected f32, found i32"}});
  test_or_error(env,
                "fn f(x: i32) -> f32 { let y: f32 = x y }",
                {{{35, 36}, "expected i32, found f32"}, {{37, 38}, "expected f32, found i32"}});
  test_or_error(env, "fn f() -> f32 { 1 }", {{{16, 17}, "expected f32, found i32"}});
  test_or_error(env,
                "fn f() -> f32 { let x = 1 x }",
                {{{24, 25}, "expected i32, found f32"}, {{26, 27}, "expected f32, found i32"}});
  test_or_error(env, "fn f() -> f32 { let x: f32 = 1 x }", {{{29, 30}, "expected f32, found i32"}});
}

BOOST_AUTO_TEST_CASE(cp_return_copy_ref_arg) {
  test_or_error(
    create_primative_env(), "fn f(x: &i32) -> i32 { x }", {{{23, 24}, "attempting to return borrowed parameter 'x'"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_return) {
  test_name_error(create_primative_env(), "fn f() -> abc { x }", {{{10, 13}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_arg) {
  test_name_error(create_primative_env(), "fn f(x: abc) -> () { () }", {{{8, 11}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_let) {
  test_name_error(create_primative_env(), "fn f() -> () { let x : abc = y x }", {{{23, 26}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_multi) {
  test_name_error(create_primative_env(),
                  "fn f(x: a) -> b { let x : c = y x }",
                  {{{8, 9}, "undefined type"}, {{14, 15}, "undefined type"}, {{26, 27}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_simple) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  test_expr_or(e, {{{"x", I}}, {I}}, {{"f", 0}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_extra_binding) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  test_expr_or(e, {{{"x", I}}, {I}}, {{"f", 0}}, {{"x", I}, {"y", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_no_args) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  test_expr_or(e, {{}, {I}}, {{"f", 0}}, {}, "f()");
}

BOOST_AUTO_TEST_CASE(cp_expr_unnamed_type) {
  struct A {};

  const auto a_type = anyf::type_id<A>();

  Env e = create_primative_env();
  e.add_function("f", [](A) {});
  test_expr_or(e, {{{"x", a_type}}}, {{"f", 0}}, {{"x", a_type}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_return) {
  Env e = create_primative_env();
  e.add_function("f", []() { return i32(); });
  e.add_function("f", []() { return f32(); });
  test_expr_or(e, {{}, {I}}, {{"f", 0}}, {}, "let x: i32 = f()");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_binding) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_expr_or(e, {{{"x", I}}, {I}}, {{"f", 0}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_deduced_binding_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });
  test_expr_or(e, {{{"x", I, true}}, {I}}, {{"f", 0}}, {{"x", I}}, "f(x)");
}

BOOST_AUTO_TEST_CASE(cp_expr_undeclared_function) {
  Env env = create_primative_env();

  test_expr_or_error(env, "missing()", {{{0, 9}, "use of undeclared function 'missing'"}});
  test_expr_or_error(env, "let x = missing()", {{{8, 17}, "use of undeclared function 'missing'"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undeclared_binding) {
  Env env = create_primative_env();
  env.add_function("f", [](i32) {});

  test_expr_or_error(env, "f(x)", {{{2, 3}, "use of undeclared binding 'x'"}});
  test_expr_or_error(env, "let y = f(x)", {{{10, 11}, "use of undeclared binding 'x'"}});
}

} // namespace ooze
