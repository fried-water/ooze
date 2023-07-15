#include "test.h"

#include "parser.h"
#include "pretty_print.h"
#include "type_check.h"
#include "user_msg.h"

namespace ooze {

namespace {

const CompoundType<TypeID> I = leaf_type<TypeID>(anyf::type_id<i32>());
const CompoundType<TypeID> F = leaf_type<TypeID>(anyf::type_id<f32>());

template <typename... Ts>
auto errors(Ts... ts) {
  return tl::unexpected{std::vector<ContextualError>{std::move(ts)...}};
}

constexpr auto clear_refs = [](auto f) {
  knot::preorder(f, [](Slice& ref) { ref = {}; });
  return f;
};

void test_infer_header(const Env& e, std::string_view expr, std::string_view exp_header, std::vector<Slice> exp_refs) {
  const auto expr_result = parse_expr(expr).and_then([&](const auto& s) { return type_name_resolution(e, s); });

  int next_ref = 0;
  const auto header_result =
    parse_header(exp_header)
      .and_then([&](const auto& h) { return type_name_resolution(e, h); })
      .map([&](TypedHeader h) {
        knot::preorder(h, [](Slice& ref) { ref = {}; });
        knot::preorder(h, [&](ast::Pattern& p) {
          knot::visit(p.v, [&](const ast::Ident&) {
            BOOST_REQUIRE(next_ref < exp_refs.size());
            p.ref = exp_refs[next_ref++];
          });
        });

        return h;
      });

  BOOST_REQUIRE(header_result.has_value());

  if(!expr_result.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(expr, expr_result.error())));
    BOOST_REQUIRE(false);
  }

  const auto actual = inferred_header(expr_result.value());

  if(actual != header_result.value()) {
    fmt::print("E {}\n", knot::debug(header_result.value()));
    fmt::print("A {}\n", knot::debug(actual));
    BOOST_CHECK(false);
  }
}

void test_or(const Env& e,
             const std::vector<EnvFunctionRef>& overloads,
             std::string_view f,
             std::optional<std::string_view> exp = {},
             bool debug = false) {
  const auto fr = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });

  if(!fr.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(f, fr.error())));
    BOOST_REQUIRE(fr.has_value());
  }

  int f_idx = 0;
  const auto expected =
    parse_function(exp ? *exp : f)
      .and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); })
      .map([&](TypedFunction f) {
        knot::preorder(f, [](Slice& ref) { ref = {}; });
        return knot::map<CheckedFunction>(std::move(f), [&](const NamedFunction&) { return overloads[f_idx++]; });
      });

  BOOST_REQUIRE(expected.has_value());

  const auto result = overload_resolution_concrete(e, std::move(fr.value()), {}, debug).map(clear_refs);

  if(!result.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(f, result.error())));
  }
  BOOST_REQUIRE(result.has_value());

  if(expected.value() != result.value()) {
    fmt::print("E {}\n", pretty_print(untype<UnTypedFunction>(e, expected.value())));
    fmt::print("A {}\n", pretty_print(untype<UnTypedFunction>(e, result.value())));
  }

  BOOST_CHECK(expected.value() == result.value());
}

void test_or_unresolved(
  const Env& e, std::string_view f, std::optional<std::string_view> exp = {}, bool debug = false) {
  const auto fr = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });

  if(!fr.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(f, fr.error())));
    BOOST_REQUIRE(fr.has_value());
  }

  int f_idx = 0;
  const auto expected =
    parse_function(exp ? *exp : f)
      .and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); })
      .map(clear_refs);

  BOOST_REQUIRE(expected.has_value());

  const auto result =
    overload_resolution(e, std::move(fr.value()), {}, debug)
      .map([&](auto variant) {
        BOOST_REQUIRE(std::holds_alternative<TypedFunction>(variant));
        return std::get<TypedFunction>(std::move(variant));
      })
      .map(clear_refs);

  if(!result.has_value()) {
    fmt::print("Parse error: {}", knot::debug(contextualize(f, result.error())));
  }
  BOOST_REQUIRE(result.has_value());

  if(expected.value() != result.value()) {
    fmt::print("E {}\n", pretty_print(untype<UnTypedFunction>(e, expected.value())));
    fmt::print("A {}\n", pretty_print(untype<UnTypedFunction>(e, result.value())));
  }

  BOOST_CHECK(expected.value() == result.value());
}

void test_or_error(
  const Env& e, std::string_view f, const std::vector<ContextualError>& expected_errors, bool debug = false) {
  const auto fr = parse_function(f).and_then([&](const UnTypedFunction& f) { return type_name_resolution(e, f); });
  BOOST_REQUIRE(fr.has_value());

  const auto result = overload_resolution_concrete(e, fr.value(), {}, debug);

  BOOST_REQUIRE(!result.has_value());

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

FunctionType<TypeID> sino(CompoundType<TypeID> input) { return {tuple_type<TypeID>({input}), tuple_type<TypeID>({})}; }

FunctionType<TypeID> niso(CompoundType<TypeID> output) { return {tuple_type<TypeID>({}), std::move(output)}; }

FunctionType<TypeID> siso(CompoundType<TypeID> input, CompoundType<TypeID> output) {
  return {tuple_type<TypeID>({std::move(input)}), std::move(output)};
}

} // namespace

BOOST_AUTO_TEST_CASE(infer_empty) { test_infer_header(Env{}, "()", "() -> _", {{}}); }

BOOST_AUTO_TEST_CASE(infer_return_int) { test_infer_header(Env{}, "0", "() -> _", {}); }

BOOST_AUTO_TEST_CASE(infer_single_arg) { test_infer_header(create_primative_env(), "a", "(a) -> _", {{0, 1}}); }

BOOST_AUTO_TEST_CASE(infer_multi_arg) {
  test_infer_header(
    create_primative_env(), "{ (a, ((b, c), d)) }", "(a, b, c, d) -> _", {{3, 4}, {8, 9}, {11, 12}, {15, 16}});
}

BOOST_AUTO_TEST_CASE(infer_with_let) {
  test_infer_header(create_primative_env(), "{ let x = a; x }", "(a) -> _", {{10, 11}});
}

BOOST_AUTO_TEST_CASE(infer_multi_let) {
  test_infer_header(create_primative_env(), "{ let x = a; let y = x; y }", "(a) -> _", {{10, 11}});
}

BOOST_AUTO_TEST_CASE(cp_empty) { test_or(create_primative_env(), {}, "() -> () { () }", "() -> () { () }"); }

BOOST_AUTO_TEST_CASE(cp_return_literal) { test_or(create_primative_env(), {}, "() -> _ { 5 }", "() -> i32 { 5 }"); }

BOOST_AUTO_TEST_CASE(cp_return_binding) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { x }", "(x: i32) -> i32 { x }");
}

BOOST_AUTO_TEST_CASE(cp_return_tuple) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { (x) }", "(x: i32) -> (i32) { (x) }");
  test_or(create_primative_env(), {}, "(x: i32) -> (_) { (x) }", "(x: i32) -> (i32) { (x) }");
}

BOOST_AUTO_TEST_CASE(cp_return_tuple_nested) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { (x, (x)) }", "(x: i32) -> (i32, (i32)) { (x, (x)) }");
}

BOOST_AUTO_TEST_CASE(cp_param_up) {
  test_or(create_primative_env(), {}, "(x) -> i32 { x }", "(x: i32) -> i32 { x }");
  test_or(create_primative_env(), {}, "(x, y) -> (i32, f32) { (x, y) }", "(x: i32, y: f32) -> (i32, f32) { (x, y) }");
}

BOOST_AUTO_TEST_CASE(cp_param_down) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { x }", "(x: i32) -> i32 { x}");
  test_or(create_primative_env(), {}, "(x: i32, y: f32) -> _ { (x, y) }", "(x: i32, y: f32) -> (i32, f32) { (x, y) }");
}

BOOST_AUTO_TEST_CASE(cp_param_across) {
  test_or(create_primative_env(), {}, "(x) -> (i32, _) { (x, x) }", "(x: i32) -> (i32, i32) { (x, x) }");
}

BOOST_AUTO_TEST_CASE(cp_through_assignment) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
  test_or(create_primative_env(), {}, "(x) -> i32 { let y = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(cp_assignment_hint) {
  test_or(create_primative_env(), {}, "(x) -> _ { let y : i32 = x; y }", "(x: i32) -> i32 { let y : i32 = x; y }");
}

BOOST_AUTO_TEST_CASE(cp_assignment_tuple) {
  test_or(create_primative_env(), {}, "() -> _ { let x = (1); x }", "() -> (i32) { let x : (i32) = (1); x }");
}

BOOST_AUTO_TEST_CASE(cp_assignment_literal) {
  test_or(create_primative_env(), {}, "() -> _ { let y = 5; y }", "() -> i32 { let y : i32 = 5; y }");
}

BOOST_AUTO_TEST_CASE(cp_parameter_tuple) {
  test_or(create_primative_env(), {}, "((x): (_)) -> i32 { x }", "((x): (i32)) -> i32 { x }");
  test_or(create_primative_env(), {}, "((x)) -> i32 { x }", "((x): (i32)) -> i32 { x }");

  test_or(create_primative_env(), {}, "(x) -> (i32) { (x) }", "(x: i32) -> (i32) { (x) }");
  test_or(
    create_primative_env(), {}, "(x, y) -> (i32, (i32)) { (x, (y)) }", "(x: i32, y: i32) -> (i32, (i32)) { (x, (y)) }");
}

BOOST_AUTO_TEST_CASE(cp_unpack_tuple_up) {
  test_or(create_primative_env(),
          {},
          "(tuple) -> (i32, i32) { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(cp_unpack_tuple_down) {
  test_or(create_primative_env(),
          {},
          "(tuple: (i32, i32)) -> _ { let (x, y) = tuple; (x, y) }",
          "(tuple: (i32, i32)) -> (i32, i32) { let (x, y) : (i32, i32) = tuple; (x, y) }");
}

BOOST_AUTO_TEST_CASE(cp_scope) {
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

  test_or(create_primative_env(), {}, input, output);
}

BOOST_AUTO_TEST_CASE(cp_function_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> i32 = f(x)");
}

BOOST_AUTO_TEST_CASE(cp_function_assign) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> i32 { let x: i32 = f(x); x }");
}

BOOST_AUTO_TEST_CASE(cp_function_multi) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](f32 x) { return x; });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> i32 { f(x) }");
}

BOOST_AUTO_TEST_CASE(cp_prop_single_function) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x) -> _ { f(x) }", "(x: i32) -> i32 { f(x) }");
}

BOOST_AUTO_TEST_CASE(cp_fn_param) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});
  test_or(e, {{"f", 0, sino(I)}}, "(x: i32) -> () { f(x) }", "(x: i32) -> () { f(x) }");
}

BOOST_AUTO_TEST_CASE(cp_fn_overload_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](const i32&) {});
  e.add_function("f", [](i32) {});

  test_or(e, {{"f", 0, sino(borrow_type(I))}}, "(x: &i32) -> () { f(x) }", "(x: &i32) -> () { f(x) }");
}

BOOST_AUTO_TEST_CASE(cp_fn_overload_input) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1, siso(F, I)}}, "(x: f32) -> i32 { f(x) }");
  test_or(e, {{"f", 0, siso(I, I)}, {"f", 1, siso(F, I)}}, "(x: i32, y: f32) -> (i32, i32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_fn_overload_output) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](i32) { return f32(); });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1, siso(I, F)}}, "(x: i32) -> f32 { f(x) }");
  test_or(e, {{"f", 0, siso(I, I)}, {"f", 1, siso(I, F)}}, "(x: i32, y: i32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_constant) {
  test_or(create_primative_env(), {}, "() -> _ { 1 }", "() -> i32 { 1 }");
  test_or(create_primative_env(), {}, "() -> _ { 'abc' }", "() -> string { 'abc' }");
}

BOOST_AUTO_TEST_CASE(cp_fn_up) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return f32(); });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x) -> i32 { f(x) }", "(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1, siso(F, F)}}, "(x) -> f32 { f(x) }", "(x: f32) -> f32 { f(x) }");
  test_or(e,
          {{"f", 0, siso(I, I)}, {"f", 1, siso(F, F)}},
          "(x, y) -> (i32, f32) { (f(x), f(y)) }",
          "(x: i32, y: f32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_fn_down) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return f32(); });

  test_or(e, {{"f", 0, siso(I, I)}}, "(x: i32) -> _ { f(x) }", "(x: i32) -> i32 { f(x) }");
  test_or(e, {{"f", 1, siso(F, F)}}, "(x: f32) -> _ { f(x) }", "(x: f32) -> f32 { f(x) }");
  test_or(e,
          {{"f", 0, siso(I, I)}, {"f", 1, siso(F, F)}},
          "(x: i32, y: f32) -> _ { (f(x), f(y)) }",
          "(x: i32, y: f32) -> (i32, f32) { (f(x), f(y)) }");
}

BOOST_AUTO_TEST_CASE(cp_borrow) {
  test_or(create_primative_env(), {}, "(x: i32) -> _ { let _ = &x; () }", "(x : i32) -> () { let _ : &i32 = &x; () }");
}

BOOST_AUTO_TEST_CASE(cp_param_borrow) {
  Env e = create_primative_env();
  e.add_function("ref", [](const i32& x) {});
  test_or(e, {{"ref", 0, sino(borrow_type(I))}}, "(x) -> () { ref(&x) }", "(x: i32) -> () { ref(&x) }");
}

BOOST_AUTO_TEST_CASE(cp_nested_fn_overload) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) { return i32(); });
  e.add_function("f", [](f32) { return i32(); });

  e.add_function("g", [](i32) { return i32(); });

  e.add_function("h", []() { return i32(); });
  e.add_function("h", []() { return f32(); });

  test_or(e, {{"f", 0, siso(I, I)}, {"g", 0, siso(I, I)}, {"h", 0, niso(I)}}, "() -> i32 { f(g(h())) }");
}

BOOST_AUTO_TEST_CASE(cp_invalid_borrow_expr) {
  test_or_error(create_primative_env(), "() -> _ { let _ = &(1, 1); 1 }", {{{18, 25}, "cannot borrow a tuple"}});
  test_or_error(create_primative_env(), "() -> _ { let _ = &&1; 1 }", {{{18, 21}, "cannot borrow a borrow"}});
  test_or_error(create_primative_env(), "(x : (i32)) -> _ { let _ = &x; 1 }", {{{27, 29}, "cannot borrow a tuple"}});
}

BOOST_AUTO_TEST_CASE(cp_invalid_borrow_pattern) {
  // TODO update error to highlight type instead of pattern
  test_or_error(create_primative_env(), "(_ : &&i32) -> _ { 1 }", {{{1, 2}, "cannot borrow a borrow"}});
}

BOOST_AUTO_TEST_CASE(cp_return_borrow) {
  test_or_error(create_primative_env(), "() -> _ { &1 }", {{{10, 12}, "cannot return a borrowed value"}});
  test_or_error(create_primative_env(), "(x : &i32) -> _ { x }", {{{18, 19}, "cannot return a borrowed value"}});
  test_or_error(create_primative_env(), "() -> _ { (&1) }", {{{11, 13}, "cannot return a borrowed value"}});
}

BOOST_AUTO_TEST_CASE(cp_return_floating_borrow) {
  test_or_error(create_primative_env(), "(x) -> _ { &x }", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
  test_or_error(create_primative_env(), "(x : &_) -> _ { x }", {{{1, 2}, "unable to fully deduce type, deduced: &_"}});
  test_or_error(create_primative_env(), "(x) -> _ { (&x) }", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(cp_partial) {
  test_or_error(create_primative_env(), "(x) -> _ { x }", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(cp_borrow_partial) {
  test_or_error(
    create_primative_env(), "(x) -> _ { let _ = &x; () }", {{{1, 2}, "unable to fully deduce type, deduced: _"}});
}

BOOST_AUTO_TEST_CASE(cp_tuple_partial) {
  test_or_error(create_primative_env(), "(x: (_)) -> _ { x }", {{{1, 2}, "unable to fully deduce type, deduced: (_)"}});
  test_or_error(create_primative_env(), "(x) -> (_) { x }", {{{1, 2}, "unable to fully deduce type, deduced: (_)"}});
}

BOOST_AUTO_TEST_CASE(cp_function_identity) {
  test_or_unresolved(create_primative_env(), "(x) -> _ { x }", "(x: _) -> _ { x }");
}

BOOST_AUTO_TEST_CASE(cp_undeclared_function) {
  test_or_error(create_primative_env(), "() -> () { missing() }", {{{11, 20}, "use of undeclared function 'missing'"}});
}

BOOST_AUTO_TEST_CASE(cp_undeclared_binding) {
  test_or_error({}, "() -> _ { x }", {{{10, 11}, "use of undeclared binding 'x'"}});
  test_or_error({}, "() -> _ { (x, 1) }", {{{11, 12}, "use of undeclared binding 'x'"}});
  test_or_error({}, "() -> _ { let y = x; y }", {{{18, 19}, "use of undeclared binding 'x'"}});

  test_or_error({},
                "() -> () { let _y = x; z }",
                {{{20, 21}, "use of undeclared binding 'x'"}, {{23, 24}, "use of undeclared binding 'z'"}});
}

BOOST_AUTO_TEST_CASE(cp_pattern_mismatch) {
  test_or_error(create_primative_env(), "() -> () { let () = (1); () }", {{{15, 17}, "expected (), given (i32)"}});
  test_or_error({}, "() -> () { let (x) = (); () }", {{{15, 18}, "expected (_), given ()"}});
}

BOOST_AUTO_TEST_CASE(cp_return_type_mismatch) {
  test_or_error(create_primative_env(), "() -> () { 1 }", {{{11, 12}, "expected i32, given ()"}});
  test_or_error(create_primative_env(), "() -> () { (1) }", {{{11, 14}, "expected (i32), given ()"}});
}

BOOST_AUTO_TEST_CASE(cp_return_arity_mismatch) {
  test_or_error(create_primative_env(),
                "() -> (i32, i32) { (1, 1, 1) }",
                {{{19, 28}, "expected (i32, i32, i32), given (i32, i32)"}});
}

BOOST_AUTO_TEST_CASE(cp_unused_binding) {
  test_or_error(create_primative_env(),
                "(x: i32) -> _ { 1 }",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_or_error(create_primative_env(),
                "() -> _ { let x = 1; 1 }",
                {{{14, 15}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_or_error(create_primative_env(),
                "(x: i32) -> _ { let x = 1; x }",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});

  test_or_error(create_primative_env(),
                "() -> _ { let x = 1; let x = 1; x }",
                {{{14, 15}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(cp_unused_binding_ignore) {
  test_or(create_primative_env(), {}, "(_x: i32) -> i32 { 1 }", "(_x: i32) -> i32 { 1 }");
}

BOOST_AUTO_TEST_CASE(cp_function_ident_reuse) {
  test_or_error(create_primative_env(),
                "(x: i32, x: i32) -> _ { x }",
                {{{1, 2}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_or_error(create_primative_env(),
                "((x, x): (i32, i32)) -> _ { x }",
                {{{2, 3}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
  test_or_error(create_primative_env(),
                "((x, (x)): (i32, (i32))) -> _ { x }",
                {{{2, 3}, "unused binding 'x'", {"prefix with an _ to silence this error"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env,
                "() -> i32 { identity() }",
                {{{12, 22},
                  "no matching overload found",
                  {"deduced identity() -> i32 [1 candidate(s)]", "  identity(i32) -> i32"}}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(env, "(x: i32) -> i32 { let () = identity(x); x }", {{{22, 24}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_count) {
  Env env = create_primative_env();
  env.add_function("identity", [](i32 x) { return x; });

  test_or_error(
    env, "(x: i32) -> () { identity(x) }", {{{17, 28}, "expected identity(i32) -> i32, given identity(_) -> ()"}});
}

BOOST_AUTO_TEST_CASE(cp_multi_overload_match) {
  Env env = create_primative_env();
  env.add_function("f", []() { return i32(); });
  env.add_function("f", []() { return f32(); });

  test_or_error(env, "() -> (i32, f32) { let x = f(); (x, x) }", {{{23, 24}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_arg_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_or_error(e, "(x: u32) -> i32 { identity(x) }", {{{1, 2}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_bind_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_or_error(e, "(x: i32) -> i32 { let x: u32 = identity(x); x }", {{{22, 23}, "expected u32, given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_return_type) {
  Env e = create_primative_env();
  e.add_function("identity", [](i32 x) { return x; });

  test_or_error(
    e, "(x: i32) -> u32 { identity(x) }", {{{18, 29}, "expected identity(i32) -> i32, given identity(_) -> u32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_value_type) {
  Env e = create_primative_env();
  e.add_function("val", [](i32 x) {});

  test_or_error(e, "(x: &i32) -> () { val(x) }", {{{1, 2}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_empty_tuple_as_arg) {
  Env e = create_primative_env();
  e.add_function("take", [](i32) {});

  test_or_error(e, "() -> () { take(()) }", {{{16, 18}, "expected (), given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_wrong_type) {
  Env e = create_primative_env();

  test_or_error(e, "(x: i32) -> f32 { x }", {{{1, 2}, "expected i32, given f32"}});
  test_or_error(e, "(x: i32) -> f32 { let y = x; y }", {{{22, 23}, "expected f32, given i32"}});
  test_or_error(e, "(x: i32) -> f32 { let y: i32 = x; y }", {{{34, 35}, "expected i32, given f32"}});
  test_or_error(e, "(x: i32) -> (f32) { let y: i32 = x; (y) }", {{{37, 38}, "expected i32, given f32"}});
  test_or_error(e, "(x: i32) -> f32 { let y: f32 = x; y }", {{{1, 2}, "expected i32, given f32"}});
  test_or_error(e, "() -> f32 { 1 }", {{{12, 13}, "expected i32, given f32"}});
  test_or_error(e, "() -> f32 { let x = 1; x }", {{{16, 17}, "expected i32, given f32"}});
  test_or_error(e, "() -> f32 { let x: f32 = 1; x }", {{{16, 17}, "expected f32, given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_return_wrong_type_tuple_arg) {
  test_or_error(create_primative_env(), "((x): (i32)) -> f32 { x }", {{{2, 3}, "expected i32, given f32"}});
}

BOOST_AUTO_TEST_CASE(cp_missized_pattern) {
  Env e = create_primative_env();

  test_or_error(e, "(() : (_)) -> _ { 1 }", {{{1, 3}, "expected (), given (_)"}});
  test_or_error(e, "((x) : ()) -> _ { 1 }", {{{1, 4}, "expected (_), given ()"}});
  test_or_error(e, "((x) : (_, _)) -> _ { 1 }", {{{1, 4}, "expected (_), given (_, _)"}});

  test_or_error(e, "() -> _ { let () = (1); 1 }", {{{14, 16}, "expected (), given (i32)"}});
  test_or_error(e, "() -> _ { let (x) = (); 1 }", {{{14, 17}, "expected (_), given ()"}});
  test_or_error(e, "() -> _ { let (x) = (1, 1); 1 }", {{{14, 17}, "expected (_), given (i32, i32)"}});

  test_or_error(e, "(x) -> _ { let () : (_) = x; 1 }", {{{15, 17}, "expected (), given (_)"}});
  test_or_error(e, "(x) -> _ { let (x) : () = x; 1 }", {{{15, 18}, "expected (_), given ()"}});
  test_or_error(e, "(x) -> _ { let (x) : (_, _) = x; 1 }", {{{15, 18}, "expected (_), given (_, _)"}});
}

BOOST_AUTO_TEST_CASE(cp_only_undeclared_error) {
  Env e = create_primative_env();
  e.add_function("f", []() {});

  test_or_error(e, "() -> _ = f(missing())", {{{12, 21}, "use of undeclared function 'missing'"}});
}

BOOST_AUTO_TEST_CASE(cp_return_copy_ref_arg) {
  test_or_error(create_primative_env(), "(x: &i32) -> i32 = x", {{{1, 2}, "expected &i32, given i32"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_return) {
  test_name_error(create_primative_env(), "() -> abc = x", {{{6, 9}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_arg) {
  test_name_error(create_primative_env(), "(x: abc) -> () = ()", {{{4, 7}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_let) {
  test_name_error(create_primative_env(), "() -> () { let x : abc = y; x }", {{{19, 22}, "undefined type"}});
}

BOOST_AUTO_TEST_CASE(cp_expr_undefined_multi) {
  test_name_error(create_primative_env(),
                  "(x: a) -> b { let x : c = y; x }",
                  {{{4, 5}, "undefined type"}, {{10, 11}, "undefined type"}, {{22, 23}, "undefined type"}});
}

} // namespace ooze
