#include "test.h"

#include "bindings.h"
#include "pretty_print.h"
#include "runtime_test.h"

#include "ooze/core.h"
#include "ooze/executor.h"

namespace ooze {

namespace {

StringResult<std::pair<Type, std::vector<Any>>, Env>
run(NativeRegistry r, std::string_view script, std::string_view expr) {
  auto executor = make_seq_executor();

  return Env(std::move(r))
    .parse_scripts(make_sv_array(script))
    .and_then([&](Env env) { return std::move(env).run(executor, expr); })
    .map_state([](Env env) {
      BOOST_REQUIRE(env.bindings().empty());
      return env;
    })
    .map([](Binding b, Env e) { return std::tuple(await(std::move(b)), std::move(e)); });
}

template <typename T>
void check_binding(
  const Env& e, const std::pair<Type, std::vector<Any>>& binding, std::string_view exp_type, const T& exp_value) {
  BOOST_CHECK_EQUAL(exp_type, e.pretty_print(binding.first));
  compare(exp_value, binding.second);
}

template <typename T>
void check_run(
  NativeRegistry r, std::string_view script, std::string_view expr, std::string_view exp_type, const T& exp_value) {
  auto [binding, renv] = check_result(run(std::move(r), script, expr));
  check_binding(renv, binding, exp_type, exp_value);
}

StringResult<std::unordered_map<std::string, std::pair<Type, std::vector<Any>>>, Env>
assign(NativeRegistry r, std::string_view script, std::string_view expr) {
  auto executor = make_seq_executor();

  return Env(std::move(r))
    .parse_scripts(make_sv_array(script))
    .and_then([&](Env env) { return std::move(env).run(executor, expr); })
    .map([&](Binding output, Env env) {
      BOOST_REQUIRE_EQUAL("()", env.pretty_print(output.type));
      BOOST_REQUIRE_EQUAL(0, output.values.size());

      std::unordered_map<std::string, std::pair<Type, std::vector<Any>>> results;
      for(auto& [name, type, state] : env.bindings()) {
        results.emplace(name, await(check_result(env.run(executor, name))));
      }

      return std::tuple(std::move(results), std::move(env));
    });
}

Any execute1(Env env, std::string_view expr) {
  auto executor = make_seq_executor();
  Binding binding = check_result(env.run(executor, expr));
  BOOST_REQUIRE_EQUAL(1, binding.values.size());
  return await(take(std::move(binding.values[0])));
}

#define check_any_tree(_EXP, _EXPR)                                                                                    \
  [](auto e, const Tree<Any>& t) {                                                                                     \
    BOOST_REQUIRE(std::holds_alternative<Any>(t.v));                                                                   \
    check_any(e, std::get<Any>(t.v));                                                                                  \
  }(_EXP, _EXPR)

#define check_vec(_SIZE, _EXPR)                                                                                        \
  [](size_t s, const Tree<Any>& t) -> const std::vector<Tree<Any>>& {                                                  \
    BOOST_REQUIRE(std::holds_alternative<std::vector<Tree<Any>>>(t.v));                                                \
    BOOST_REQUIRE(s == std::get<std::vector<Tree<Any>>>(t.v).size());                                                  \
    return std::get<std::vector<Tree<Any>>>(t.v);                                                                      \
  }(_SIZE, _EXPR)

#define check_element(_NAME, _MAP)                                                                                     \
  [](const std::string& name, const auto& m) -> const auto& {                                                          \
    const auto it = m.find(name);                                                                                      \
    BOOST_REQUIRE(it != m.end());                                                                                      \
    return it->second;                                                                                                 \
  }                                                                                                                    \
  (_NAME, _MAP)

} // namespace

BOOST_AUTO_TEST_SUITE(ooze)

BOOST_AUTO_TEST_CASE(basic) {
  auto r = NativeRegistry{}.add_type<i32>("i32").add_fn("sum", [](int x, int y) { return x + y; });

  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 { sum(sum(x, y), y) }";
  check_run(std::move(r), script, "f(5, 6)", "i32", std::tuple(17));
}

BOOST_AUTO_TEST_CASE(no_args) {
  constexpr std::string_view script = "fn f() -> i32 { 17 }";
  check_run(NativeRegistry{}.add_type<i32>("i32"), script, "f()", "i32", std::tuple(17));
}

BOOST_AUTO_TEST_CASE(identity) {
  constexpr std::string_view script = "fn f(x: i32) -> i32 { x }";
  check_run(NativeRegistry{}.add_type<i32>("i32"), script, "f(5)", "i32", std::tuple(5));
}

BOOST_AUTO_TEST_CASE(borrow_param) {
  constexpr std::string_view script = "fn f(x: &i32) -> string { to_string(x) }";
  check_run(create_primitive_registry(), script, "f(&1)", "string", std::string("1"));
}

BOOST_AUTO_TEST_CASE(borrow_assign) {
  constexpr std::string_view script = "fn f(x: i32) -> string { let x = &x; to_string(x) }";
  check_run(create_primitive_registry(), script, "f(1)", "string", std::string("1"));
}

BOOST_AUTO_TEST_CASE(tuple) {
  check_run(create_primitive_registry(), "", "((1), 2)", "((i32), i32)", std::tuple(1, 2));
}

BOOST_AUTO_TEST_CASE(tuple_fn) {
  constexpr std::string_view script = "fn f((w, x) : (i32, i32), (y, z): (i32, i32)) { ((z, x), (y, w)) }";
  check_run(
    create_primitive_registry(), script, "f((1, 2), (3, 4))", "((i32, i32), (i32, i32))", std::tuple(4, 2, 3, 1));
}

BOOST_AUTO_TEST_CASE(tuple_parameter) {
  constexpr std::string_view script = "fn f(x : (i32, i32)){ let (y, z) = x; (z, y) }";
  check_run(create_primitive_registry(), script, "f((1, 2))", "(i32, i32)", std::tuple(2, 1));
}

BOOST_AUTO_TEST_CASE(tuple_assignment) {
  constexpr std::string_view script = "fn f() { let x = (1, 2); let (y, z) = x; (z, y) }";
  check_run(create_primitive_registry(), script, "f()", "(i32, i32)", std::tuple(2, 1));
}

BOOST_AUTO_TEST_CASE(fn_parameter) {
  constexpr std::string_view script =
    "fn one() -> i32 { 1 }\n"
    "fn f(g: fn() -> i32) -> i32 { g() }\n";
  check_run(create_primitive_registry(), script, "f(one)", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(wildcard_parameter) {
  constexpr std::string_view script = "fn f(_ : i32, x : i32) { x }";
  check_run(create_primitive_registry(), script, "f(1, 2)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(wildcard_assignment) {
  constexpr std::string_view script = "fn f() { let (_, x) = (1, 2); x }";
  check_run(create_primitive_registry(), script, "f()", "i32", std::tuple(2));
}

struct Point {
  int x;
  int y;

  friend bool operator==(const Point&, const Point&) = default;
};

BOOST_AUTO_TEST_CASE(custom_type) {
  constexpr std::string_view script = "fn f(x: Point, y: Point) -> Point { sum(sum(x, y), y) }";

  auto r =
    create_primitive_registry()
      .add_type<Point>("Point")
      .add_fn("create_point",
              [](i32 x, i32 y) {
                return Point{x, y};
              })
      .add_fn("sum", [](Point p1, Point p2) {
        return Point{p1.x + p2.x, p1.y + p2.y};
      });

  check_run(std::move(r), script, "f(create_point(1, 2), create_point(9, 7))", "Point", std::tuple(Point{19, 16}));
}

BOOST_AUTO_TEST_CASE(already_move) {
  constexpr std::string_view script = "fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }";

  auto r =
    create_primitive_registry().add_type<std::unique_ptr<int>>("unique_int").add_fn("make_unique_int", [](int x) {
      return std::make_unique<int>(x);
    });

  const std::vector<std::string> expected{"1:5 error: binding 'x' used more than once",
                                          " | fn f(x: unique_int) -> (unique_int, unique_int) { (x, x) }",
                                          " |      ^"};

  check_range(expected, check_error(run(std::move(r), script, "f(make_unique_int(0))")));
}

BOOST_AUTO_TEST_CASE(clone) {
  check_run(NativeRegistry{}.add_type<std::string>("string"), "", "clone(&'abc')", "string", std::string("abc"));
}

BOOST_AUTO_TEST_CASE(expr_rebind) {
  constexpr std::string_view script = "fn f(x: i32) -> i32 { let x = double(x); let x = double(x); x }";

  auto r = NativeRegistry{}.add_type<i32>("i32").add_fn("double", [](int x) { return x + x; });

  check_run(std::move(r), script, "f(1)", "i32", std::tuple(4));
}

BOOST_AUTO_TEST_CASE(scope) {
  constexpr std::string_view script =
    "fn f(a: i32, b: i32) -> (i32, (string, i32, i32)) {"
    "  let b = {"
    "    let c : i32 = a;"
    "    let a : string = 'abc';"
    "    (a, b, c)"
    "  };"
    "  (a, b)"
    "}";

  check_run(create_primitive_registry(),
            script,
            "f(1, 2)",
            "(i32, (string, i32, i32))",
            std::tuple(1, std::string("abc"), 2, 1));
}

BOOST_AUTO_TEST_CASE(if_) {
  constexpr std::string_view script = "fn f(b: bool) -> i32 { if b { 1 } else { 2 } }";
  check_run(create_primitive_registry(), script, "f(true)", "i32", std::tuple(1));
  check_run(create_primitive_registry(), script, "f(false)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(if_not_taken) {
  auto r = create_primitive_registry().add_fn("never", []() {
    BOOST_REQUIRE(false);
    return 1;
  });

  constexpr std::string_view script =
    "fn f(b: bool) -> i32 { if b { 1 } else { never() } }\n"
    "fn g(b: bool) -> i32 { if b { never() } else { 2 } }\n";
  check_run(r, script, "f(true)", "i32", std::tuple(1));
  check_run(r, script, "g(false)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(if_capture_value) {
  constexpr std::string_view script =
    "fn f(b: bool) {\n"
    "  let common_tuple = ('a', 'b');\n"
    "  let common = 'c';\n"
    "  let left_tuple = ('l1', 'l2');\n"
    "  let left = 'l3';\n"
    "  let right_tuple = ('r1', 'r2');\n"
    "  let right = 'r3';\n"
    "  if b { (common_tuple, left, common, left_tuple) } else { (right_tuple, common, right, common_tuple) }\n"
    "}";

  const std::string_view exp_type = "((string, string), string, string, (string, string))";

  const auto exp_if = std::tuple(
    std::string("a"), std::string("b"), std::string("l3"), std::string("c"), std::string("l1"), std::string("l2"));
  check_run(create_primitive_registry(), script, "f(true)", exp_type, exp_if);

  const auto exp_else = std::tuple(
    std::string("r1"), std::string("r2"), std::string("c"), std::string("r3"), std::string("a"), std::string("b"));
  check_run(create_primitive_registry(), script, "f(false)", exp_type, exp_else);
}

BOOST_AUTO_TEST_CASE(if_value_borrow) {
  constexpr std::string_view script = "fn f(b: bool, x: string) -> string { if b { x } else { clone(&x) } }";
  check_run(create_primitive_registry(), script, "f(true, 'abc')", "string", std::tuple(std::string("abc")));
  check_run(create_primitive_registry(), script, "f(false, 'abc')", "string", std::tuple(std::string("abc")));
}

BOOST_AUTO_TEST_CASE(if_nested) {
  constexpr std::string_view script =
    "fn f(b1: bool, b2: bool) -> string {\n"
    "  let x = 'x';\n"
    "  let y = 'y';\n"
    "  let z = 'z';\n"
    "  if b1 { x } else if b2 { y } else { z }\n"
    "}";

  check_run(create_primitive_registry(), script, "f(true, true)", "string", std::string("x"));
  check_run(create_primitive_registry(), script, "f(true, false)", "string", std::string("x"));

  check_run(create_primitive_registry(), script, "f(false, true)", "string", std::string("y"));
  check_run(create_primitive_registry(), script, "f(false, false)", "string", std::string("z"));
}

BOOST_AUTO_TEST_CASE(if_capture_reorder_common) {
  auto r =
    NativeRegistry{}.add_type<bool>("bool").add_type<i32>("i32").add_fn("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: i32, y: i32) -> i32 {\n"
    "  if b { sub(x, y) } else { sub(y, x) }\n"
    "}";

  check_run(r, script, "f(true, 0, 1)", "i32", std::tuple(-1));
  check_run(r, script, "f(false, 0, 1)", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(if_capture_unordered) {
  auto r = NativeRegistry{}.add_type<bool>("bool").add_type<i32>("i32");

  constexpr std::string_view script =
    "fn f(b: bool, x: i32, y: i32, z: i32) -> (i32, i32) {\n"
    "  if b { (x, z) } else { (y, z) }\n"
    "}";

  check_run(r, script, "f(true, 0, 1, 2)", "(i32, i32)", std::tuple(0, 2));
  check_run(r, script, "f(false, 0, 1, 2)", "(i32, i32)", std::tuple(1, 2));
}

BOOST_AUTO_TEST_CASE(if_capture_borrow) {
  auto r = NativeRegistry{}.add_type<bool>("bool").add_type<std::string>("string");

  constexpr std::string_view script =
    "fn f(b: bool, x: &string, y: &string) -> string {\n"
    "  if b { clone(x) } else { clone(y) }\n"
    "}";

  check_run(r, script, "f(true, &'a', &'b')", "string", std::string("a"));
  check_run(r, script, "f(false, &'a', &'b')", "string", std::string("b"));
}

BOOST_AUTO_TEST_CASE(if_capture_borrow_unordered) {
  auto r = NativeRegistry{}.add_type<bool>("bool").add_type<std::string>("string").add_fn(
    "f", [](const std::string& x, const std::string& y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x, y, z) -> string {\n"
    "  if b { f(x, z) } else { f(y, z) }\n"
    "}";

  check_run(r, script, "f(true, &'a', &'b', &'c')", "string", std::string("ac"));
  check_run(r, script, "f(false, &'a', &'b', &'c')", "string", std::string("bc"));
}

BOOST_AUTO_TEST_CASE(if_nested_capture) {
  constexpr std::string_view script =
    "fn f(b1: bool, b2: bool) -> (string, string) {\n"
    "  let a = 'a';\n"
    "  let x = 'x';\n"
    "  let y = 'y';\n"
    "  let z = 'z';\n"
    "  if b1 { (a, x) } else if b2 { (a, y) } else { (a, z) }\n"
    "}";

  check_run(create_primitive_registry(),
            script,
            "f(true, true)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("x")));
  check_run(create_primitive_registry(),
            script,
            "f(true, false)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("x")));

  check_run(create_primitive_registry(),
            script,
            "f(false, true)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("y")));
  check_run(create_primitive_registry(),
            script,
            "f(false, false)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("z")));
}

BOOST_AUTO_TEST_CASE(if_capture_tuple_borrow) {
  auto r = NativeRegistry{}.add_type<bool>("bool").add_type<std::string>("string").add_fn(
    "f", [](const std::string& x, const std::string& y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: &string, y: &string) -> string {\n"
    "  let t = (x, y);"
    "  if b { let (a, b) = t; f(a, b) } else { let (a, b) = t; f(b, a) }\n"
    "}";

  check_run(r, script, "f(true, &'a', &'b')", "string", std::string("ab"));
  check_run(r, script, "f(false, &'a', &'b')", "string", std::string("ba"));
}

BOOST_AUTO_TEST_CASE(if_capture_tuple_mixed) {
  auto r = NativeRegistry{}.add_type<bool>("bool").add_type<std::string>("string").add_fn(
    "f", [](const std::string& x, std::string y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: string, y: string) -> string {\n"
    "  let t: (&string, string) = (&x, y);"
    "  if b { let (a, b) = t; f(a, b) } else { let (a, b) = t; f(a, b) }\n"
    "}";

  check_run(r, script, "f(true, 'a', 'b')", "string", std::string("ab"));
  check_run(r, script, "f(false, 'a', 'b')", "string", std::string("ab"));
}

BOOST_AUTO_TEST_CASE(out_of_order) {
  constexpr std::string_view script =
    "fn f() { g() }\n"
    "fn g() -> i32 { 1 }\n";

  check_run(NativeRegistry{}.add_type<i32>("i32"), script, "f()", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(recursion) {
  auto r = NativeRegistry{}
             .add_type<bool>("bool")
             .add_type<i32>("i32")
             .add_fn("eq", [](i32 x, i32 y) { return x == y; })
             .add_fn("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn f(x: i32) -> i32 {\n"
    "  if eq(x, 0) { 0 } else { f(sub(x, 1)) }\n"
    "}";

  check_run(r, script, "f(0)", "i32", 0);
  check_run(r, script, "f(1)", "i32", 0);
  check_run(r, script, "f(2)", "i32", 0);
  check_run(r, script, "f(10)", "i32", 0);
}

BOOST_AUTO_TEST_CASE(fibonacci) {
  auto r = NativeRegistry{}
             .add_type<bool>("bool")
             .add_type<i32>("i32")
             .add_fn("le", [](i32 x, i32 y) { return x <= y; })
             .add_fn("add", [](i32 x, i32 y) { return x + y; })
             .add_fn("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn fib(x: i32) -> i32 {\n"
    "  if le(x, 1) {\n"
    "    x\n"
    "  } else {\n"
    "    add(fib(sub(x, 1)), fib(sub(x, 2)))\n"
    "  }\n"
    "}";

  check_run(r, script, "fib(0)", "i32", 0);
  check_run(r, script, "fib(1)", "i32", 1);
  check_run(r, script, "fib(2)", "i32", 1);
  check_run(r, script, "fib(6)", "i32", 8);
}

BOOST_AUTO_TEST_CASE(generic_script) {
  constexpr std::string_view script =
    "fn f(x : &_) -> string { to_string(x) }\n"
    "fn g(x: i32) -> string { f(&x) }\n"
    "fn h(x: f64) -> string { f(&x) }\n";

  auto r = NativeRegistry{}
             .add_type<std::string>("string")
             .add_type<i32>("i32")
             .add_type<f64>("f64")
             .add_fn("to_string", [](const i32& x) { return fmt::format("{}", x); })
             .add_fn("to_string", [](const f64& x) { return fmt::format("{}", x); });

  check_run(
    std::move(r), script, "(g(3), h(0.5))", "(string, string)", std::tuple(std::string("3"), std::string("0.5")));
}

BOOST_AUTO_TEST_CASE(generic) {
  constexpr std::string_view script = "fn f(x : &_) -> string { to_string(x) }\n";

  auto r = NativeRegistry{}
             .add_type<std::string>("string")
             .add_type<i32>("i32")
             .add_type<f64>("f64")
             .add_fn("to_string", [](const i32& x) { return fmt::format("{}", x); })
             .add_fn("to_string", [](const f64& x) { return fmt::format("{}", x); });

  check_run(
    std::move(r), script, "(f(&3), f(&0.5))", "(string, string)", std::tuple(std::string("3"), std::string("0.5")));
}

BOOST_AUTO_TEST_CASE(generic_assign) {
  constexpr std::string_view script = "fn f(x : &_) -> string { to_string(x) }\n";

  auto r = NativeRegistry{}.add_type<std::string>("string").add_type<i32>("i32").add_fn("to_string", [](const i32& x) {
    return fmt::format("{}", x);
  });

  const auto [m, e] = check_result(assign(std::move(r), script, "let x = f(&3);"));
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_binding(e, check_element("x", m), "string", std::string("3"));
}

BOOST_AUTO_TEST_CASE(assign_empty) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let () = ();"));
  BOOST_REQUIRE(m.empty());
}

BOOST_AUTO_TEST_CASE(assign_basic) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let x = 1;"));
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
}

BOOST_AUTO_TEST_CASE(assign_tuple_destructure) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let (x, y) = (1, 2);"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
  check_binding(e, check_element("y", m), "i32", 2);
}

BOOST_AUTO_TEST_CASE(assign_tuple_nested_destructure) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let (x, (y, z)) = (1, (2, 3));"));
  BOOST_REQUIRE_EQUAL(3, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
  check_binding(e, check_element("y", m), "i32", 2);
  check_binding(e, check_element("z", m), "i32", 3);
}

BOOST_AUTO_TEST_CASE(assign_tuple_wildcard) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let (_, x, _, y) = (1, 2, 3, 4);"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e, check_element("x", m), "i32", 2);
  check_binding(e, check_element("y", m), "i32", 4);
}

BOOST_AUTO_TEST_CASE(assign_tuple) {
  const auto [m, e] = check_result(assign(create_primitive_registry(), "", "let x = (1, 2);"));

  BOOST_REQUIRE_EQUAL(1, m.size());
  check_binding(e, check_element("x", m), "(i32, i32)", std::tuple(1, 2));
}

BOOST_AUTO_TEST_CASE(unnamed_type) {
  struct A {
    bool operator==(const A&) const { return true; }
  };

  auto r = create_primitive_registry().add_fn("create", []() { return A{}; }).add_fn("identity", [](A a) { return a; });

  check_run(
    std::move(r), "", "identity(create())", fmt::format("type 0x{:x}", type_id(knot::Type<A>{}).id), std::tuple(A{}));
}

BOOST_AUTO_TEST_CASE(assign_deduce_overloads) {
  auto r = create_primitive_registry().add_fn("f", []() { return 5; }).add_fn("f", []() { return 3.0f; });

  const auto [m, e] = check_result(assign(std::move(r), "", "let (x, y) : (i32, f32) = (f(), f());"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e, check_element("x", m), "i32", 5);
  check_binding(e, check_element("y", m), "f32", 3.0f);
}

BOOST_AUTO_TEST_CASE(assign_wrong_type) {
  const std::vector<std::string> expected{"1:4 error: expected f32, given i32", " | let x: f32 = 1;", " |     ^"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "let x: f32 = 1;")));
}

BOOST_AUTO_TEST_CASE(run_borrow) {
  const std::vector<std::string> expected{"1:0 error: cannot return a borrowed value", " | &1", " | ^~"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "&1")));
}

BOOST_AUTO_TEST_CASE(assign_borrow) {
  const std::vector<std::string> expected{
    "1:8 error: cannot return a borrowed value", " | let x = &1;", " |         ^~"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "let x = &1;")));
}

BOOST_AUTO_TEST_CASE(undeclared_function) {
  const std::vector<std::string> expected{"1:0 error: undeclared binding 'f'", " | f()", " | ^"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "f()")));
}

BOOST_AUTO_TEST_CASE(undeclared_binding) {
  const std::vector<std::string> expected{"1:0 error: undeclared binding 'x'", " | x", " | ^"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "x")));
}

BOOST_AUTO_TEST_CASE(bad_pattern) {
  const std::vector<std::string> expected{"1:4 error: expected (_), given ()", " | let (x) = ();", " |     ^~~"};
  check_range(expected, check_error(run(create_primitive_registry(), "", "let (x) = ();")));
}

BOOST_AUTO_TEST_CASE(expr_or_error) {
  auto r = create_primitive_registry().add_fn("f", [](i32) {});

  const std::vector<std::string> expected{"1:2 error: expected string, given i32", " | f('abc')", " |   ^~~~~"};
  check_range(expected, check_error(run(std::move(r), "", "f('abc')")));
}

BOOST_AUTO_TEST_CASE(to_string) {
  auto executor = make_seq_executor();
  check_result_value(Env(create_primitive_registry()).run_to_string(executor, "1")).then([](Any a) {
    check_any(std::string("1"), a);
  });
}

BOOST_AUTO_TEST_CASE(to_string_fn) {
  auto executor = make_seq_executor();
  auto r = create_primitive_registry().add_fn("f", []() { return std::string("abc"); });
  check_result_value(Env(std::move(r)).run_to_string(executor, "f()")).then([](Any a) {
    check_any(std::string("abc"), a);
  });
}

BOOST_AUTO_TEST_CASE(copy_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());
  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 3;"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e) = check_result(std::move(e).run(executor, "x"));
  check_binding(e, await(std::move(result)), "i32", 3);

  std::tie(result, e) = check_result(std::move(e).run(executor, "x"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(extract_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());
  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 'abc';"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e) = check_result(std::move(e).run(executor, "x"));
  check_binding(e, await(std::move(result)), "string", std::string("abc"));

  const std::vector<std::string> expected{"1:0 error: undeclared binding 'x'", " | x", " | ^"};
  check_range(expected, check_error(std::move(e).run(executor, "x")));
}

BOOST_AUTO_TEST_CASE(assign_env_fn) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry().add_fn("f", []() { return 3; }));

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let f2 = f;"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e) = check_result(std::move(e).run(executor, "f2()"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(assign_script_fn) {
  auto executor = make_seq_executor();

  Env e = check_result(Env(create_primitive_registry()).parse_scripts(make_sv_array("fn f() -> i32 { 3 }")));

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let f2 = f;"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e) = check_result(std::move(e).run(executor, "f2()"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(reuse_borrowed_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 3;"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e) = check_result(std::move(e).run(executor, "clone(&x)"));
  check_binding(e, await(std::move(result)), "i32", 3);

  std::tie(result, e) = check_result(std::move(e).run(executor, "clone(&x)"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(reuse_to_string_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());

  Future result;

  std::tie(result, e) = check_result(std::move(e).run_to_string(executor, "let x = 1;"));
  std::move(result).then([](Any a) { check_any(std::string(), a); });

  std::tie(result, e) = check_result(std::move(e).run_to_string(executor, "x"));
  std::move(result).then([](Any a) { check_any(std::string("1"), a); });

  std::tie(result, e) = check_result(std::move(e).run_to_string(executor, "x"));
  std::move(result).then([](Any a) { check_any(std::string("1"), a); });
}

BOOST_AUTO_TEST_CASE(reuse_assign_binding_indirect) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 1;"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let y = clone(&x);"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let z = clone(&x);"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "(x, y, z)"));
  check_binding(e, await(std::move(result)), "(i32, i32, i32)", std::tuple(1, 1, 1));
}

BOOST_AUTO_TEST_CASE(tuple_untuple) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());

  Binding result;
  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 3;"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let y = 'abc';"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let z = (x, y);"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let (a, b) = z;"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "(a, b)"));
  check_binding(e, await(std::move(result)), "(i32, string)", std::tuple(3, std::string("abc")));
}

BOOST_AUTO_TEST_CASE(overload_fn_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry().add_fn("f", []() { return 1; }));

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let f = 1;"));

  const std::vector<std::string> expected{
    "1:0 error: ambiguous overload", " | f", " | ^", "deduced _ [2 candidate(s)]", "  fn() -> i32", "  i32"};

  check_range(expected, check_error(std::move(e).run(executor, "f")));
}

BOOST_AUTO_TEST_CASE(overwrite_binding) {
  auto executor = make_seq_executor();

  Env e = Env(create_primitive_registry());

  Binding result;

  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 3;"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "let x = 4;"));
  std::tie(result, e) = check_result(std::move(e).run(executor, "x"));
  check_binding(e, await(std::move(result)), "i32", 4);
}

BOOST_AUTO_TEST_CASE(print_fn) {
  auto executor = make_seq_executor();
  Env e = Env(create_primitive_registry().add_fn("f", []() { return 1; }));

  // TODO improve this error
  check_error(std::move(e).run_to_string(executor, "f"));
}

BOOST_AUTO_TEST_CASE(native_constant_fn) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32").add_fn("f", []() { return 3; }));
  check_any(3, execute1(std::move(e), "f()"));
}

BOOST_AUTO_TEST_CASE(native_identity_fn) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32").add_fn("f", [](i32 x) { return x; }));
  check_any(7, execute1(std::move(e), "f(7)"));
}

BOOST_AUTO_TEST_CASE(native_clone_fn) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  check_any(7, execute1(std::move(e), "clone(&7)"));
}

BOOST_AUTO_TEST_CASE(script_constant_fn) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  check_result(e.parse_scripts(make_sv_array("fn f() -> i32 { 3 }")));
  check_any(3, execute1(std::move(e), "f()"));
}

BOOST_AUTO_TEST_CASE(script_identity_fn) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  check_result(e.parse_scripts(make_sv_array("fn f(x: i32) -> i32 { x }")));
  check_any(7, execute1(std::move(e), "f(7)"));
}

BOOST_AUTO_TEST_CASE(script_call_native) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32").add_fn("c", [](const i32& x) { return x; }));
  check_result(e.parse_scripts(make_sv_array("fn f(x: &i32) -> i32 { c(x) }")));
  check_any(7, execute1(std::move(e), "f(&7)"));
}

BOOST_AUTO_TEST_CASE(script_call_script) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  check_result(e.parse_scripts(make_sv_array("fn f(x: i32) -> i32 { x }", "fn g(x: i32) -> i32 { f(x) }")));
  check_any(7, execute1(std::move(e), "g(7)"));
}

BOOST_AUTO_TEST_CASE(script_parse_error_env_preserved) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  check_result(e.parse_scripts(make_sv_array("fn f() -> i32 { 1 }")));
  check_error_state(e.parse_scripts(make_sv_array("fn f() -> i32 = ")));

  const auto globals = e.globals();

  BOOST_REQUIRE_EQUAL(2, globals.size());
  BOOST_CHECK_EQUAL("clone", globals[0].first);
  BOOST_CHECK_EQUAL("fn(&i32) -> i32", e.pretty_print(globals[0].second));
  BOOST_CHECK_EQUAL("f", globals[1].first);
  BOOST_CHECK_EQUAL("fn() -> i32", e.pretty_print(globals[1].second));
}

BOOST_AUTO_TEST_CASE(borrow_dependency_hang) {
  auto r = create_primitive_registry().add_fn("append", [](std::string x, const std::string& y) { return x + y; });

  constexpr std::string_view expr = "{ let x = 'abc'; append(x, &x) }";

  const std::vector<std::string> expected = {
    "1:6 error: Dependency Error", " | { let x = 'abc'; append(x, &x) }", " |       ^"};

  check_range(expected, check_error(run(std::move(r), "", expr)));
}

BOOST_AUTO_TEST_CASE(borrow_dependency_hang2) {
  auto r = create_primitive_registry()
             .add_fn("append", [](std::string x, const std::string& y) { return x + y; })
             .add_fn("identity", [](std::string x) { return x; });

  constexpr std::string_view expr = "{ let x = 'abc'; append(identity(x), &x) }";

  const std::vector<std::string> expected = {
    "1:6 error: Dependency Error", " | { let x = 'abc'; append(identity(x), &x) }", " |       ^"};

  check_range(expected, check_error(run(std::move(r), "", expr)));
}

BOOST_AUTO_TEST_CASE(borrow_dependency_hang_if_indirect) {
  auto r = create_primitive_registry().add_fn("append", [](const std::string& x, std::string y) { return x + y; });

  constexpr std::string_view expr =
    "{\n"
    "  let x = 'abc';\n"
    "  let y = if false { 'def' } else { x };\n"
    "  append(&x, y)\n"
    "}";

  const std::vector<std::string> expected = {"2:6 error: Dependency Error", " |   let x = 'abc';", " |       ^"};

  check_range(expected, check_error(run(std::move(r), "", expr)));
}

BOOST_AUTO_TEST_CASE(borrow_dependency_no_hang) {
  auto r = create_primitive_registry().add_fn("append", [](std::string x, std::string y) { return x + y; });

  constexpr std::string_view expr =
    "{"
    "  let x = 'abc';"
    "  append(x, clone(&x))"
    "}";

  check_run(std::move(r), "", expr, "string", std::string("abcabc"));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
