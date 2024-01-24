#include "test.h"

#include "bindings.h"
#include "pretty_print.h"
#include "runtime_test.h"

#include "ooze/core.h"
#include "ooze/executor/sequential_executor.h"

namespace ooze {

namespace {

auto await(Binding b) {
  return std::pair(b.type,
                   transform_to_vec(std::move(b.values), [](AsyncValue v) { return take(std::move(v)).wait(); }));
}

StringResult<std::pair<Type, std::vector<Any>>, Env> run(Env env, std::string_view script, std::string_view expr) {
  auto executor = make_seq_executor();

  return parse_scripts(std::move(env), make_sv_array(script))
    .append_state(Bindings{})
    .and_then([&](Env env, Bindings bindings) { return run(executor, std::move(env), std::move(bindings), expr); })
    .map_state([](Env env, Bindings bindings) {
      BOOST_REQUIRE(bindings.empty());
      return env;
    })
    .map([](Binding b, Env e) { return std::tuple(await(std::move(b)), std::move(e)); });
}

template <typename T>
void check_binding(
  const Env& e, const std::pair<Type, std::vector<Any>>& binding, std::string_view exp_type, const T& exp_value) {
  BOOST_CHECK_EQUAL(exp_type, pretty_print(make_sv_array(e.src), e.tg, e.native_types.names, binding.first));
  compare(exp_value, binding.second);
}

template <typename T>
void check_run(Env env, std::string_view script, std::string_view expr, std::string_view exp_type, const T& exp_value) {
  auto [type, renv] = check_result(run(std::move(env), script, expr));
  check_binding(renv, type, exp_type, exp_value);
}

StringResult<std::unordered_map<std::string, std::pair<Type, std::vector<Any>>>, Env>
assign(Env env, std::string_view script, std::string_view expr) {
  auto executor = make_seq_executor();

  return parse_scripts(std::move(env), make_sv_array(script))
    .append_state(Bindings{})
    .and_then([&](Env env, Bindings bindings) { return run(executor, std::move(env), std::move(bindings), expr); })
    .map([&](Binding output, Env env, Bindings bindings) {
      BOOST_REQUIRE(TypeTag::Tuple == env.tg.get<TypeTag>(output.type));
      BOOST_REQUIRE_EQUAL(0, env.tg.fanout(output.type).size());
      BOOST_REQUIRE_EQUAL(0, output.values.size());

      std::unordered_map<std::string, std::pair<Type, std::vector<Any>>> results;
      for(auto& [name, binding] : bindings) {
        results.emplace(std::move(name), await(std::move(binding)));
      }

      return std::tuple(std::move(results), std::move(env), Bindings{});
    })
    .map_state([](Env e, auto) { return e; });
}

template <typename... Ts, typename... Bs>
Any execute1(Env e, std::string_view name, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  for(const auto& [id, fn] : e.functions) {
    if(sv(make_sv_array(e.src), e.ast.srcs[id.get()]) == name) {
      std::vector<Any> results =
        execute(std::make_shared<const Program>(std::move(e.program)), fn, std::move(ts), std::move(bs));
      BOOST_REQUIRE_EQUAL(1, results.size());
      return std::move(results[0]);
    }
  }

  BOOST_REQUIRE(false);
  return {};
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
  Env env;
  env.add_type<i32>("i32");
  env.add_function("sum", [](int x, int y) { return x + y; });

  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 = sum(sum(x, y), y)";
  check_run(std::move(env), script, "f(5, 6)", "i32", std::tuple(17));
}

BOOST_AUTO_TEST_CASE(no_args) {
  Env env;
  env.add_type<i32>("i32");

  constexpr std::string_view script = "fn f() -> i32 = 17";
  check_run(std::move(env), script, "f()", "i32", std::tuple(17));
}

BOOST_AUTO_TEST_CASE(identity) {
  Env env;
  env.add_type<i32>("i32");

  constexpr std::string_view script = "fn f(x: i32) -> i32 = x";
  check_run(std::move(env), script, "f(5)", "i32", std::tuple(5));
}

BOOST_AUTO_TEST_CASE(borrow_param) {
  constexpr std::string_view script = "fn f(x: &i32) -> string = to_string(x)";
  check_run(create_primative_env(), script, "f(&1)", "string", std::string("1"));
}

BOOST_AUTO_TEST_CASE(borrow_assign) {
  constexpr std::string_view script = "fn f(x: i32) -> string { let x = &x; to_string(x) }";
  check_run(create_primative_env(), script, "f(1)", "string", std::string("1"));
}

BOOST_AUTO_TEST_CASE(tuple) { check_run(create_primative_env(), "", "((1), 2)", "((i32), i32)", std::tuple(1, 2)); }

BOOST_AUTO_TEST_CASE(tuple_fn) {
  constexpr std::string_view script = "fn f((w, x) : (i32, i32), (y, z): (i32, i32)) -> _ = ((z, x), (y, w))";
  check_run(create_primative_env(), script, "f((1, 2), (3, 4))", "((i32, i32), (i32, i32))", std::tuple(4, 2, 3, 1));
}

BOOST_AUTO_TEST_CASE(tuple_parameter) {
  constexpr std::string_view script = "fn f(x : (i32, i32)) -> _ { let (y, z) = x; (z, y) }";
  check_run(create_primative_env(), script, "f((1, 2))", "(i32, i32)", std::tuple(2, 1));
}

BOOST_AUTO_TEST_CASE(tuple_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let x = (1, 2); let (y, z) = x; (z, y) }";
  check_run(create_primative_env(), script, "f()", "(i32, i32)", std::tuple(2, 1));
}

BOOST_AUTO_TEST_CASE(fn_parameter) {
  constexpr std::string_view script =
    "fn one() -> i32 = 1\n"
    "fn f(g: fn() -> i32) -> i32 = g()\n";
  check_run(create_primative_env(), script, "f(one)", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(wildcard_parameter) {
  constexpr std::string_view script = "fn f(_ : i32, x : i32) -> _ = x";
  check_run(create_primative_env(), script, "f(1, 2)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(wildcard_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let (_, x) = (1, 2); x }";
  check_run(create_primative_env(), script, "f()", "i32", std::tuple(2));
}

struct Point {
  int x;
  int y;

  KNOT_COMPAREABLE(Point);
};

BOOST_AUTO_TEST_CASE(custom_type) {
  constexpr std::string_view script = "fn f(x: Point, y: Point) -> Point = sum(sum(x, y), y)";

  Env env = create_primative_env();
  add_tieable_type<Point>(env, "Point");
  env.add_function("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  check_run(
    std::move(env), script, "f(create_point(&1, &2), create_point(&9, &7))", "Point", std::tuple(Point{19, 16}));
}

BOOST_AUTO_TEST_CASE(already_move) {
  constexpr std::string_view script = "fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)";

  Env env = create_primative_env();
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  const std::vector<std::string> expected{"1:5 error: binding 'x' used more than once",
                                          " | fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)",
                                          " |      ^"};

  check_range(expected, check_error(run(std::move(env), script, "f(make_unique_int(0))")));
}

BOOST_AUTO_TEST_CASE(clone) {
  Env env;
  env.add_type<std::string>("string");
  check_run(std::move(env), "", "clone(&'abc')", "string", std::string("abc"));
}

BOOST_AUTO_TEST_CASE(expr_rebind) {
  constexpr std::string_view script = "fn f(x: i32) -> i32 { let x = double(x); let x = double(x); x }";

  Env env;
  env.add_type<i32>("i32");
  env.add_function("double", [](int x) { return x + x; });

  check_run(std::move(env), script, "f(1)", "i32", std::tuple(4));
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

  check_run(
    create_primative_env(), script, "f(1, 2)", "(i32, (string, i32, i32))", std::tuple(1, std::string("abc"), 2, 1));
}

BOOST_AUTO_TEST_CASE(select) {
  constexpr std::string_view script = "fn f(b: bool) -> i32  = select b { 1 } else { 2 }";
  check_run(create_primative_env(), script, "f(true)", "i32", std::tuple(1));
  check_run(create_primative_env(), script, "f(false)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(if_) {
  constexpr std::string_view script = "fn f(b: bool) -> i32  = if b { 1 } else { 2 }";
  check_run(create_primative_env(), script, "f(true)", "i32", std::tuple(1));
  check_run(create_primative_env(), script, "f(false)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(if_not_taken) {
  Env e = create_primative_env();
  e.add_function("never", []() {
    BOOST_REQUIRE(false);
    return 1;
  });

  constexpr std::string_view script =
    "fn f(b: bool) -> i32  = if b { 1 } else { never() }\n"
    "fn g(b: bool) -> i32  = if b { never() } else { 2 }\n";
  check_run(e, script, "f(true)", "i32", std::tuple(1));
  check_run(e, script, "g(false)", "i32", std::tuple(2));
}

BOOST_AUTO_TEST_CASE(if_capture_value) {
  constexpr std::string_view script =
    "fn f(b: bool) -> _ = {\n"
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
  check_run(create_primative_env(), script, "f(true)", exp_type, exp_if);

  const auto exp_else = std::tuple(
    std::string("r1"), std::string("r2"), std::string("c"), std::string("r3"), std::string("a"), std::string("b"));
  check_run(create_primative_env(), script, "f(false)", exp_type, exp_else);
}

BOOST_AUTO_TEST_CASE(if_nested) {
  constexpr std::string_view script =
    "fn f(b1: bool, b2: bool) -> string = {\n"
    "  let x = 'x';\n"
    "  let y = 'y';\n"
    "  let z = 'z';\n"
    "  if b1 { x } else if b2 { y } else { z }\n"
    "}";

  check_run(create_primative_env(), script, "f(true, true)", "string", std::string("x"));
  check_run(create_primative_env(), script, "f(true, false)", "string", std::string("x"));

  check_run(create_primative_env(), script, "f(false, true)", "string", std::string("y"));
  check_run(create_primative_env(), script, "f(false, false)", "string", std::string("z"));
}

BOOST_AUTO_TEST_CASE(if_capture_reorder_common) {
  Env e = create_primative_env();
  e.add_function("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: i32, y: i32) -> i32 = {\n"
    "  if b { sub(x, y) } else { sub(y, x) }\n"
    "}";

  check_run(e, script, "f(true, 0, 1)", "i32", std::tuple(-1));
  check_run(e, script, "f(false, 0, 1)", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(if_capture_unordered) {
  Env e;
  e.add_type<bool>("bool");
  e.add_type<i32>("i32");

  constexpr std::string_view script =
    "fn f(b: bool, x: i32, y: i32, z: i32) -> (i32, i32) = {\n"
    "  if b { (x, z) } else { (y, z) }\n"
    "}";

  check_run(e, script, "f(true, 0, 1, 2)", "(i32, i32)", std::tuple(0, 2));
  check_run(e, script, "f(false, 0, 1, 2)", "(i32, i32)", std::tuple(1, 2));
}

BOOST_AUTO_TEST_CASE(if_capture_borrow) {
  Env e;
  e.add_type<bool>("bool");
  e.add_type<std::string>("string");

  constexpr std::string_view script =
    "fn f(b: bool, x: &string, y: &string) -> string = {\n"
    "  if b { clone(x) } else { clone(y) }\n"
    "}";

  check_run(e, script, "f(true, &'a', &'b')", "string", std::string("a"));
  check_run(e, script, "f(false, &'a', &'b')", "string", std::string("b"));
}

BOOST_AUTO_TEST_CASE(if_capture_borrow_unordered) {
  Env e;
  e.add_type<bool>("bool");
  e.add_type<std::string>("string");

  e.add_function("f", [](const std::string& x, const std::string& y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x, y, z) -> string = {\n"
    "  if b { f(x, z) } else { f(y, z) }\n"
    "}";

  check_run(e, script, "f(true, &'a', &'b', &'c')", "string", std::string("ac"));
  check_run(e, script, "f(false, &'a', &'b', &'c')", "string", std::string("bc"));
}

BOOST_AUTO_TEST_CASE(if_nested_capture) {
  constexpr std::string_view script =
    "fn f(b1: bool, b2: bool) -> (string, string) = {\n"
    "  let a = 'a';\n"
    "  let x = 'x';\n"
    "  let y = 'y';\n"
    "  let z = 'z';\n"
    "  if b1 { (a, x) } else if b2 { (a, y) } else { (a, z) }\n"
    "}";

  check_run(create_primative_env(),
            script,
            "f(true, true)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("x")));
  check_run(create_primative_env(),
            script,
            "f(true, false)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("x")));

  check_run(create_primative_env(),
            script,
            "f(false, true)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("y")));
  check_run(create_primative_env(),
            script,
            "f(false, false)",
            "(string, string)",
            std::tuple(std::string("a"), std::string("z")));
}

BOOST_AUTO_TEST_CASE(if_capture_tuple_borrow) {
  Env e;
  e.add_type<bool>("bool");
  e.add_type<std::string>("string");

  e.add_function("f", [](const std::string& x, const std::string& y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: &string, y: &string) -> string = {\n"
    "  let t = (x, y);"
    "  if b { let (a, b) = t; f(a, b) } else { let (a, b) = t; f(b, a) }\n"
    "}";

  check_run(e, script, "f(true, &'a', &'b')", "string", std::string("ab"));
  check_run(e, script, "f(false, &'a', &'b')", "string", std::string("ba"));
}

BOOST_AUTO_TEST_CASE(if_capture_tuple_mixed) {
  Env e;
  e.add_type<bool>("bool");
  e.add_type<std::string>("string");

  e.add_function("f", [](const std::string& x, std::string y) { return x + y; });

  constexpr std::string_view script =
    "fn f(b: bool, x: string, y: string) -> string = {\n"
    "  let t: (&string, string) = (&x, y);"
    "  if b { let (a, b) = t; f(a, b) } else { let (a, b) = t; f(a, b) }\n"
    "}";

  check_run(e, script, "f(true, 'a', 'b')", "string", std::string("ab"));
  check_run(e, script, "f(false, 'a', 'b')", "string", std::string("ab"));
}

BOOST_AUTO_TEST_CASE(out_of_order) {
  constexpr std::string_view script =
    "fn f() -> _ = g()\n"
    "fn g() -> i32 = 1\n";

  Env env;
  env.add_type<i32>("i32");

  check_run(std::move(env), script, "f()", "i32", std::tuple(1));
}

BOOST_AUTO_TEST_CASE(recursion) {
  Env env;
  env.add_type<bool>("bool");
  env.add_type<i32>("i32");

  env.add_function("eq", [](i32 x, i32 y) { return x == y; });
  env.add_function("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn f(x: i32) -> i32 {\n"
    "  if eq(x, 0) { 0 } else { f(sub(x, 1)) }\n"
    "}";

  check_run(env, script, "f(0)", "i32", 0);
  check_run(env, script, "f(1)", "i32", 0);
  check_run(env, script, "f(2)", "i32", 0);
  check_run(env, script, "f(10)", "i32", 0);
}

BOOST_AUTO_TEST_CASE(fibonacci) {
  Env env;
  env.add_type<bool>("bool");
  env.add_type<i32>("i32");

  env.add_function("le", [](i32 x, i32 y) { return x <= y; });
  env.add_function("add", [](i32 x, i32 y) { return x + y; });
  env.add_function("sub", [](i32 x, i32 y) { return x - y; });

  constexpr std::string_view script =
    "fn fib(x: i32) -> i32 {\n"
    "  if le(x, 1) {\n"
    "    x\n"
    "  } else {\n"
    "    add(fib(sub(x, 1)), fib(sub(x, 2)))\n"
    "  }\n"
    "}";

  check_run(env, script, "fib(0)", "i32", 0);
  check_run(env, script, "fib(1)", "i32", 1);
  check_run(env, script, "fib(2)", "i32", 1);
  check_run(env, script, "fib(6)", "i32", 8);
}

BOOST_AUTO_TEST_CASE(generic, *boost::unit_test::disabled()) {
  constexpr std::string_view script =
    "fn f(x : &_) -> string = to_string(x)\n"
    "fn g(x: i32) -> string = f(&x)\n";

  check_run(create_primative_env(),
            script,
            "(g(3), f(&0.5))",
            "(string, string)",
            std::tuple(std::string("3"), std::string("0.5")));
}

BOOST_AUTO_TEST_CASE(assign_empty) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let () = ()"));
  BOOST_REQUIRE(m.empty());
}

BOOST_AUTO_TEST_CASE(assign_basic) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let x = 1"));
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
}

BOOST_AUTO_TEST_CASE(assign_tuple_destructure) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let (x, y) = (1, 2)"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
  check_binding(e, check_element("y", m), "i32", 2);
}

BOOST_AUTO_TEST_CASE(assign_tuple_nested_destructure) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let (x, (y, z)) = (1, (2, 3))"));
  BOOST_REQUIRE_EQUAL(3, m.size());
  check_binding(e, check_element("x", m), "i32", 1);
  check_binding(e, check_element("y", m), "i32", 2);
  check_binding(e, check_element("z", m), "i32", 3);
}

BOOST_AUTO_TEST_CASE(assign_tuple_wildcard) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let (_, x, _, y) = (1, 2, 3, 4)"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e, check_element("x", m), "i32", 2);
  check_binding(e, check_element("y", m), "i32", 4);
}

BOOST_AUTO_TEST_CASE(assign_tuple) {
  const auto [m, e] = check_result(assign(create_primative_env(), "", "let x = (1, 2)"));

  BOOST_REQUIRE_EQUAL(1, m.size());
  check_binding(e, check_element("x", m), "(i32, i32)", std::tuple(1, 2));
}

BOOST_AUTO_TEST_CASE(unnamed_type) {
  struct A {
    bool operator==(const A&) const { return true; }
  };

  Env e = create_primative_env();
  e.add_function("create", []() { return A{}; });
  e.add_function("identity", [](A a) { return a; });

  check_run(
    std::move(e), "", "identity(create())", fmt::format("type 0x{:x}", type_id(knot::Type<A>{}).id), std::tuple(A{}));
}

BOOST_AUTO_TEST_CASE(assign_deduce_overloads) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 5; });
  e.add_function("f", []() { return 3.0f; });

  const auto [m, e2] = check_result(assign(std::move(e), "", "let (x, y) : (i32, f32) = (f(), f())"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_binding(e2, check_element("x", m), "i32", 5);
  check_binding(e2, check_element("y", m), "f32", 3.0f);
}

BOOST_AUTO_TEST_CASE(assign_wrong_type) {
  const std::vector<std::string> expected{"1:4 error: expected f32, given i32", " | let x: f32 = 1", " |     ^"};
  check_range(expected, check_error(run(create_primative_env(), "", "let x: f32 = 1")));
}

BOOST_AUTO_TEST_CASE(run_borrow) {
  const std::vector<std::string> expected{"1:0 error: cannot return a borrowed value", " | &1", " | ^~"};
  check_range(expected, check_error(run(create_primative_env(), "", "&1")));
}

BOOST_AUTO_TEST_CASE(assign_borrow) {
  const std::vector<std::string> expected{
    "1:8 error: cannot return a borrowed value", " | let x = &1", " |         ^~"};
  check_range(expected, check_error(run(create_primative_env(), "", "let x = &1")));
}

BOOST_AUTO_TEST_CASE(undeclared_function) {
  const std::vector<std::string> expected{"1:0 error: undeclared binding 'f'", " | f()", " | ^"};
  check_range(expected, check_error(run(create_primative_env(), "", "f()")));
}

BOOST_AUTO_TEST_CASE(undeclared_binding) {
  const std::vector<std::string> expected{"1:0 error: undeclared binding 'x'", " | x", " | ^"};
  check_range(expected, check_error(run(create_primative_env(), "", "x")));
}

BOOST_AUTO_TEST_CASE(bad_pattern) {
  const std::vector<std::string> expected{"1:4 error: expected (_), given ()", " | let (x) = ()", " |     ^~~"};
  check_range(expected, check_error(run(create_primative_env(), "", "let (x) = ()")));
}

BOOST_AUTO_TEST_CASE(expr_or_error) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});

  const std::vector<std::string> expected{"1:2 error: expected string, given i32", " | f('abc')", " |   ^~~~~"};
  check_range(expected, check_error(run(std::move(e), "", "f('abc')")));
}

BOOST_AUTO_TEST_CASE(to_string) {
  auto executor = make_seq_executor();
  BOOST_CHECK_EQUAL("1", check_result_value(run_to_string(executor, create_primative_env(), Bindings{}, "1")));
}

BOOST_AUTO_TEST_CASE(to_string_fn) {
  auto executor = make_seq_executor();
  Env e = create_primative_env();
  e.add_function("f", []() { return std::string("abc"); });
  BOOST_CHECK_EQUAL("abc", check_result_value(run_to_string(executor, std::move(e), Bindings{}, "f()")));
}

BOOST_AUTO_TEST_CASE(copy_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();
  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 3"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "x"));
  check_binding(e, await(std::move(result)), "i32", 3);

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "x"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(extract_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();
  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 'abc'"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "x"));
  check_binding(e, await(std::move(result)), "string", std::string("abc"));

  const std::vector<std::string> expected{"1:0 error: undeclared binding 'x'", " | x", " | ^"};
  check_range(expected, check_error(run(std::move(e), "", "x")));
}

BOOST_AUTO_TEST_CASE(assign_env_fn) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();
  e.add_function("f", []() { return 3; });

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let f2 = f"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "f2()"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(assign_script_fn) {
  auto executor = make_seq_executor();

  Env e = check_result(parse_scripts(create_primative_env(), make_sv_array("fn f() -> i32 = 3")));

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let f2 = f"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "f2()"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(reuse_borrowed_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 3"));
  check_binding(e, await(std::move(result)), "()", std::tuple());

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "clone(&x)"));
  check_binding(e, await(std::move(result)), "i32", 3);

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "clone(&x)"));
  check_binding(e, await(std::move(result)), "i32", 3);
}

BOOST_AUTO_TEST_CASE(reuse_to_string_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();

  std::string result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run_to_string(executor, std::move(e), std::move(bindings), "let x = 1"));
  BOOST_CHECK_EQUAL("", result);

  std::tie(result, e, bindings) = check_result(run_to_string(executor, std::move(e), std::move(bindings), "x"));
  BOOST_CHECK_EQUAL("1", result);

  std::tie(result, e, bindings) = check_result(run_to_string(executor, std::move(e), std::move(bindings), "x"));
  BOOST_CHECK_EQUAL("1", result);
}

BOOST_AUTO_TEST_CASE(reuse_assign_binding_indirect) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 1"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let y = clone(&x)"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let z = clone(&x)"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "(x, y, z)"));
  check_binding(e, await(std::move(result)), "(i32, i32, i32)", std::tuple(1, 1, 1));
}

BOOST_AUTO_TEST_CASE(tuple_untuple) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 3"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let y = 'abc'"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let z = (x, y)"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let (a, b) = z"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "(a, b)"));
  check_binding(e, await(std::move(result)), "(i32, string)", std::tuple(3, std::string("abc")));
}

BOOST_AUTO_TEST_CASE(overload_fn_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let f = 1"));

  const std::vector<std::string> expected{
    "1:0 error: ambiguous overload", " | f", " | ^", "deduced _ [2 candidate(s)]", "  fn() -> i32", "  i32"};

  check_range(expected, check_error(run(executor, std::move(e), std::move(bindings), "f")));
}

BOOST_AUTO_TEST_CASE(overwrite_binding) {
  auto executor = make_seq_executor();

  Env e = create_primative_env();

  Binding result;
  Bindings bindings;

  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 3"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "let x = 4"));
  std::tie(result, e, bindings) = check_result(run(executor, std::move(e), std::move(bindings), "x"));
  check_binding(e, await(std::move(result)), "i32", 4);
}

BOOST_AUTO_TEST_CASE(print_fn) {
  auto executor = make_seq_executor();
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });

  // TODO improve this error
  check_error(run_to_string(executor, std::move(e), Bindings{}, "f"));
}

BOOST_AUTO_TEST_CASE(native_constant_fn) {
  Env e;
  e.add_type<i32>("i32");
  e.add_function("f", []() { return 3; });
  check_any(3, execute1(std::move(e), "f", {}, {}));
}

BOOST_AUTO_TEST_CASE(native_identity_fn) {
  Env e;
  e.add_type<i32>("i32");
  e.add_function("f", [](i32 x) { return x; });
  check_any(7, execute1(std::move(e), "f", std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(native_clone_fn) {
  Env e;
  e.add_type<i32>("i32");
  check_any(7, execute1(std::move(e), "clone", {}, std::tuple(7)));
}

BOOST_AUTO_TEST_CASE(script_constant_fn) {
  Env e;
  e.add_type<i32>("i32");

  e = check_result(parse_scripts(std::move(e), make_sv_array("fn f() -> i32 = 3")));

  check_any(3, execute1(std::move(e), "f", {}, {}));
}

BOOST_AUTO_TEST_CASE(script_identity_fn) {
  Env e;
  e.add_type<i32>("i32");

  e = check_result(parse_scripts(std::move(e), make_sv_array("fn f(x: i32) -> i32 = x")));

  check_any(7, execute1(std::move(e), "f", std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(script_call_native) {
  Env e;

  e.add_type<i32>("i32");
  e.add_function("c", [](const i32& x) { return x; });

  e = check_result(parse_scripts(std::move(e), make_sv_array("fn f(x: &i32) -> i32 = c(x)")));

  check_any(7, execute1(std::move(e), "f", {}, std::tuple(7)));
}

BOOST_AUTO_TEST_CASE(script_call_script) {
  Env e;
  e.add_type<i32>("i32");

  e = check_result(parse_scripts(std::move(e), make_sv_array("fn f(x: i32) -> i32 = x", "fn g(x: i32) -> i32 = f(x)")));

  check_any(7, execute1(std::move(e), "g", std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(script_parse_error_env_same) {
  Env e;
  e.add_type<i32>("i32");

  auto [errors, e2] = check_error_state(parse_scripts(e, make_sv_array("fn f() -> i32 = ")));

  BOOST_CHECK_EQUAL(e.src, e2.src);
  BOOST_CHECK(e.ast == e2.ast);
  BOOST_CHECK(e.tg == e2.tg);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
