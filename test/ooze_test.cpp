#include "test.h"

#include "bindings.h"
#include "ooze/core.h"

#include <anyf/executor/task_executor.h>
#include <anyf/graph_execution.h>

namespace ooze {

namespace {

Result<Tree<Any>> run(Env e, std::string_view script, std::string_view expr) {
  RuntimeEnv r = make_default_runtime(std::move(e));

  return parse_script(r.env, script).and_then([&]() { return ooze::run(r, expr); }).map([](Tree<Binding> bindings) {
    return await(std::move(bindings));
  });
}

auto run_or_assign(Env e, std::string_view script, std::string_view expr) {
  RuntimeEnv r = make_default_runtime(std::move(e));

  return parse_script(r.env, script)
    .and_then([&]() { return ooze::run_or_assign(r, expr); })
    .map([&](Tree<Binding> bindings) {
      std::unordered_map<std::string, Tree<Any>> assignments;
      for(auto& [name, tree] : r.bindings) {
        assignments.emplace(name, await(std::move(tree)));
      }

      return std::pair(await(std::move(bindings)), std::move(assignments));
    });
}

#define check_any(_EXP, _EXPR)                                                                                         \
  [](auto e, const Any& a) {                                                                                           \
    BOOST_REQUIRE(anyf::holds_alternative<decltype(e)>(a));                                                            \
    BOOST_REQUIRE(e == anyf::any_cast<decltype(e)>(a));                                                                \
  }(_EXP, _EXPR)

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
  [](const std::string& name, const std::unordered_map<std::string, Tree<Any>>& m) -> const Tree<Any>& {               \
    const auto it = m.find(name);                                                                                      \
    BOOST_REQUIRE(it != m.end());                                                                                      \
    return it->second;                                                                                                 \
  }(_NAME, _MAP)

} // namespace

BOOST_AUTO_TEST_CASE(ooze_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 = sum(sum(x, y), y)";
  Env env = create_primative_env();
  env.add_function("sum", [](int x, int y) { return x + y; });

  check_any_tree(17, check_result(run(std::move(env), script, "f(5, 6)")));
}

BOOST_AUTO_TEST_CASE(ooze_expr_fn_no_args) {
  constexpr std::string_view script = "fn f() -> i32 = 17";
  check_any_tree(17, check_result(run(create_primative_env(), script, "f()")));
}

BOOST_AUTO_TEST_CASE(ooze_ref_param_ref) {
  constexpr std::string_view script = "fn f(x: &i32) -> string = to_string(x)";
  check_any_tree(std::string("1"), check_result(run(create_primative_env(), script, "f(&1)")));
}

BOOST_AUTO_TEST_CASE(ooze_ref_assign_ref) {
  constexpr std::string_view script = "fn f(x: i32) -> string { let x = &x; to_string(x) }";
  check_any_tree(std::string("1"), check_result(run(create_primative_env(), script, "f(1)")));
}

BOOST_AUTO_TEST_CASE(ooze_assign_basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 = sum(sum(x, y), y)";
  Env env = create_primative_env();
  env.add_function("sum", [](int x, int y) { return x + y; });

  check_any_tree(17, check_result(run_or_assign(std::move(env), script, "f(5, 6)")).first);
}

BOOST_AUTO_TEST_CASE(ooze_tuple) {
  const Tree<Any> tree = check_result(run(create_primative_env(), "", "((1), 2)"));
  const auto& v = check_vec(2, tree);
  check_any_tree(1, check_vec(1, v[0])[0]);
  check_any_tree(2, v[1]);
}

BOOST_AUTO_TEST_CASE(ooze_tuple_function) {
  constexpr std::string_view script = "fn f((w, x) : (i32, i32), (y, z): (i32, i32)) -> _ = ((z, x), (y, w))";
  const Tree<Any> tree = check_result(run(create_primative_env(), script, "f((1, 2), (3, 4))"));
  const auto& v = check_vec(2, tree);
  const auto& va = check_vec(2, v[0]);
  const auto& vb = check_vec(2, v[1]);
  check_any_tree(4, va[0]);
  check_any_tree(2, va[1]);
  check_any_tree(3, vb[0]);
  check_any_tree(1, vb[1]);
}

BOOST_AUTO_TEST_CASE(ooze_tuple_parameter) {
  constexpr std::string_view script = "fn f(x : (i32, i32)) -> _ { let (y, z) = x; (z, y) }";

  const Tree<Any> tree = check_result(run(create_primative_env(), script, "f((1, 2))"));
  const auto& v = check_vec(2, tree);
  check_any_tree(2, v[0]);
  check_any_tree(1, v[1]);
}

BOOST_AUTO_TEST_CASE(ooze_tuple_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let x = (1, 2); let (y, z) = x; (z, y) }";

  const Tree<Any> tree = check_result(run(create_primative_env(), script, "f()"));
  const auto& v = check_vec(2, tree);
  check_any_tree(2, v[0]);
  check_any_tree(1, v[1]);
}

BOOST_AUTO_TEST_CASE(ooze_wildcard_parameter) {
  constexpr std::string_view script = "fn f(_ : i32, x : i32) -> _ = x";
  check_any_tree(2, check_result(run(create_primative_env(), script, "f(1, 2)")));
}

BOOST_AUTO_TEST_CASE(ooze_wildcard_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let (_, x) = (1, 2); x }";
  check_any_tree(2, check_result(run(create_primative_env(), script, "f()")));
}

struct Point {
  int x;
  int y;

  KNOT_COMPAREABLE(Point);
};

BOOST_AUTO_TEST_CASE(ooze_custom_type) {
  const auto script = "fn f(x: Point, y: Point) -> Point = sum(sum(x, y), y)";

  Env env = create_primative_env();
  add_tieable_type<Point>(env, "Point");
  env.add_function("sum", [](Point p1, Point p2) { return Point{p1.x + p2.x, p1.y + p2.y}; });

  check_any_tree((Point{19, 16}),
                 check_result(run(std::move(env), script, "f(create_point(&1, &2), create_point(&9, &7))")));
}

BOOST_AUTO_TEST_CASE(ooze_already_move) {
  const auto script = "fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)";

  Env env = create_primative_env();
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  const std::vector<std::string> expected{"1:54 error: binding already moved",
                                          " | fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)",
                                          " |                                                       ^"};

  BOOST_CHECK(expected == check_error(run(std::move(env), script, "f(make_unique_int(0))")));
}

BOOST_AUTO_TEST_CASE(ooze_clone) {
  Env env;
  env.add_type<std::string>("string");
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  check_any_tree(std::string("abc"), check_result(run(std::move(env), "", "clone(&'abc')")));
}

BOOST_AUTO_TEST_CASE(ooze_expr_rebind) {
  const auto script = "fn f(x: i32) -> i32 { let x = double(x); let x = double(x); x }";

  Env env = create_primative_env();
  env.add_function("double", [](int x) { return x + x; });

  check_any_tree(4, check_result(run(std::move(env), script, "f(1)")));
}

BOOST_AUTO_TEST_CASE(ooze_scope) {
  constexpr std::string_view script = "fn f(a: i32, b: i32) -> (i32, (string, i32, i32)) {"
                                      "  let b = {"
                                      "    let c : i32 = a;"
                                      "    let a : string = 'abc';"
                                      "    (a, b, c)"
                                      "  };"
                                      "  (a, b)"
                                      "}";

  const auto tree = check_result(run(create_primative_env(), script, "f(1, 2)"));

  const auto& v = check_vec(2, tree);
  check_any_tree(1, v[0]);
  const auto& v2 = check_vec(3, v[1]);
  check_any_tree(std::string("abc"), v2[0]);
  check_any_tree(2, v2[1]);
  check_any_tree(1, v2[2]);
}

BOOST_AUTO_TEST_CASE(ooze_assign) {
  const auto m = check_result(run_or_assign(create_primative_env(), "", "let x = 1")).second;
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_any_tree(1, check_element("x", m));
}

BOOST_AUTO_TEST_CASE(ooze_assign_tuple_destructure) {
  const auto m = check_result(run_or_assign(create_primative_env(), "", "let (x, y) = (1, 2)")).second;

  BOOST_REQUIRE_EQUAL(2, m.size());
  check_any_tree(1, check_element("x", m));
  check_any_tree(2, check_element("y", m));
}

BOOST_AUTO_TEST_CASE(ooze_assign_tuple_wildcard) {
  const auto m = check_result(run_or_assign(create_primative_env(), "", "let (_, x) = (1, 2)")).second;
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_any_tree(2, check_element("x", m));
}

BOOST_AUTO_TEST_CASE(ooze_assign_tuple) {
  const auto m = check_result(run_or_assign(create_primative_env(), "", "let x = (1, 2)")).second;

  BOOST_REQUIRE_EQUAL(1, m.size());
  const Tree<Any>& tree = check_element("x", m);
  const auto& v = check_vec(2, tree);
  check_any_tree(1, v[0]);
  check_any_tree(2, v[1]);
}

BOOST_AUTO_TEST_CASE(ooze_expr_unnamed_type) {
  struct A {
    bool operator==(const A&) const { return true; }
  };

  Env e = create_primative_env();
  e.add_function("create", []() { return A{}; });
  e.add_function("identity", [](A a) { return a; });

  check_any_tree(A{}, check_result(run(std::move(e), "", "identity(create())")));
}

BOOST_AUTO_TEST_CASE(ooze_assign_deduce_overloads) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 5; });
  e.add_function("f", []() { return 3.0f; });

  const auto m = check_result(run_or_assign(std::move(e), "", "let (x, y) : (i32, f32) = (f(), f())")).second;
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_any_tree(5, check_element("x", m));
  check_any_tree(3.0f, check_element("y", m));
}

BOOST_AUTO_TEST_CASE(ooze_assign_wrong_type) {
  const std::vector<std::string> expected{
    "1:13 error: expected i32, given f32", " | let x: f32 = 1", " |              ^"};

  const auto error = check_error(run_or_assign(create_primative_env(), "", "let x: f32 = 1"));
  BOOST_CHECK(expected == error);
}

BOOST_AUTO_TEST_CASE(ooze_expr_undeclared_function) {
  const std::vector<std::string> expected{"1:0 error: use of undeclared function 'f'", " | f()", " | ^~~"};

  const auto error = check_error(run(create_primative_env(), "", "f()"));
  BOOST_CHECK(expected == error);
}

BOOST_AUTO_TEST_CASE(ooze_expr_undeclared_binding) {
  const std::vector<std::string> expected{"1:0 error: use of undeclared binding 'x'", " | x", " | ^"};

  const auto error = check_error(run(create_primative_env(), "", "x"));
  BOOST_CHECK(expected == error);
}

BOOST_AUTO_TEST_CASE(ooze_assign_bad_pattern) {
  const std::vector<std::string> expected{"1:4 error: expected (_), given ()", " | let (x) = ()", " |     ^~~"};

  const auto error = check_error(run_or_assign(create_primative_env(), "", "let (x) = ()"));
  BOOST_CHECK(expected == error);
}

BOOST_AUTO_TEST_CASE(ooze_expr_or_error) {
  Env e = create_primative_env();
  e.add_function("f", [](i32) {});

  const std::vector<std::string> expected{"1:0 error: no matching overload found",
                                          " | f('abc')",
                                          " | ^~~~~~~~",
                                          "deduced f(string) -> _ [1 candidate(s)]",
                                          "  f(i32) -> ()"};

  const auto error = check_error(run(std::move(e), "", "f('abc')"));
  BOOST_CHECK(expected == error);
}

BOOST_AUTO_TEST_CASE(ooze_to_string) {
  RuntimeEnv r = make_default_runtime(create_primative_env());
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(r, "1")));
}

BOOST_AUTO_TEST_CASE(ooze_to_string_fn) {
  Env e = create_primative_env();
  e.add_function("f", []() { return std::string("abc"); });

  RuntimeEnv r = make_default_runtime(std::move(e));

  BOOST_CHECK_EQUAL("abc", check_result(run_to_string(r, "f()")));
}

BOOST_AUTO_TEST_CASE(ooze_reuse_ref_binding) {
  Env e = create_primative_env();
  e.add_function("f", [](const int& x) { return std::to_string(x); });

  RuntimeEnv r = make_default_runtime(std::move(e));
  check_result(run_or_assign(r, "let x = 3"));
  check_any_tree(std::string("3"), await(check_result(run(r, "f(&x)"))));
  check_any_tree(std::string("3"), await(check_result(run(r, "f(&x)"))));
}

BOOST_AUTO_TEST_CASE(ooze_reuse_to_string_binding) {
  RuntimeEnv r = make_default_runtime(create_primative_env());
  check_result(run_or_assign(r, "let x = 1"));
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(r, "x")));
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(r, "x")));
}

BOOST_AUTO_TEST_CASE(ooze_reuse_to_string_binding_indirect) {
  Env e = create_primative_env();
  e.add_function("f", [](const int& i) { return i; });

  RuntimeEnv r = make_default_runtime(std::move(e));
  check_result(run_or_assign(r, "let x = 1"));
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(r, "f(&x)")));
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(r, "f(&x)")));
}

BOOST_AUTO_TEST_CASE(ooze_reuse_assign_binding_indirect) {
  Env e = create_primative_env();
  e.add_function("f", [](const int& i) { return i; });

  RuntimeEnv r = make_default_runtime(std::move(e));
  check_result(run_or_assign(r, "let x = 1"));
  check_result(run_or_assign(r, "let y = f(&x)"));
  check_result(run_or_assign(r, "let z = f(&x)"));

  r.bindings.clear();

  check_result(run_to_string_or_assign(r, "let x = 1"));
  check_result(run_to_string_or_assign(r, "let y = f(&x)"));
  check_result(run_to_string_or_assign(r, "let z = f(&x)"));
}

BOOST_AUTO_TEST_CASE(ooze_tuple_untuple) {
  RuntimeEnv r = make_default_runtime(create_primative_env());

  check_result(run_or_assign(r, "let x = 3"));
  check_result(run_or_assign(r, "let y = 'abc'"));
  check_result(run_or_assign(r, "let z = (x, y)"));
  check_result(run_or_assign(r, "let (a, b) = z"));

  BOOST_CHECK_EQUAL("3", check_result(run_to_string(r, "a")));
  BOOST_CHECK_EQUAL("abc", check_result(run_to_string(r, "b")));
}

} // namespace ooze
