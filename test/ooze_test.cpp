#include "test.h"

#include "bindings.h"
#include "ooze/core.h"
#include "ooze/executor/task_executor.h"

namespace ooze {

namespace {

StringResult<Tree<Any>> run(Env env, std::string_view script, std::string_view expr) {
  auto executor = make_task_executor();

  return parse_script(std::move(env), script)
    .append_state(Bindings{})
    .and_then([&](Env env, Bindings bindings) { return run(executor, std::move(env), std::move(bindings), expr); })
    .map_state([](Env, Bindings bindings) {
      BOOST_REQUIRE(bindings.empty());
      return std::tuple();
    })
    .map(await);
}

StringResult<std::unordered_map<std::string, Tree<Any>>>
assign(Env env, std::string_view script, std::string_view expr) {
  auto executor = make_task_executor();

  return parse_script(std::move(env), script)
    .append_state(Bindings{})
    .and_then([&](Env env, Bindings bindings) { return run(executor, std::move(env), std::move(bindings), expr); })
    .map([&](Tree<Binding> output, Env env, Bindings bindings) {
      BOOST_REQUIRE(output.v.index() == 0 && std::get<0>(output.v).size() == 0);
      std::unordered_map<std::string, Tree<Any>> results;
      for(auto& [name, tree] : bindings) {
        results.emplace(std::move(name), await(std::move(tree)));
      }
      return std::tuple(std::move(results), std::move(env), Bindings{});
    })
    .map_state(nullify());
}

#define check_any(_EXP, _EXPR)                                                                                         \
  [](auto e, const Any& a) {                                                                                           \
    BOOST_REQUIRE(holds_alternative<decltype(e)>(a));                                                                  \
    BOOST_REQUIRE(e == any_cast<decltype(e)>(a));                                                                      \
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

BOOST_AUTO_TEST_SUITE(ooze)

BOOST_AUTO_TEST_CASE(basic) {
  constexpr std::string_view script = "fn f(x: i32, y: i32) -> i32 = sum(sum(x, y), y)";
  Env env = create_primative_env();
  env.add_function("sum", [](int x, int y) { return x + y; });

  check_any_tree(17, check_result(run(std::move(env), script, "f(5, 6)")));
}

BOOST_AUTO_TEST_CASE(no_args) {
  constexpr std::string_view script = "fn f() -> i32 = 17";
  check_any_tree(17, check_result(run(create_primative_env(), script, "f()")));
}

BOOST_AUTO_TEST_CASE(identity) {
  constexpr std::string_view script = "fn f(x: i32) -> i32 = x";
  check_any_tree(5, check_result(run(create_primative_env(), script, "f(5)")));
}

BOOST_AUTO_TEST_CASE(borrow_param) {
  constexpr std::string_view script = "fn f(x: &i32) -> string = to_string(x)";
  check_any_tree(std::string("1"), check_result(run(create_primative_env(), script, "f(&1)")));
}

BOOST_AUTO_TEST_CASE(borrow_assign) {
  constexpr std::string_view script = "fn f(x: i32) -> string { let x = &x; to_string(x) }";
  check_any_tree(std::string("1"), check_result(run(create_primative_env(), script, "f(1)")));
}

BOOST_AUTO_TEST_CASE(tuple) {
  const Tree<Any> tree = check_result(run(create_primative_env(), "", "((1), 2)"));
  const auto& v = check_vec(2, tree);
  check_any_tree(1, check_vec(1, v[0])[0]);
  check_any_tree(2, v[1]);
}

BOOST_AUTO_TEST_CASE(tuple_fn) {
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

BOOST_AUTO_TEST_CASE(tuple_parameter) {
  constexpr std::string_view script = "fn f(x : (i32, i32)) -> _ { let (y, z) = x; (z, y) }";

  const Tree<Any> tree = check_result(run(create_primative_env(), script, "f((1, 2))"));
  const auto& v = check_vec(2, tree);
  check_any_tree(2, v[0]);
  check_any_tree(1, v[1]);
}

BOOST_AUTO_TEST_CASE(tuple_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let x = (1, 2); let (y, z) = x; (z, y) }";

  const Tree<Any> tree = check_result(run(create_primative_env(), script, "f()"));
  const auto& v = check_vec(2, tree);
  check_any_tree(2, v[0]);
  check_any_tree(1, v[1]);
}

BOOST_AUTO_TEST_CASE(fn_parameter) {
  constexpr std::string_view script =
    "fn one() -> i32 = 1\n"
    "fn f(g: fn() -> i32) -> i32 = g()\n";
  check_any_tree(1, check_result(run(create_primative_env(), script, "f(one)")));
}

BOOST_AUTO_TEST_CASE(wildcard_parameter) {
  constexpr std::string_view script = "fn f(_ : i32, x : i32) -> _ = x";
  check_any_tree(2, check_result(run(create_primative_env(), script, "f(1, 2)")));
}

BOOST_AUTO_TEST_CASE(wildcard_assignment) {
  constexpr std::string_view script = "fn f() -> _ { let (_, x) = (1, 2); x }";
  check_any_tree(2, check_result(run(create_primative_env(), script, "f()")));
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

  check_any_tree((Point{19, 16}),
                 check_result(run(std::move(env), script, "f(create_point(&1, &2), create_point(&9, &7))")));
}

BOOST_AUTO_TEST_CASE(already_move) {
  constexpr std::string_view script = "fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)";

  Env env = create_primative_env();
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  const std::vector<std::string> expected{
    "1:5 error: binding 'x' used 2 times", " | fn f(x: unique_int) -> (unique_int, unique_int) = (x, x)", " |      ^"};

  check_range(expected, check_error(run(std::move(env), script, "f(make_unique_int(0))")));
}

BOOST_AUTO_TEST_CASE(clone) {
  Env env;
  env.add_type<std::string>("string");
  env.add_type<std::unique_ptr<int>>("unique_int");
  env.add_function("make_unique_int", [](int x) { return std::make_unique<int>(x); });

  check_any_tree(std::string("abc"), check_result(run(std::move(env), "", "clone(&'abc')")));
}

BOOST_AUTO_TEST_CASE(expr_rebind) {
  constexpr std::string_view script = "fn f(x: i32) -> i32 { let x = double(x); let x = double(x); x }";

  Env env = create_primative_env();
  env.add_function("double", [](int x) { return x + x; });

  check_any_tree(4, check_result(run(std::move(env), script, "f(1)")));
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

  const auto tree = check_result(run(create_primative_env(), script, "f(1, 2)"));

  const auto& v = check_vec(2, tree);
  check_any_tree(1, v[0]);
  const auto& v2 = check_vec(3, v[1]);
  check_any_tree(std::string("abc"), v2[0]);
  check_any_tree(2, v2[1]);
  check_any_tree(1, v2[2]);
}

BOOST_AUTO_TEST_CASE(select) {
  constexpr std::string_view script = "fn f(b: bool) -> i32  = select b { 1 } else { 2 }";
  check_any_tree(1, check_result(run(create_primative_env(), script, "f(true)")));
  check_any_tree(2, check_result(run(create_primative_env(), script, "f(false)")));
}

BOOST_AUTO_TEST_CASE(ooze_out_of_order, *boost::unit_test::disabled()) {
  constexpr std::string_view script =
    "fn f() -> _ = g()\n"
    "fn g() -> i32 = 1\n";

  Env e = create_primative_env();

  check_void_result(parse_script(e, script));
}

BOOST_AUTO_TEST_CASE(generic) {
  constexpr std::string_view script =
    "fn f(x : &_) -> string = to_string(x)\n"
    "fn g(x: i32) -> string = f(&x)\n";

  const auto tree = check_result(run(create_primative_env(), script, "(g(3), f(&0.5))"));

  const auto& v = check_vec(2, tree);
  check_any_tree(std::string("3"), v[0]);
  check_any_tree(std::string("0.5"), v[1]);
}

BOOST_AUTO_TEST_CASE(basic_assign) {
  const auto m = check_result(assign(create_primative_env(), "", "let x = 1"));
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_any_tree(1, check_element("x", m));
}

BOOST_AUTO_TEST_CASE(assign_tuple_destructure) {
  const auto m = check_result(assign(create_primative_env(), "", "let (x, y) = (1, 2)"));

  BOOST_REQUIRE_EQUAL(2, m.size());
  check_any_tree(1, check_element("x", m));
  check_any_tree(2, check_element("y", m));
}

BOOST_AUTO_TEST_CASE(assign_tuple_wildcard) {
  const auto m = check_result(assign(create_primative_env(), "", "let (_, x) = (1, 2)"));
  BOOST_REQUIRE_EQUAL(1, m.size());
  check_any_tree(2, check_element("x", m));
}

BOOST_AUTO_TEST_CASE(assign_tuple) {
  const auto m = check_result(assign(create_primative_env(), "", "let x = (1, 2)"));

  BOOST_REQUIRE_EQUAL(1, m.size());
  const Tree<Any>& tree = check_element("x", m);
  const auto& v = check_vec(2, tree);
  check_any_tree(1, v[0]);
  check_any_tree(2, v[1]);
}

BOOST_AUTO_TEST_CASE(unnamed_type) {
  struct A {
    bool operator==(const A&) const { return true; }
  };

  Env e = create_primative_env();
  e.add_function("create", []() { return A{}; });
  e.add_function("identity", [](A a) { return a; });

  check_any_tree(A{}, check_result(run(std::move(e), "", "identity(create())")));
}

BOOST_AUTO_TEST_CASE(assign_deduce_overloads) {
  Env e = create_primative_env();
  e.add_function("f", []() { return 5; });
  e.add_function("f", []() { return 3.0f; });

  const auto m = check_result(assign(std::move(e), "", "let (x, y) : (i32, f32) = (f(), f())"));
  BOOST_REQUIRE_EQUAL(2, m.size());
  check_any_tree(5, check_element("x", m));
  check_any_tree(3.0f, check_element("y", m));
}

BOOST_AUTO_TEST_CASE(assign_wrong_type) {
  const std::vector<std::string> expected{
    "1:13 error: expected i32, given f32", " | let x: f32 = 1", " |              ^"};
  check_range(expected, check_error(run(create_primative_env(), "", "let x: f32 = 1")));
}

BOOST_AUTO_TEST_CASE(undeclared_function) {
  const std::vector<std::string> expected{"1:0 error: use of undeclared binding 'f'", " | f()", " | ^"};
  check_range(expected, check_error(run(create_primative_env(), "", "f()")));
}

BOOST_AUTO_TEST_CASE(undeclared_binding) {
  const std::vector<std::string> expected{"1:0 error: use of undeclared binding 'x'", " | x", " | ^"};
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
  auto executor = make_task_executor();
  BOOST_CHECK_EQUAL("1", check_result(run_to_string(executor, create_primative_env(), {}, "1")));
}

BOOST_AUTO_TEST_CASE(to_string_fn) {
  auto executor = make_task_executor();
  Env e = create_primative_env();
  e.add_function("f", []() { return std::string("abc"); });
  BOOST_CHECK_EQUAL("abc", check_result(run_to_string(executor, std::move(e), {}, "f()")));
}

BOOST_AUTO_TEST_CASE(reuse_borrowed_binding) {
  auto executor = make_task_executor();

  Env e = create_primative_env();
  e.add_function("f", [](const int& x) { return std::to_string(x); });

  const auto result =
    run(executor, std::move(e), {}, "let x = 3")
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "f(&x)");
      })
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        check_any_tree(std::string("3"), await(std::move(result)));
        return run(executor, std::move(env), std::move(bindings), "f(&x)");
      })
      .map([](Tree<Binding> result, Env env, Bindings bindings) {
        check_any_tree(std::string("3"), await(std::move(result)));
        return std::tuple(std::move(env), std::move(bindings));
      });

  if(!result) {
    fmt::print("{}\n", knot::debug(result.error()));
    BOOST_CHECK(result);
  }
}

BOOST_AUTO_TEST_CASE(reuse_to_string_binding) {
  auto executor = make_task_executor();

  const auto result =
    run(executor, create_primative_env(), {}, "let x = 1")
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run_to_string(executor, std::move(env), std::move(bindings), "x");
      })
      .and_then([&](std::string result, Env env, Bindings bindings) {
        BOOST_REQUIRE_EQUAL("1", result);
        return run_to_string(executor, std::move(env), std::move(bindings), "x");
      })
      .map([](std::string result, Env env, Bindings bindings) {
        BOOST_REQUIRE_EQUAL("1", result);
        return std::tuple(std::move(env), std::move(bindings));
      });

  if(!result) {
    fmt::print("{}\n", knot::debug(result.error()));
    BOOST_CHECK(result);
  }
}

BOOST_AUTO_TEST_CASE(reuse_to_string_indirect) {
  auto executor = make_task_executor();

  Env e = create_primative_env();
  e.add_function("f", [](const int& x) { return x; });

  const auto result =
    run(executor, std::move(e), {}, "let x = 1")
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run_to_string(executor, std::move(env), std::move(bindings), "f(&x)");
      })
      .and_then([&](std::string result, Env env, Bindings bindings) {
        BOOST_REQUIRE_EQUAL("1", result);
        return run_to_string(executor, std::move(env), std::move(bindings), "f(&x)");
      })
      .map([](std::string result, Env env, Bindings bindings) {
        BOOST_REQUIRE_EQUAL("1", result);
        return std::tuple(std::move(env), std::move(bindings));
      });

  if(!result) {
    fmt::print("{}\n", knot::debug(result.error()));
    BOOST_CHECK(result);
  }
}

BOOST_AUTO_TEST_CASE(reuse_assign_binding_indirect) {
  auto executor = make_task_executor();

  Env e = create_primative_env();
  e.add_function("f", [](const int& x) { return x; });

  const auto result =
    run(executor, std::move(e), {}, "let x = 1")
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "let y = f(&x)");
      })
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "let z = f(&x)");
      });

  if(!result) {
    fmt::print("{}\n", knot::debug(result.error()));
    BOOST_CHECK(result);
  }
}

BOOST_AUTO_TEST_CASE(tuple_untuple) {
  auto executor = make_task_executor();

  const auto result =
    run(executor, create_primative_env(), {}, "let x = 3")
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "let y = 'abc'");
      })
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "let z = (x, y)");
      })
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run(executor, std::move(env), std::move(bindings), "let (a, b) = z");
      })
      .and_then([&](Tree<Binding> result, Env env, Bindings bindings) {
        return run_to_string(executor, std::move(env), std::move(bindings), "a");
      })
      .and_then([&](std::string result, Env env, Bindings bindings) {
        BOOST_CHECK_EQUAL("3", result);
        return run_to_string(executor, std::move(env), std::move(bindings), "b");
      })
      .map([&](std::string result, Env env, Bindings bindings) {
        BOOST_CHECK_EQUAL("abc", result);
        return std::tuple(std::move(env), std::move(bindings));
      });

  if(!result) {
    fmt::print("{}\n", knot::debug(result.error()));
    BOOST_CHECK(result);
  }
}

BOOST_AUTO_TEST_CASE(print_fn) {
  auto executor = make_task_executor();
  Env e = create_primative_env();
  e.add_function("f", []() { return 1; });

  // TODO improve this error
  BOOST_CHECK(!run_to_string(executor, std::move(e), {}, "f"));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
