#include "test.h"

#include "bindings.h"
#include "pretty_print.h"
#include "repl.h"
#include "runtime_test.h"

#include "ooze/executor/sequential_executor.h"

namespace ooze {

namespace {

const TypeID I = type_id(knot::Type<i32>{});

#define step_and_compare(_EXP, _STR, _ENV)                                                                             \
  [&](const std::vector<std::string>& e, std::string_view str, Env env) {                                              \
    auto [future, e1] = step_repl(make_seq_executor(), std::move(env), str);                                           \
    check_any(e, await(std::move(future)));                                                                            \
    return std::move(e1);                                                                                              \
  }(_EXP, _STR, _ENV)

} // namespace

BOOST_AUTO_TEST_SUITE(repl)

BOOST_AUTO_TEST_CASE(empty) {
  auto [future, env] = step_repl(make_seq_executor(), {}, "");
  check_any(std::vector<std::string>(), await(std::move(future)));
  BOOST_CHECK(env.bindings().empty());
}

BOOST_AUTO_TEST_CASE(run_expr) {
  Env e = Env(create_primitive_registry().add_fn("pow", [](int x) { return x * x; }));

  e = step_and_compare({"3"}, "3", std::move(e));
  e = step_and_compare({"abc"}, "'abc'", std::move(e));
  e = step_and_compare({"9"}, "pow(3)", std::move(e));
}

BOOST_AUTO_TEST_CASE(move_only_binding) {
  Env e = Env(create_primitive_registry()
                .add_type<std::unique_ptr<int>>("unique_int")
                .add_fn("make_ptr", [](int x) { return std::make_unique<int>(x); })
                .add_fn("take_ptr", [](std::unique_ptr<int> x) { return *x; }));

  e = step_and_compare({}, "let x = make_ptr(5)", std::move(e));
  e = step_and_compare({"5"}, "take_ptr(x)", std::move(e));
}

BOOST_AUTO_TEST_CASE(store_env_function) {
  Env e = Env(create_primitive_registry().add_fn("f", []() { return 37; }));

  e = step_and_compare({}, "let x = f", std::move(e));
  e = step_and_compare({"37"}, "x()", std::move(e));
}

BOOST_AUTO_TEST_CASE(store_script_function) {
  Env e = check_result(Env(create_primitive_registry()).parse_scripts(make_sv_array("fn f() -> i32 = 37")));

  e = step_and_compare({}, "let x = f", std::move(e));
  e = step_and_compare({"37"}, "x()", std::move(e));
}

BOOST_AUTO_TEST_CASE(no_bindings) { step_and_compare({"0 binding(s)"}, ":b", Env{}); }

BOOST_AUTO_TEST_CASE(single_binding) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));
  e = step_and_compare({}, "let x = 5", std::move(e));

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(multi_binding) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32").add_type<std::string>("string"));

  e = step_and_compare({}, "let x = 5", std::move(e));
  e = step_and_compare({}, "let y = 'abc'", std::move(e));

  const std::vector<std::string> exp{"2 binding(s)", "  x: i32", "  y: string"};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(tuple_binding) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32").add_type<std::string>("string"));

  e = step_and_compare({}, "let x = (5, 'abc')", std::move(e));

  const std::vector<std::string> exp{"1 binding(s)", "  x: (i32, string)"};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(unnamed_binding) {
  Env e;
  e = step_and_compare({}, "let x = 5", std::move(e));

  const std::vector<std::string> exp{"1 binding(s)", fmt::format("  x: type 0x{:x}", I.id)};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(binding_not_ready) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));

  auto [promise, future] = make_promise_future();

  e.insert("x", std::move(future), type_id<i32>());

  const std::vector<std::string> exp_not_ready{"1 binding(s)", "  x: *i32"};
  e = step_and_compare(exp_not_ready, ":b", std::move(e));

  std::move(promise).send(Any(0));

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(binding_borrowed) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));

  Binding b = {check_result(e.parse_type("i32")), make_vector(AsyncValue{Future{Any{0}}})};

  auto borrowed = borrow(b.values[0]);

  e.insert("x", std::move(b));

  const std::vector<std::string> exp_borrowed{"1 binding(s)", "  x: &i32"};
  e = step_and_compare(exp_borrowed, ":b", std::move(e));

  borrowed = {};

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(bindings_post_dump) {
  Env e = Env(create_primitive_registry());

  e = step_and_compare({}, "let x = 5", std::move(e));
  e = step_and_compare({"5"}, "x", std::move(e));

  const std::vector<std::string> expected{"1 binding(s)", fmt::format("  x: i32")};
  step_and_compare(expected, ":b", std::move(e));
}

BOOST_AUTO_TEST_CASE(no_to_string, *boost::unit_test::disabled()) {
  Env e = Env(NativeRegistry{}.add_type<i32>("i32"));

  // TODO add some unique identifier (ptr? how should tuples be handled?)
  const std::vector<std::string> exp{"<value of i32>"};
  step_and_compare(exp, "1", std::move(e));
}

BOOST_AUTO_TEST_CASE(types) {
  struct A {};
  struct B {};

  Env e = Env(NativeRegistry{}
                .add_type<i32>("i32")
                .add_type<std::string>("string")
                .add_type<A>("A")
                .add_fn("to_string", [](const i32& x) { return fmt::format("{}", x); })
                .add_fn("to_string", [](const B&) { return std::string("B"); }));

  // Types without names won't be reported
  const std::vector<std::string> expected{
    "3 type(s)",
    "  A                    [to_string: N]",
    "  i32                  [to_string: Y]",
    "  string               [to_string: N]"};

  step_and_compare(expected, ":t", std::move(e));
}

BOOST_AUTO_TEST_CASE(functions) {
  struct A {};

  const auto a_type = type_id(knot::Type<A>{});

  Env e = Env(
    NativeRegistry{}
      .add_type<bool>("bool")
      .add_type<i8>("i8")
      .add_type<i16>("i16")
      .add_type<i32>("i32")
      .add_type<i64>("i64")
      .add_type<f32>("f32")
      .add_type<std::string>("string")
      .add_fn("create_a", []() { return A{}; })
      .add_fn("read_a", [](const A&) {})
      .add_fn("take_a", [](A) {})
      .add_fn("pow", [](int x) { return x * x; })
      .add_fn("concat", [](const std::string& a, const std::string& b) { return a + b; }));

  const std::vector<std::string> expected{
    "12 function(s)",
    "  clone [7 overloads]",
    "  concat: fn(&string, &string) -> string",
    fmt::format("  create_a: fn() -> {}", pretty_print(e.native_types().names, a_type)),
    "  pow: fn(i32) -> i32",
    fmt::format("  read_a: fn(&{}) -> ()", pretty_print(e.native_types().names, a_type)),
    fmt::format("  take_a: fn({}) -> ()", pretty_print(e.native_types().names, a_type))};

  step_and_compare(expected, ":f", std::move(e));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
