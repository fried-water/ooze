#include "test.h"

#include "bindings.h"
#include "pretty_print.h"
#include "repl.h"

#include "ooze/executor/sequential_executor.h"

namespace ooze {

namespace {

const TypeID I = type_id(knot::Type<i32>{});

#define step_and_compare(_EXP, _STR, _ENV, _BND)                                                                       \
  [&](const std::vector<std::string>& e, std::string_view str, Env env, Bindings b) {                                  \
    auto [a, e1, b1] = step_repl(make_seq_executor(), std::move(env), std::move(b), str);                              \
    if(e != a) {                                                                                                       \
      fmt::print("E: {}\n", knot::debug(e));                                                                           \
      fmt::print("A: {}\n", knot::debug(a));                                                                           \
    }                                                                                                                  \
    check_range(e, a);                                                                                                 \
    return std::tuple(std::move(e1), std::move(b1));                                                                   \
  }(_EXP, _STR, _ENV, _BND)

} // namespace

BOOST_AUTO_TEST_SUITE(repl)

BOOST_AUTO_TEST_CASE(empty) {
  const auto [ouptut, env, bindings] = step_repl(make_seq_executor(), {}, {}, "");
  BOOST_CHECK(ouptut.empty());
  BOOST_CHECK(bindings.empty());
}

BOOST_AUTO_TEST_CASE(run_expr) {
  Env e = create_primative_env();
  e.add_function("pow", [](int x) { return x * x; });

  Bindings b;
  std::tie(e, b) = step_and_compare({"3"}, "3", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"abc"}, "'abc'", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"9"}, "pow(3)", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(move_only_binding) {
  Env e = create_primative_env();
  e.add_type<std::unique_ptr<int>>("unique_int");

  e.add_function("make_ptr", [](int x) { return std::make_unique<int>(x); });
  e.add_function("take_ptr", [](std::unique_ptr<int> x) { return *x; });

  Bindings b;
  std::tie(e, b) = step_and_compare({}, "let x = make_ptr(5)", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"5"}, "take_ptr(x)", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(store_env_function) {
  Env e = create_primative_env();

  e.add_function("f", []() { return 37; });

  Bindings b;
  std::tie(e, b) = step_and_compare({}, "let x = f", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"37"}, "x()", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(store_script_function) {
  Env e = check_result(parse_scripts(create_primative_env(), make_sv_array("fn f() -> i32 = 37")));
  Bindings b;

  std::tie(e, b) = step_and_compare({}, "let x = f", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"37"}, "x()", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(no_bindings) { step_and_compare({"0 binding(s)"}, ":b", Env{}, Bindings{}); }

BOOST_AUTO_TEST_CASE(single_binding) {
  Env e;
  Bindings b;

  e.add_type<i32>("i32");

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(multi_binding) {
  Env e;
  Bindings b;

  e.add_type<i32>("i32");
  e.add_type<std::string>("string");

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, "let y = 'abc'", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  const std::vector<std::string> exp{"2 binding(s)", "  x: i32", "  y: string"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(tuple_binding) {
  Env e;
  Bindings b;

  e.add_type<i32>("i32");
  e.add_type<std::string>("string");

  std::tie(e, b) = step_and_compare({}, "let x = (5, 'abc')", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  const std::vector<std::string> exp{"1 binding(s)", "  x: (i32, string)"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(unnamed_binding) {
  Env e;
  Bindings b;

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  const std::vector<std::string> exp{"1 binding(s)", fmt::format("  x: type 0x{:x}", I.id)};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(post_named_binding) {
  Env e;
  Bindings b;

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  e.add_type<i32>("i32");

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(binding_not_ready) {
  Env e;
  e.add_type<i32>("i32");

  auto [promise, future] = make_promise_future();

  Bindings b;
  b.emplace("x", Binding{e.tg.add_node(TypeTag::Leaf, I), make_vector(AsyncValue{std::move(future)})});

  const std::vector<std::string> exp_not_ready{"1 binding(s)", "  x: *i32"};
  std::tie(e, b) = step_and_compare(exp_not_ready, ":b", std::move(e), std::move(b));

  std::move(promise).send(0);

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(binding_borrowed) {
  Env e;
  e.add_type<i32>("i32");

  Bindings b;
  b.emplace("x", Binding{e.tg.add_node(TypeTag::Leaf, I), make_vector(AsyncValue{Future(Any(0))})});

  auto borrowed = borrow(b["x"].values[0]);

  const std::vector<std::string> exp_borrowed{"1 binding(s)", "  x: &i32"};
  std::tie(e, b) = step_and_compare(exp_borrowed, ":b", std::move(e), std::move(b));

  borrowed = {};

  const std::vector<std::string> exp{"1 binding(s)", "  x: i32"};
  step_and_compare(exp, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(bindings_post_dump) {
  Env e = create_primative_env();
  Bindings b;

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"5"}, "x", std::move(e), std::move(b));

  const std::vector<std::string> expected{"1 binding(s)", fmt::format("  x: i32")};
  step_and_compare(expected, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(no_to_string, *boost::unit_test::disabled()) {
  Env e;
  e.add_type<i32>("i32");

  // TODO add some unique identifier (ptr? how should tuples be handled?)
  const std::vector<std::string> exp{"<value of i32>"};
  step_and_compare(exp, "1", std::move(e), Bindings{});
}

BOOST_AUTO_TEST_CASE(types) {
  struct A {};
  struct B {};

  Env e;
  e.add_type<std::string>("string");

  add_tieable_type<int>(e, "i32");

  e.add_type<A>("A");

  // Types without names won't be reported
  e.add_function("to_string", [](const B&) { return std::string("B"); });

  const std::vector<std::string> expected{
    "3 type(s)",
    "  A                    [to_string: N]",
    "  i32                  [to_string: Y]",
    "  string               [to_string: N]"};

  step_and_compare(expected, ":t", std::move(e), Bindings{});
}

BOOST_AUTO_TEST_CASE(functions) {
  Env e = create_primative_env();

  struct A {};

  const auto a_type = type_id(knot::Type<A>{});

  e.add_function("create_a", []() { return A{}; });
  e.add_function("read_a", [](const A&) {});
  e.add_function("take_a", [](A) {});
  e.add_function("pow", [](int x) { return x * x; });
  e.add_function("concat", [](const std::string& a, const std::string& b) { return a + b; });

  const std::vector<std::string> expected{
    "60 function(s)",
    "  clone [14 overloads]",
    "  concat(&string, &string) -> string",
    fmt::format("  create_a() -> {}", pretty_print(e.native_types.names, a_type)),
    "  deserialize [12 overloads]",
    "  pow(i32) -> i32",
    "  println(&string) -> ()",
    "  read(&string) -> byte_vector",
    "  read(&string) -> string",
    fmt::format("  read_a(&{}) -> ()", pretty_print(e.native_types.names, a_type)),
    "  serialize [12 overloads]",
    fmt::format("  take_a({}) -> ()", pretty_print(e.native_types.names, a_type)),
    "  to_string [12 overloads]",
    "  write(&string, &byte_vector) -> ()",
    "  write(&string, &string) -> ()"};

  step_and_compare(expected, ":f", std::move(e), Bindings{});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
