#include "test.h"

#include "ooze/executor/task_executor.h"
#include "pretty_print.h"
#include "repl.h"

namespace ooze {

namespace {

#define step_and_compare(_EXP, _STR, _ENV, _BND)                                                                       \
  [&](const std::vector<std::string>& e, std::string_view str, Env env, Bindings b) {                                  \
    auto [a, e1, b1] = step_repl(executor, std::move(env), std::move(b), str);                                         \
    if(e != a) {                                                                                                       \
      fmt::print("E: {}\n", knot::debug(e));                                                                           \
      fmt::print("A: {}\n", knot::debug(a));                                                                           \
    }                                                                                                                  \
    check_range(e, a);                                                                                                 \
    return std::tuple(std::move(e1), std::move(b1));                                                                   \
  }(_EXP, _STR, std::move(_ENV), std::move(_BND))

} // namespace

BOOST_AUTO_TEST_SUITE(repl)

BOOST_AUTO_TEST_CASE(empty) {
  auto executor = make_task_executor();
  const auto [ouptut, env, bindings] = step_repl(executor, {}, {}, "");
  BOOST_CHECK(ouptut.empty());
  BOOST_CHECK(bindings.empty());
}

BOOST_AUTO_TEST_CASE(run_expr) {
  auto executor = make_task_executor();
  Env e = create_primative_env();
  e.add_function("pow", [](int x) { return x * x; });

  Bindings b;
  std::tie(e, b) = step_and_compare({"3"}, "3", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"abc"}, "'abc'", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"9"}, "pow(3)", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(pass_binding_by_value) {
  auto executor = make_task_executor();
  Env e = create_primative_env();
  e.add_type<std::unique_ptr<int>>("unique_int");

  e.add_function("make_ptr", [](int x) { return std::make_unique<int>(x); });
  e.add_function("take_ptr", [](std::unique_ptr<int> x) { return *x; });

  Bindings b;
  std::tie(e, b) = step_and_compare({}, "let x = make_ptr(5)", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"5"}, "take_ptr(x)", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(store_function) {
  auto executor = make_task_executor();
  Env e = create_primative_env();

  e.add_function("f", []() { return 37; });

  Bindings b;
  std::tie(e, b) = step_and_compare({}, "let x = f", std::move(e), std::move(b));

  BOOST_REQUIRE_EQUAL(1, b.size());
  Binding& x = std::get<Binding>(b.at("x").v);
  BOOST_CHECK(function_type(tuple_type<TypeID>({}), leaf_type(type_id(knot::Type<int>{}))) == x.type);

  std::tie(e, b) = step_and_compare({"37"}, "x()", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(bindings) {
  auto executor = make_task_executor();
  Env e;
  Bindings b;

  std::tie(e, b) = step_and_compare({"0 binding(s)"}, ":b", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, "let x =  5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  const std::vector<std::string> one_unknown_binding{
    "1 binding(s)", fmt::format("  x: type 0x{:x}", type_id(knot::Type<int>{}).id)};

  std::tie(e, b) = step_and_compare(one_unknown_binding, ":b", std::move(e), std::move(b));

  std::tie(e, b) = step_and_compare({}, "let y =  'abc'", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({}, ":a", std::move(e), std::move(b));

  e.add_type<int>("i32");

  const std::vector<std::string> two_bindings{
    "2 binding(s)", "  x: i32", fmt::format("  y: {}", pretty_print(e, type_id(knot::Type<std::string>{})))};

  std::tie(e, b) = step_and_compare(two_bindings, ":b", std::move(e), std::move(b));
  BOOST_REQUIRE(b.size() == 2);

  Binding& x = std::get<Binding>(b.at("x").v);
  Binding& y = std::get<Binding>(b.at("y").v);

  BOOST_CHECK(leaf_type(type_id(knot::Type<int>{})) == x.type);
  BOOST_CHECK(leaf_type(type_id(knot::Type<std::string>{})) == y.type);

  BOOST_CHECK_EQUAL(5, any_cast<int>(std::move(x.future).wait()));
  BOOST_CHECK_EQUAL("abc", any_cast<std::string>(std::move(y.future).wait()));
}

BOOST_AUTO_TEST_CASE(binding_overload_fn) {
  auto executor = make_task_executor();
  Env e = create_primative_env();

  e.add_function("f", []() { return 1; });

  Bindings b;
  std::tie(e, b) = step_and_compare({}, "let f = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"5"}, "f", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(bindings_post_dump) {
  auto executor = make_task_executor();
  Env e = create_primative_env();
  Bindings b;

  std::tie(e, b) = step_and_compare({}, "let x = 5", std::move(e), std::move(b));
  std::tie(e, b) = step_and_compare({"5"}, "x", std::move(e), std::move(b));

  const std::vector<std::string> expected{"1 binding(s)", fmt::format("  x: i32")};
  step_and_compare(expected, ":b", std::move(e), std::move(b));
}

BOOST_AUTO_TEST_CASE(types) {
  struct A {};
  struct B {};

  auto executor = make_task_executor();
  Env e;

  add_tieable_type<int>(e, "i32");

  e.add_type<A>("A");

  // Types without names won't be reported
  e.add_function("to_string", [](const B&) { return std::string("B"); });

  const std::vector<std::string> expected{
    "2 type(s)", "  A                    [to_string: N]", "  i32                  [to_string: Y]"};

  step_and_compare(expected, ":t", std::move(e), Bindings{});
}

BOOST_AUTO_TEST_CASE(functions) {
  auto executor = make_task_executor();
  Env e = create_primative_env();

  struct A {};

  const auto a_type = type_id(knot::Type<A>{});

  e.add_function("create_a", []() { return A{}; });
  e.add_function("read_a", [](const A&) {});
  e.add_function("take_a", [](A) {});
  e.add_function("pow", [](int x) { return x * x; });
  e.add_function("concat", [](const std::string& a, const std::string& b) { return a + b; });

  const std::vector<std::string> expected{
    "9 function(s)",
    "  clone [13 overloads]",
    "  to_string [12 overloads]",
    "  serialize [12 overloads]",
    "  deserialize [12 overloads]",
    "  concat(&string, &string) -> string",
    fmt::format("  create_a() -> {}", pretty_print(e, a_type)),
    "  pow(i32) -> i32",
    "  read(&string) -> string",
    "  read(&string) -> vector<byte>",
    fmt::format("  read_a(&{}) -> ()", pretty_print(e, a_type)),
    fmt::format("  take_a({}) -> ()", pretty_print(e, a_type)),
    "  write(&string, &string) -> ()",
    "  write(&string, &vector<byte>) -> ()"};

  step_and_compare(expected, ":f", std::move(e), Bindings{});
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
