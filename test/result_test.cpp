#include "test.h"

#include "ooze/result.h"

namespace ooze {

namespace {
struct State {
  int data = 0;
  KNOT_ORDERED(State);
};
} // namespace

BOOST_AUTO_TEST_CASE(result_trivial) {
  Result<int, std::string> r = 5;
  BOOST_CHECK(r.has_value());
  BOOST_CHECK_EQUAL(5, r.value());
  BOOST_CHECK(std::tuple() == r.state());

  r = fail(knot::Type<int>{}, std::string("abc"));
  BOOST_CHECK(!r.has_value());
  BOOST_CHECK_EQUAL("abc", r.error());
  BOOST_CHECK(std::tuple() == r.state());
}

BOOST_AUTO_TEST_CASE(result_trivial_void) {
  Result<void, std::string> r;
  BOOST_CHECK(r.has_value());
  BOOST_CHECK(std::tuple() == r.state());

  r = fail<void>(std::string("abc"));
  BOOST_CHECK(!r.has_value());
  BOOST_CHECK_EQUAL("abc", r.error());
  BOOST_CHECK(std::tuple() == r.state());
}

BOOST_AUTO_TEST_CASE(result_trivial_state) {
  Result<int, std::string, State> r = {1, State{2}};
  BOOST_CHECK(r.has_value());
  BOOST_CHECK_EQUAL(1, r.value());
  BOOST_CHECK(std::tuple(State{2}) == r.state());

  r = fail<int>(std::string("abc"), State{3});
  BOOST_CHECK(!r.has_value());
  BOOST_CHECK_EQUAL("abc", r.error());
  BOOST_CHECK(std::tuple(State{3}) == r.state());
}

BOOST_AUTO_TEST_CASE(result_trivial_state_void) {
  Result<void, std::string, State> r = {{}, State{2}};
  BOOST_CHECK(r.has_value());
  BOOST_CHECK(std::tuple(State{2}) == r.state());

  r = fail(knot::Type<void>{}, std::string("abc"), State{3});
  BOOST_CHECK(!r.has_value());
  BOOST_CHECK_EQUAL("abc", r.error());
  BOOST_CHECK(std::tuple(State{3}) == r.state());
}

BOOST_AUTO_TEST_CASE(result_and_then_pass) {
  const auto r = success<std::string>(5).and_then([](int x) { return success<std::string>(float(x)); });
  BOOST_CHECK((success<std::string>(5.0f) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_fail) {
  const auto r = success<std::string>(5).and_then([](int) { return fail<float>(std::string("abc")); });
  BOOST_CHECK((fail<float>(std::string("abc")) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_void_pass) {
  const auto r = success<std::string>().and_then([]() { return success<std::string>(); });
  BOOST_CHECK((success<std::string>() == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_void_fail) {
  const auto r = success<std::string>().and_then([]() { return fail<void>(std::string("abc")); });
  BOOST_CHECK((fail<void>(std::string("abc")) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_pass_state) {
  const auto r = success<std::string>(5, State{3}).and_then([](int x, State s) {
    return success<std::string>(float(x), std::move(s));
  });
  BOOST_CHECK((success<std::string>(5.0f, State{3}) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_fail_state) {
  const auto r = success<std::string>(5, State{1}).and_then([](int, State s) {
    return fail<float>(std::string("abc"), std::move(s));
  });
  BOOST_CHECK((fail<float>(std::string("abc"), State{1}) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_to_void) {
  const auto r = success<std::string>(1).and_then([](int) { return success<std::string>(); });
  BOOST_CHECK((success<std::string>() == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_from_void) {
  const auto r = success<std::string>().and_then([]() { return success<std::string>(1); });
  BOOST_CHECK((success<std::string>(1) == r));
}

BOOST_AUTO_TEST_CASE(result_and_then_skip) {
  const auto r = fail<int>(std::string("abc")).and_then([](int x) {
    BOOST_CHECK(false);
    return success<std::string>(float(x));
  });
  BOOST_CHECK((fail<int>(std::string("abc")) == r));
}

BOOST_AUTO_TEST_CASE(result_map) {
  const auto r = success<std::string>(5).map([](int x) { return float(x); });
  BOOST_CHECK((success<std::string>(5.0f) == r));
}

BOOST_AUTO_TEST_CASE(result_map_void) {
  const auto r = success<std::string>().map([]() {});
  BOOST_CHECK((success<std::string>() == r));
}

BOOST_AUTO_TEST_CASE(result_map_state) {
  const auto r =
    success<std::string>(5, State{3}).map([](int x, State s) { return std::tuple(float(x), std::move(s)); });
  BOOST_CHECK((success<std::string>(5.0f, State{3}) == r));
}

BOOST_AUTO_TEST_CASE(result_map_void_state) {
  const auto r = Result<void, std::string, State>({}, State{3}).map([](State s) { return s; });
  BOOST_CHECK((success<std::string>(State{3}) == r));
}

BOOST_AUTO_TEST_CASE(result_map_to_void) {
  const auto r = success<std::string>(1).map([](int) {});
  BOOST_CHECK((success<std::string>() == r));
}

BOOST_AUTO_TEST_CASE(result_map_from_void) {
  const auto r = success<std::string>().map([]() { return 1; });
  BOOST_CHECK((success<std::string>(1) == r));
}

BOOST_AUTO_TEST_CASE(result_map_skip) {
  const auto r = fail<int>(std::string("abc")).map([](int x) { BOOST_CHECK(false); });
  BOOST_CHECK((fail<void>(std::string("abc")) == r));
}

// TODO map_error() tests

BOOST_AUTO_TEST_CASE(result_map_error) {
  const auto r = fail<float>(5).map_error([](int x) { return std::to_string(x); });
  BOOST_CHECK((fail<float>(std::string("5")) == r));
}

BOOST_AUTO_TEST_CASE(result_map_error_void) {
  const auto r = fail<void>(5).map_error([](int x) { return std::to_string(x); });
  BOOST_CHECK((fail<void>(std::string("5")) == r));
}

BOOST_AUTO_TEST_CASE(result_map_error_state) {
  const auto r = fail<float>(5, State{1}).map_error([](int x) { return std::to_string(x); });
  BOOST_CHECK((fail<float>(std::string("5"), State{1}) == r));
}

BOOST_AUTO_TEST_CASE(result_map_error_void_state) {
  const auto r = fail<void>(5, State{1}).map_error([](int x) { return std::to_string(x); });
  BOOST_CHECK((fail<void>(std::string("5"), State{1}) == r));
}

BOOST_AUTO_TEST_CASE(result_map_error_skip) {
  const auto r = success<std::string>(5).map_error([](std::string x) -> float {
    BOOST_CHECK(false);
    return 1.0f;
  });
  BOOST_CHECK((success<float>(5) == r));
}

// TODO allow Result<T, T>

} // namespace ooze
