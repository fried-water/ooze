#include "pch.h"

#include "overload_resolution.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

namespace {

const TypeID I = anyf::type_id<i32>();
const TypeID F = anyf::type_id<f32>();

template <typename... Ts>
auto errors(Ts... ts) {
  return tl::unexpected{std::vector<std::string>{std::move(ts)...}};
}

} // namespace

BOOST_AUTO_TEST_CASE(overload_resolution_basic) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x; });

  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}, {F, true}}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_undeclared) {
  Env e = create_primative_env();

  BOOST_CHECK(err("use of undeclared function 'f'") == overload_resolution(e, "f", {{I}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_match_return_type) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x) { return std::tuple(x, x); });

  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}, Span<TypeID>{I}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}}, Span<TypeID>{I, I}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_missing_args) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x + y; });

  const auto expected_errors1 = errors("no matching overload found for f() [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for g(i32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  const auto expected_errors3 =
    errors("no matching overload found for g(f32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "g", {{I, true}}));
  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{F, true}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_wrong_args) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x, f32 y) { return x + y; });

  const auto expected_errors1 = errors("no matching overload found for f(f32) [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for g(i32, i32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  const auto expected_errors3 =
    errors("no matching overload found for g(f32, f32) [1 candidate(s)]", "  g(i32, f32) -> f32");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {{F, true}}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "g", {{I, true}, {I, true}}));
  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{F, true}, {F, true}}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_wrong_return) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](i32 x) { return std::tuple(x, x); });

  const auto expected_errors1 =
    errors("no matching overload found for f(i32) -> () [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors2 =
    errors("no matching overload found for f(i32) -> (i32, i32) [1 candidate(s)]", "  f(i32) -> i32");

  const auto expected_errors3 =
    errors("no matching overload found for g(i32) -> () [1 candidate(s)]", "  g(i32) -> (i32, i32)");

  const auto expected_errors4 =
    errors("no matching overload found for g(i32) -> i32 [1 candidate(s)]", "  g(i32) -> (i32, i32)");

  BOOST_CHECK(expected_errors1 == overload_resolution(e, "f", {{I, true}}, Span<TypeID>{}));
  BOOST_CHECK(expected_errors2 == overload_resolution(e, "f", {{I, true}}, Span<TypeID>{I, I}));

  BOOST_CHECK(expected_errors3 == overload_resolution(e, "g", {{I, true}}, Span<TypeID>{}));
  BOOST_CHECK(expected_errors4 == overload_resolution(e, "g", {{I, true}}, Span<TypeID>{I}));
}

BOOST_AUTO_TEST_CASE(overload_resolution_borrow) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("g", [](const i32& x) { return x; });

  const auto expected_errors = errors("no matching overload found for f(i32&) [1 candidate(s)]", "  f(i32) -> i32");

  BOOST_CHECK(expected_errors == overload_resolution(e, "f", {{I}}));
  BOOST_CHECK(overload_resolution(e, "f", {{I, true}}).has_value());

  BOOST_CHECK(overload_resolution(e, "g", {{I}}).has_value());
  BOOST_CHECK(overload_resolution(e, "g", {{I, true}}).has_value());
}

BOOST_AUTO_TEST_CASE(overload_resolution_ambiguous) {
  Env e = create_primative_env();
  e.add_function("f", [](i32 x) { return x; });
  e.add_function("f", [](const i32& x) { return x; });

  const auto expected_errors =
    errors("function call is ambiguous f(i32) [2 candidate(s)]", "  f(i32) -> i32", "  f(i32&) -> i32");

  BOOST_CHECK(expected_errors == overload_resolution(e, "f", {{I, true}}));
  BOOST_CHECK(overload_resolution(e, "f", {{I}}).has_value());
}

} // namespace ooze
