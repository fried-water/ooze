#include "test.h"

#include "ooze/any.h"

namespace ooze {

namespace {

struct MoveOnlyType {
  MoveOnlyType() = default;
  MoveOnlyType(const MoveOnlyType&) = delete;
  MoveOnlyType(MoveOnlyType&&) = default;
  friend bool operator==(const MoveOnlyType&, const MoveOnlyType&) { return true; }
};

} // namespace

BOOST_AUTO_TEST_SUITE(any)

BOOST_AUTO_TEST_CASE(basic) {
  Any any;

  BOOST_CHECK(!any.has_value());

  any = 5;

  BOOST_CHECK(any.has_value());
  BOOST_CHECK(type_id(knot::Type<int>{}) == any.type());
  BOOST_CHECK(5 == any_cast<int>(any));

  any = std::string("abc");

  BOOST_CHECK(any.has_value());
  BOOST_CHECK(type_id(knot::Type<std::string>{}) == any.type());
  BOOST_CHECK("abc" == any_cast<std::string>(any));

  const Any any2 = any;

  BOOST_CHECK(any2.has_value());
  BOOST_CHECK(type_id(knot::Type<std::string>{}) == any2.type());
  BOOST_CHECK("abc" == any_cast<std::string>(any2));

  any = {};

  BOOST_CHECK(!any.has_value());

  any = any2;

  BOOST_CHECK(any.has_value());
  BOOST_CHECK(type_id(knot::Type<std::string>{}) == any.type());
  BOOST_CHECK("abc" == any_cast<std::string>(any));
}

BOOST_AUTO_TEST_CASE(move_only) {
  Any any = MoveOnlyType();

  BOOST_CHECK(any.has_value());
  BOOST_CHECK(type_id(knot::Type<MoveOnlyType>{}) == any.type());
  BOOST_CHECK(MoveOnlyType() == any_cast<MoveOnlyType>(any));

  BOOST_CHECK_THROW((Any(any)), BadCopy);

  Any any2 = std::move(any);

  BOOST_CHECK(!any.has_value());
  BOOST_CHECK(any2.has_value());
  BOOST_CHECK(type_id(knot::Type<MoveOnlyType>{}) == any2.type());
  BOOST_CHECK(MoveOnlyType() == any_cast<MoveOnlyType>(any2));

  const MoveOnlyType m = any_cast<MoveOnlyType>(std::move(any2));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
