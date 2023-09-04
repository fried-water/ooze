#include "test.h"

#include "ooze/type.h"

namespace ooze {

namespace {

struct MyType {};

struct ImplicitCopyOnlyType {
  ImplicitCopyOnlyType(const ImplicitCopyOnlyType&) {}
  // Implicitly deleted
  // ImplicitCopyOnlyType(ImplicitCopyOnlyType&&) = delete;
};

struct MoveOnlyType {
  MoveOnlyType(const MoveOnlyType&) = delete;
  MoveOnlyType(MoveOnlyType&&) = default;
};

} // namespace

BOOST_AUTO_TEST_SUITE(type)

BOOST_AUTO_TEST_CASE(is_copy_constructible) {
  BOOST_CHECK_EQUAL(true, is_copyable(type_id(knot::Type<MyType>())));
  BOOST_CHECK_EQUAL(true, is_copyable(type_id(knot::Type<ImplicitCopyOnlyType>())));
  BOOST_CHECK_EQUAL(false, is_copyable(type_id(knot::Type<MoveOnlyType>())));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
