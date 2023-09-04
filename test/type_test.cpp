#include "test.h"

#include "ooze/type.h"

namespace ooze {

namespace my_namespace {
struct MyType {};
} // namespace my_namespace

namespace {

struct MyType {};

struct ImplicitCopyOnlyType {
  ImplicitCopyOnlyType(ImplicitCopyOnlyType const&) {}
  // Implicitly deleted
  // ImplicitCopyOnlyType(ImplicitCopyOnlyType&&) = delete;
};

struct MoveOnlyType {
  MoveOnlyType(MoveOnlyType const&) = delete;
  MoveOnlyType(MoveOnlyType&&) = default;
};

} // namespace

BOOST_AUTO_TEST_SUITE(type)

BOOST_AUTO_TEST_CASE(is_copy_constructible) {
  BOOST_CHECK_EQUAL(true, is_copyable(type_id(Type<MyType>())));
  BOOST_CHECK_EQUAL(true, is_copyable(type_id(Type<ImplicitCopyOnlyType>())));
  BOOST_CHECK_EQUAL(false, is_copyable(type_id(Type<MoveOnlyType>())));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
