#include "test.h"

#include "ooze/graph.h"

namespace ooze {

BOOST_AUTO_TEST_SUITE(graph)

BOOST_AUTO_TEST_CASE(base) {
  const auto g = Graph<i32>({{1, 2}, {0}, {1}});

  check_range((std::array{1, 2}), g.fanout(0));
  check_range((std::array{0}), g.fanout(1));
  check_range((std::array{1}), g.fanout(2));

  BOOST_CHECK_EQUAL(3, g.num_nodes());
  BOOST_CHECK_EQUAL(4, g.num_edges());
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
