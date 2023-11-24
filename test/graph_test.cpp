#include "test.h"

#include "ooze/graph.h"

namespace ooze {

BOOST_AUTO_TEST_SUITE(graph)

namespace {

std::vector<i32> preorder_to_vec(const Graph<i32>& g, i32 id, const std::vector<i32>& ignore = {}) {
  std::vector<i32> v;
  preorder(g, id, [&](i32 id) {
    v.push_back(id);
    return std::find(ignore.begin(), ignore.end(), id) == ignore.end();
  });
  return v;
}

} // namespace

BOOST_AUTO_TEST_CASE(base) {
  const auto g = Graph<i32>({{1, 2}, {0}, {1}});

  check_range((std::array{1, 2}), g.fanout(0));
  check_range((std::array{0}), g.fanout(1));
  check_range((std::array{1}), g.fanout(2));

  BOOST_CHECK_EQUAL(3, g.num_nodes());
  BOOST_CHECK_EQUAL(4, g.num_edges());
}

BOOST_AUTO_TEST_CASE(preorder_leaf) {
  const auto g = Graph<i32>(std::vector<std::vector<i32>>{{}});
  check_range((std::array{0}), preorder_to_vec(g, 0));
}

BOOST_AUTO_TEST_CASE(preorder_tree) {
  const auto g = Graph<i32>(std::vector<std::vector<i32>>{{1, 3}, {2}, {}, {}});
  check_range((std::array{0, 1, 2, 3}), preorder_to_vec(g, 0));
  check_range((std::array{1, 2}), preorder_to_vec(g, 1));
  check_range((std::array{2}), preorder_to_vec(g, 2));
  check_range((std::array{3}), preorder_to_vec(g, 3));
}

BOOST_AUTO_TEST_CASE(preorder_tree_ignore) {
  const auto g = Graph<i32>(std::vector<std::vector<i32>>{{1, 3}, {2}, {}, {}});
  check_range((std::array{0, 1, 3}), preorder_to_vec(g, 0, {1}));
  check_range((std::array{1}), preorder_to_vec(g, 1, {1}));
  check_range((std::array{2}), preorder_to_vec(g, 2, {1}));
  check_range((std::array{3}), preorder_to_vec(g, 3, {1}));
}

BOOST_AUTO_TEST_CASE(preorder_tree_converge) {
  const auto g = Graph<i32>(std::vector<std::vector<i32>>{{1, 3}, {2}, {}, {2}});
  check_range((std::array{0, 1, 2, 3, 2}), preorder_to_vec(g, 0));
  check_range((std::array{1, 2}), preorder_to_vec(g, 1));
  check_range((std::array{2}), preorder_to_vec(g, 2));
  check_range((std::array{3, 2}), preorder_to_vec(g, 3));
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
