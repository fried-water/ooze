#include "test.h"

#include "async_functions.h"
#include "function_graph_inner.h"

#include "ooze/executor/task_executor.h"

#include <chrono>
#include <thread>
#include <unordered_map>

namespace ooze {

namespace {

FunctionGraph create_graph(int depth) {
  std::unordered_map<std::pair<int, int>, Oterm, knot::Hash> edges;

  const auto identity = create_async_function([](int x) { return x; });
  const auto sum = create_async_function([](const int& x, int y) { return x + y; });

  auto [cg, input] = make_graph({false});
  int num_inputs = 1 << depth;
  for(int i = 0; i < num_inputs; i++) {
    edges.emplace(std::pair(depth, i), cg.add(identity, {input}, {PassBy::Copy}, 1)[0]);
  }

  for(int layer = depth - 1; layer >= 0; layer--) {
    int nodes_on_layer = 1 << layer;
    for(int i = 0; i < nodes_on_layer; i++) {
      edges.emplace(std::pair(layer, i),
                    cg.add(sum,
                           {edges.at(std::pair(layer + 1, i * 2)), edges.at(std::pair(layer + 1, i * 2 + 1))},
                           {PassBy::Borrow, PassBy::Move},
                           1)[0]);
    }
  }

  return std::move(cg).finalize({edges.at(std::pair(0, 0))}, {PassBy::Copy});
}

} // namespace

BOOST_AUTO_TEST_CASE(stress_graph, *boost::unit_test::disabled()) {
  const int depth = 14;
  const int graph_size = ((1 << depth) * 2 - 1);
  const int num_executions = 10;

  fmt::print("Thread count: {}\n", std::thread::hardware_concurrency());
  fmt::print("Creating graph of size {}\n", graph_size);

  auto t0 = std::chrono::steady_clock::now();
  auto g = create_graph(depth);

  fmt::print("Creating graph took {}ms\n",
             std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - t0).count());

  for(int i = 1; i <= std::thread::hardware_concurrency(); i++) {
    auto ex = make_task_executor(i);
    auto t0 = std::chrono::steady_clock::now();
    int result = -1;
    for(int i = 0; i < num_executions; i++) {
      result = any_cast<int>(std::move(create_async_graph(g)(ex, make_vector(Future(1)), {})[0]).wait());
    }

    const auto ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - t0).count();
    fmt::print("R = {} using {} threads {} executions took {}ms ({:0.0f} nodes/s)\n",
               result,
               i,
               num_executions,
               ms,
               1000.0 * graph_size * num_executions / ms);
  }
}

} // namespace ooze
