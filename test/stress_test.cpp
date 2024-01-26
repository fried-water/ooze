#include "test.h"

#include "constructing_graph.h"
#include "runtime.h"

#include "ooze/executor/tbb_executor.h"

#include <chrono>
#include <thread>
#include <unordered_map>

namespace ooze {

namespace {

std::pair<Inst, Program> create_graph(int depth) {
  Program program;

  std::unordered_map<std::pair<int, int>, Oterm, knot::Hash> edges;

  const Inst identity = program.add([](int x) { return x; });
  const Inst sum = program.add([](const int& x, int y) { return x + y; });

  auto [cg, input] = make_graph({false});
  const int num_inputs = 1 << depth;
  for(int i = 0; i < num_inputs; i++) {
    edges.emplace(std::pair(depth, i), cg.add(identity, {input}, {PassBy::Copy}, 1)[0]);
  }

  for(int layer = depth - 1; layer >= 0; layer--) {
    const int nodes_on_layer = 1 << layer;
    for(int i = 0; i < nodes_on_layer; i++) {
      edges.emplace(std::pair(layer, i),
                    cg.add(sum,
                           {edges.at(std::pair(layer + 1, i * 2)), edges.at(std::pair(layer + 1, i * 2 + 1))},
                           {PassBy::Borrow, PassBy::Move},
                           1)[0]);
    }
  }

  const Inst g = program.add(std::move(cg).finalize({edges.at(std::pair(0, 0))}, {PassBy::Copy}));
  return {g, std::move(program)};
}

} // namespace

BOOST_AUTO_TEST_CASE(stress_graph, *boost::unit_test::disabled()) {
  const int depth = 14;
  const int graph_size = ((1 << depth) * 2 - 1);
  const int num_executions = 10;

  fmt::print("Thread count: {}\n", std::thread::hardware_concurrency());
  fmt::print("Creating graph of size {}\n", graph_size);

  auto t0 = std::chrono::steady_clock::now();
  auto [inst, program] = create_graph(depth);

  auto shared_prog = std::make_shared<const Program>(std::move(program));

  fmt::print("Creating graph took {}ms\n",
             std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - t0).count());

  for(int i = 1; i <= std::thread::hardware_concurrency(); i++) {
    auto t0 = std::chrono::steady_clock::now();
    int result = -1;
    for(int c = 0; c < num_executions; c++) {
      auto ex = make_tbb_executor(i);
      std::move(execute(shared_prog, inst, ex, make_vector(Future(1)), {})[0]).then([&](Any a) {
        result = any_cast<i32>(a);
      });
    }

    const auto ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - t0).count();
    fmt::print("R = {} using {} threads {} executions took {}ms ({:0.0f} nodes/s)\n",
               result,
               i,
               num_executions,
               ms,
               1000.0 * graph_size * num_executions / double(ms));
  }
}

} // namespace ooze
