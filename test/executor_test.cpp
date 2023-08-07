#include "test.h"

#include "graph_execution.h"
#include "graph_inner.h"
#include "ooze/executor/sequential_executor.h"
#include "ooze/executor/task_executor.h"
#include "ooze/executor/tbb_executor.h"

#include <algorithm>
#include <chrono>
#include <numeric>
#include <random>

namespace ooze {

namespace {

template <typename Out, typename In>
Out execute_siso_graph(FunctionGraph g, ExecutorRef e, In&& input) {
  std::vector<Future> results = execute_graph(g, e, make_vector(Future(e, Any(std::move(input)))), {});
  BOOST_REQUIRE_EQUAL(1, results.size());
  return any_cast<Out>(std::move(results.front()).wait());
}

std::vector<int> create_vector(int size) {
  std::mt19937 rng;
  std::uniform_int_distribution<> dist(0, 100'000);
  std::vector<int> result;
  result.reserve(size);
  for(int i = 0; i < size; i++) {
    result.push_back(dist(rng));
  }

  return result;
}

auto create_shuffle(int seed) {
  return [seed](std::vector<int> vec) {
    std::shuffle(vec.begin(), vec.end(), std::mt19937(seed));
    return vec;
  };
}

i64 accumulate(const std::vector<int>& v) { return std::accumulate(v.begin(), v.end(), i64(0)); }

auto create_pipeline(int seed) {
  auto [cg, size] = make_graph(make_type_properties(TypeList<int>{}));
  const auto create_output = cg.add(AnyFunction(create_vector), size);
  const auto shuffle_output = cg.add(AnyFunction(create_shuffle(seed)), create_output);
  const auto sort_output = cg.add(AnyFunction([](std::vector<int> v) { return sorted(std::move(v)); }), shuffle_output);
  const auto acc_output = cg.add(AnyFunction(accumulate), sort_output);
  return std::move(cg).finalize(acc_output);
}

auto create_graph() {
  auto [cg, size] = make_graph(make_type_properties(TypeList<int>{}));

  const std::array ps = {
    cg.add(create_pipeline(0), size)[0],
    cg.add(create_pipeline(1), size)[0],
    cg.add(create_pipeline(2), size)[0],
    cg.add(create_pipeline(3), size)[0],
    cg.add(create_pipeline(4), size)[0],
    cg.add(create_pipeline(5), size)[0],
    cg.add(create_pipeline(6), size)[0],
    cg.add(create_pipeline(7), size)[0]};

  const auto sumf = AnyFunction([](i64 x, i64 y) { return x + y; });

  const auto o1 = cg.add(sumf, {ps[0], ps[1]})[0];
  const auto o2 = cg.add(sumf, {ps[2], ps[3]})[0];
  const auto o3 = cg.add(sumf, {ps[4], ps[5]})[0];
  const auto o4 = cg.add(sumf, {ps[6], ps[7]})[0];

  const auto o5 = cg.add(sumf, {o1, o2})[0];
  const auto o6 = cg.add(sumf, {o3, o4})[0];

  return std::move(cg).finalize(cg.add(sumf, {o5, o6}));
}

template <typename MakeExecutor>
void execute_graph_with_threads(FunctionGraph g, MakeExecutor f) {
  const int size = 500'000;
  const int max_threads = 8;

  for(int num_threads = 1; num_threads <= max_threads; num_threads++) {
    Executor e = f(num_threads);
    const auto t0 = std::chrono::steady_clock::now();
    const i64 result = execute_siso_graph<i64>(g, e, size);
    const auto t1 = std::chrono::steady_clock::now();
    fmt::print("{} THREADS: result is {} after {}us\n",
               num_threads,
               result,
               std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count());
  }
}

} // namespace

BOOST_AUTO_TEST_CASE(example_graph_tbb, *boost::unit_test::disabled()) {
  fmt::print("\nExecuting graph with TBB\n\n");
  execute_graph_with_threads(create_graph(), [](int n) { return make_tbb_executor(n); });
}

BOOST_AUTO_TEST_CASE(example_graph_task, *boost::unit_test::disabled()) {
  fmt::print("\nExecuting graph with custom task system\n\n");
  execute_graph_with_threads(create_graph(), [](int n) { return make_task_executor(n); });
}

BOOST_AUTO_TEST_CASE(example_graph_seq, *boost::unit_test::disabled()) {
  fmt::print("\nExecuting graph Sequentially\n\n");
  execute_graph_with_threads(create_graph(), [](int) { return make_seq_executor(); });
}

BOOST_AUTO_TEST_CASE(test_executor_ref_count) {
  Executor e = make_seq_executor();

  ExecutorRef er1 = e;
  ExecutorRef er2 = e;

  BOOST_CHECK_EQUAL(2, e.ref_count());

  ExecutorRef er_copy = er1;
  ExecutorRef er_move = std::move(er2);

  BOOST_CHECK_EQUAL(4, e.ref_count());

  er1 = e.ref();
  er2 = e.ref();

  BOOST_CHECK_EQUAL(4, e.ref_count());

  static_cast<void>(er_copy);
  static_cast<void>(er_move);

  // ~Executor() shouldnt crash (ref count should be 0)
}

BOOST_AUTO_TEST_CASE(test_graph_direct) {
  auto [cg, s] = make_graph(make_type_properties(TypeList<int>{}));
  const auto g = std::move(cg).finalize(s);
  BOOST_CHECK_EQUAL(7, execute_siso_graph<int>(g, make_seq_executor(), 7));
}

BOOST_AUTO_TEST_CASE(test_graph_value) {
  const auto take = [](int i) { return i; };
  auto [cg, s] = make_graph(make_type_properties(TypeList<int>{}));
  const auto g = std::move(cg).finalize(cg.add(AnyFunction(take), s));
  BOOST_CHECK_EQUAL(7, execute_siso_graph<int>(g, make_seq_executor(), 7));
}

BOOST_AUTO_TEST_CASE(test_graph_ref) {
  const auto take_ref = AnyFunction([](const int& i) { return i; });

  auto [cg, s] = make_graph(make_type_properties(TypeList<int>{}));
  const auto g = std::move(cg).finalize(cg.add(take_ref, s));
  BOOST_CHECK_EQUAL(7, execute_siso_graph<int>(g, make_seq_executor(), 7));
}

BOOST_AUTO_TEST_CASE(test_graph_functional_no_args) {
  auto [cg, inputs] = make_graph(make_type_properties(TypeList<FunctionGraph>{}));

  const auto g = std::move(cg).finalize(cg.add_functional({}, {type_id<int>()}, inputs[0], {}));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([]() { return 7; })))));

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(7, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([]() { return 4; })))));

  outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(4, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_functional) {
  auto [cg, inputs] = make_graph(make_type_properties(TypeList<FunctionGraph, int>{}));
  const auto g =
    std::move(cg).finalize(cg.add_functional({{type_id<int>(), true}}, {type_id<int>()}, inputs[0], {inputs[1]}));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([](int x) { return x + 1; })))));
  owned_inputs.push_back(Future(ex, Any(5)));

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(6, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([](int x) { return x + 3; })))));
  owned_inputs.push_back(Future(ex, Any(5)));

  outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(8, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_functional_ref) {
  auto [cg, inputs] = make_graph(make_type_properties(TypeList<FunctionGraph, int>{}));
  const auto g =
    std::move(cg).finalize(cg.add_functional({{type_id<int>(), false}}, {type_id<int>()}, inputs[0], {inputs[1]}));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([](const int& x) { return x + 1; })))));
  owned_inputs.push_back(Future(ex, Any(5)));

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(6, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(make_graph(AnyFunction([](const int& x) { return x + 3; })))));
  owned_inputs.push_back(Future(ex, Any(5)));

  outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(8, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_converge) {
  auto [cg, inputs] = make_graph(make_type_properties(TypeList<FunctionGraph, bool, int, const int&>{}));
  const auto g = std::move(cg).finalize(cg.add_converge(inputs[0], inputs[1], Span<Oterm>(inputs).subspan(2)));

  const auto body = make_graph(AnyFunction([](int x, const int& limit) { return std::tuple(x + 1 >= limit, x + 1); }));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(body)));
  owned_inputs.push_back(Future(ex, Any(false)));
  owned_inputs.push_back(Future(ex, Any(5)));

  std::vector<BorrowedFuture> borrowed_inputs;
  borrowed_inputs.push_back(borrow(Future(ex, Any(10))).first);

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), std::move(borrowed_inputs));
  BOOST_CHECK_EQUAL(10, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(body)));
  owned_inputs.push_back(Future(ex, Any(true)));
  owned_inputs.push_back(Future(ex, Any(7)));
  borrowed_inputs.push_back(borrow(Future(ex, Any(10))).first);

  outputs = execute_graph(g, ex, std::move(owned_inputs), std::move(borrowed_inputs));
  BOOST_CHECK_EQUAL(7, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_if) {
  const auto identity = [](int x) { return x; };
  const auto add1 = [](int x) { return x + 1; };

  auto [cg, inputs] = make_graph(make_type_properties(TypeList<bool, int>{}));
  const auto g =
    std::move(cg).finalize(cg.add_if(make_graph(AnyFunction(identity)), make_graph(AnyFunction(add1)), inputs));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(true)));
  owned_inputs.push_back(Future(ex, Any(5)));

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(5, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(false)));
  owned_inputs.push_back(Future(ex, Any(5)));

  outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(6, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_select) {
  auto [cg, inputs] = make_graph(make_type_properties(TypeList<bool, int, int>{}));
  const auto g = std::move(cg).finalize(cg.add_select(inputs[0], {inputs[1]}, {inputs[2]}));

  auto ex = make_seq_executor();

  std::vector<Future> owned_inputs;
  owned_inputs.push_back(Future(ex, Any(true)));
  owned_inputs.push_back(Future(ex, Any(1)));
  owned_inputs.push_back(Future(ex, Any(2)));

  auto outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(1, any_cast<int>(std::move(outputs.front()).wait()));

  owned_inputs.push_back(Future(ex, Any(false)));
  owned_inputs.push_back(Future(ex, Any(1)));
  owned_inputs.push_back(Future(ex, Any(2)));

  outputs = execute_graph(g, ex, std::move(owned_inputs), {});
  BOOST_CHECK_EQUAL(2, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_zero_arg_function) {
  const auto constant = []() { return 3; };

  auto [cg, inputs] = make_graph(make_type_properties(TypeList<>{}));
  const auto g = std::move(cg).finalize(cg.add(AnyFunction(constant), inputs));

  Executor ex = make_seq_executor();
  auto outputs = execute_graph(g, ex, {}, {});
  BOOST_CHECK_EQUAL(3, any_cast<int>(std::move(outputs.front()).wait()));
}

BOOST_AUTO_TEST_CASE(test_graph_input_sentinal) {
  const auto take = AnyFunction([](Sentinal sent) { return sent; });

  const auto take_ref = AnyFunction([](const Sentinal& sent) {
    BOOST_CHECK_EQUAL(0, sent.copies);
    return sent;
  });

  auto [cg, inputs] = make_graph(make_type_properties(TypeList<Sentinal, Sentinal, Sentinal>{}));

  const Oterm o1 = cg.add(take, {cg.add(take, {inputs[0]})[0]})[0];
  const Oterm o2 = cg.add(take, {inputs[1]})[0];
  const Oterm o3 = inputs[1];
  const Oterm o4 = cg.add(take_ref, {inputs[2]})[0];
  const Oterm o5 = inputs[2];

  const auto g = std::move(cg).finalize({o1, o2, o3, o4, o5});
  auto ex = make_seq_executor();
  std::vector<Future> futures = execute_graph(
    g, ex, make_vector(Future(ex, Any(Sentinal{})), Future(ex, Any(Sentinal{})), Future(ex, Any(Sentinal{}))), {});

  const std::array r{
    any_cast<Sentinal>(std::move(futures[0]).wait()),
    any_cast<Sentinal>(std::move(futures[1]).wait()),
    any_cast<Sentinal>(std::move(futures[2]).wait()),
    any_cast<Sentinal>(std::move(futures[3]).wait()),
    any_cast<Sentinal>(std::move(futures[4]).wait()),
  };

  BOOST_CHECK_EQUAL(0, r[0].copies); // Move since inputs[0] not used elsewhere
  BOOST_CHECK_EQUAL(1,
                    r[1].copies + r[2].copies); // inputs[1] has two outputs one is copied the other is moved
  BOOST_CHECK_EQUAL(1, r[3].copies);            // inputs[2] must be copied through take_ref
  BOOST_CHECK_EQUAL(1, r[4].copies);            // inputs[2] is copied since taken by ref elsewhere
}

BOOST_AUTO_TEST_CASE(test_graph_move_only) {
  const auto take = [](std::unique_ptr<int> ptr) { return *ptr; };
  auto [cg, ptr] = make_graph(make_type_properties(TypeList<std::unique_ptr<int>>{}));
  const auto g = std::move(cg).finalize(cg.add(AnyFunction(take), ptr));
  BOOST_CHECK_EQUAL(5, execute_siso_graph<int>(g, make_seq_executor(), std::make_unique<int>(5)));
}

BOOST_AUTO_TEST_CASE(test_graph_fwd) {
  const auto fwd = [](Sentinal&& s) -> Sentinal&& { return std::move(s); };

  auto [cg, inputs] = make_graph(make_type_properties(TypeList<Sentinal>{}));
  const auto g = std::move(cg).finalize(cg.add(AnyFunction(fwd), inputs));

  const Sentinal result = execute_siso_graph<Sentinal>(g, make_seq_executor(), Sentinal{});

  BOOST_CHECK_EQUAL(0, result.copies);
  BOOST_CHECK_EQUAL(3, result.moves);
}

BOOST_AUTO_TEST_CASE(test_graph_forwarded_ref) {
  Executor ex = make_seq_executor();

  auto [cg, inputs] = make_graph(make_type_properties(TypeList<const Sentinal&, const Sentinal&>{}));

  cg.add(AnyFunction{[](const Sentinal&, Sentinal) {}}, std::array{inputs[0], inputs[0]});

  const auto g = std::move(cg).finalize({});

  auto [b1, post_future1] = borrow(Future(ex, Sentinal{}));
  auto [b2, post_future2] = borrow(Future(ex, Sentinal{}));
  auto results = execute_graph(g, ex, {}, {std::move(b1), std::move(b2)});
  const Sentinal input1 = any_cast<Sentinal>(std::move(post_future1).wait());
  const Sentinal input2 = any_cast<Sentinal>(std::move(post_future2).wait());

  BOOST_CHECK_EQUAL(0, results.size());
  BOOST_CHECK_EQUAL(0, input1.copies);
  BOOST_CHECK_EQUAL(2, input1.moves);
  BOOST_CHECK_EQUAL(0, input2.copies);
  BOOST_CHECK_EQUAL(2, input2.moves);
}

BOOST_AUTO_TEST_CASE(test_graph_timing, *boost::unit_test::disabled()) {
  const size_t COUNT = 5;

  const std::vector<TypeProperties> input_types(COUNT, make_type_properties(Type<const std::string&>{}));
  auto [cg, input_terms] = make_graph(input_types);

  std::vector<Oterm> outputs;

  for(size_t i = 0; i < COUNT; i++) {
    outputs.push_back(cg.add(AnyFunction{[=](const std::string& s) {
                               std::this_thread::sleep_for(std::chrono::duration<int, std::milli>(i));
                               return s + " out";
                             }},
                             std::array{input_terms[i]})[0]);
  }

  const auto g = std::move(cg).finalize(outputs);

  Executor ex = make_task_executor();

  std::vector<Promise> promises;
  std::vector<BorrowedFuture> inputs;
  std::vector<Future> input_futures;

  for(size_t i = 0; i < COUNT; i++) {
    auto [p, f] = make_promise_future(ex);
    auto [b, bf] = borrow(std::move(f));
    promises.push_back(std::move(p));
    inputs.push_back(std::move(b));
    input_futures.push_back(std::move(bf));
  }

  auto futures = execute_graph(g, ex, {}, std::move(inputs));

  futures.insert(
    futures.end(), std::make_move_iterator(input_futures.begin()), std::make_move_iterator(input_futures.end()));

  std::mutex m;
  std::vector<std::pair<std::string, std::chrono::time_point<std::chrono::steady_clock>>> ordered_results;
  ordered_results.reserve(futures.size());

  std::vector<std::thread> threads;
  for(Future& f : futures) {
    threads.emplace_back([&]() {
      std::string str = any_cast<std::string>(std::move(f).wait());
      const auto time = std::chrono::steady_clock::now();
      std::lock_guard lk(m);
      ordered_results.emplace_back(std::move(str), time);
    });
  }

  auto start = std::chrono::steady_clock::now();

  for(size_t i = 0; i < COUNT; i++) {
    std::move(promises[i]).send(std::string(1, char('A' + i)));
  }

  for(std::thread& t : threads) {
    t.join();
  }

  for(const auto& [string, time] : ordered_results) {
    fmt::print("({:05} us) {}\n", std::chrono::duration_cast<std::chrono::microseconds>(time - start).count(), string);
  }
}

} // namespace ooze
