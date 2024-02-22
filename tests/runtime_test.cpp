#include "test.h"

#include "constructing_graph.h"
#include "runtime.h"
#include "runtime_test.h"

#include "ooze/executor/sequential_executor.h"
#include "ooze/executor/tbb_executor.h"
#include "ooze/function_graph.h"
#include "ooze/program.h"
#include "ooze/type.h"

#include <algorithm>
#include <chrono>
#include <memory>
#include <numeric>
#include <random>

namespace ooze {

namespace {

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

auto create_pipeline(Program& p, int seed) {
  auto [cg, size] = make_graph({false});
  const auto create_output = cg.add(p.add(create_vector), size, {PassBy::Copy}, 1);
  const auto shuffle_output = cg.add(p.add(create_shuffle(seed)), create_output, {PassBy::Move}, 1);
  const auto sort_output =
    cg.add(p.add([](std::vector<int> v) { return sorted(std::move(v)); }), shuffle_output, {PassBy::Move}, 1);
  const auto acc_output = cg.add(p.add(accumulate), sort_output, {PassBy::Borrow}, 1);
  return std::move(cg).finalize(acc_output, {PassBy::Copy});
}

auto create_graph(Program& p) {
  auto [cg, size] = make_graph({false});

  const std::array ps = {
    cg.add(create_pipeline(p, 0), size)[0],
    cg.add(create_pipeline(p, 1), size)[0],
    cg.add(create_pipeline(p, 2), size)[0],
    cg.add(create_pipeline(p, 3), size)[0],
    cg.add(create_pipeline(p, 4), size)[0],
    cg.add(create_pipeline(p, 5), size)[0],
    cg.add(create_pipeline(p, 6), size)[0],
    cg.add(create_pipeline(p, 7), size)[0]};

  const Inst sumf = p.add([](i64 x, i64 y) { return x + y; });

  const auto pbs = std::array{PassBy::Copy, PassBy::Copy};

  const auto o1 = cg.add(sumf, {ps[0], ps[1]}, pbs, 1)[0];
  const auto o2 = cg.add(sumf, {ps[2], ps[3]}, pbs, 1)[0];
  const auto o3 = cg.add(sumf, {ps[4], ps[5]}, pbs, 1)[0];
  const auto o4 = cg.add(sumf, {ps[6], ps[7]}, pbs, 1)[0];

  const auto o5 = cg.add(sumf, {o1, o2}, pbs, 1)[0];
  const auto o6 = cg.add(sumf, {o3, o4}, pbs, 1)[0];

  return p.add(std::move(cg).finalize(cg.add(sumf, {o5, o6}, pbs, 1), {PassBy::Copy}));
}

template <typename MakeExecutor>
void execute_with_threads(std::shared_ptr<const Program> p, Inst i, MakeExecutor f) {
  const int size = 500'000;
  const int max_threads = 8;

  for(int num_threads = 1; num_threads <= max_threads; num_threads++) {

    const auto t0 = std::chrono::steady_clock::now();
    std::vector<Future> futures;
    {
      Executor e = f(num_threads);
      futures = execute(p, i, e, std::tuple(size), std::tuple());
    }
    const auto results = await(std::move(futures));
    const auto t1 = std::chrono::steady_clock::now();
    fmt::print("{} THREADS: result is {} after {}us\n",
               num_threads,
               any_cast<i64>(results[0]),
               std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count());
  }
}

template <typename... Ts, typename... Bs>
auto execute(Program p, FunctionGraph g, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  const Inst fn = p.add(std::move(g));
  return execute(std::make_shared<Program>(std::move(p)), fn, std::move(ts), std::move(bs));
}

} // namespace

BOOST_AUTO_TEST_SUITE(runtime)

BOOST_AUTO_TEST_SUITE(graph)

BOOST_AUTO_TEST_CASE(example_tbb, *boost::unit_test::disabled()) {
  fmt::print("\nExecuting graph with TBB\n\n");
  Program p;
  const Inst fn = create_graph(p);
  execute_with_threads(std::make_shared<Program>(std::move(p)), fn, [](int n) { return make_tbb_executor(n); });
}

BOOST_AUTO_TEST_CASE(example_seq, *boost::unit_test::disabled()) {
  fmt::print("\nExecuting graph Sequentially\n\n");
  Program p;
  const Inst fn = create_graph(p);
  execute_with_threads(std::make_shared<Program>(std::move(p)), fn, [](int) { return make_seq_executor(); });
}

BOOST_AUTO_TEST_CASE(empty) {
  auto [cg, s] = make_graph({false});
  compare(7, execute({}, std::move(cg).finalize(s, {PassBy::Copy}), std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(copy) {
  Program p;
  const Inst take = p.add([](int i) { return i; });
  auto [cg, s] = make_graph({false});
  FunctionGraph g = std::move(cg).finalize(cg.add(take, s, {PassBy::Copy}, 1), {PassBy::Copy});
  compare(7, execute(std::move(p), std::move(g), std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(move) {
  Program p;
  const Inst take = p.add([](int i) { return i; });
  auto [cg, s] = make_graph({false});
  auto g = std::move(cg).finalize(cg.add(take, s, {PassBy::Move}, 1), {PassBy::Move});
  compare(7, execute(std::move(p), std::move(g), std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(borrow) {
  Program p;
  const Inst take_ref = p.add([](const int& i) { return i; });
  auto [cg, s] = make_graph({false});
  auto g = std::move(cg).finalize(cg.add(take_ref, s, {PassBy::Borrow}, 1), {PassBy::Copy});
  compare(7, execute(std::move(p), std::move(g), std::tuple(7), {}));
}

BOOST_AUTO_TEST_CASE(sentinal) {
  Program p;
  const Inst take = p.add([](Sentinal sent) { return sent; });

  const Inst borrow = p.add([](const Sentinal& sent) {
    BOOST_CHECK_EQUAL(0, sent.copies);
    return sent;
  });

  auto [cg, inputs] = make_graph({false, false, false});

  const Oterm o1 = cg.add(take, cg.add(take, {inputs[0]}, {PassBy::Move}, 1), {PassBy::Move}, 1)[0];
  const Oterm o2 = cg.add(take, {inputs[1]}, {PassBy::Copy}, 1)[0];
  const Oterm o3 = inputs[1];
  const Oterm o4 = cg.add(borrow, {inputs[2]}, {PassBy::Borrow}, 1)[0];
  const Oterm o5 = inputs[2];

  auto g = std::move(cg).finalize(
    {o1, o2, o3, o4, o5}, {PassBy::Move, PassBy::Move, PassBy::Move, PassBy::Move, PassBy::Move});

  const std::vector<Any> results =
    execute(std::move(p), std::move(g), std::tuple(Sentinal{}, Sentinal{}, Sentinal{}), {});

  BOOST_REQUIRE_EQUAL(5, results.size());
  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(results[0]).copies); // Move since inputs[0] not used elsewhere
  BOOST_CHECK_EQUAL(1, any_cast<Sentinal>(results[1]).copies); // Inputs[1] copied into take, moved out
  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(results[2]).copies); // Inputs[1] moved to output
  BOOST_CHECK_EQUAL(1, any_cast<Sentinal>(results[3]).copies); // inputs[2] must be copied through borrow
  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(results[4]).copies); // inputs[2] is moved after borrowed elsewhere
}

BOOST_AUTO_TEST_CASE(move_only) {
  Program p;
  const Inst take = p.add([](std::unique_ptr<int> ptr) { return *ptr; });
  auto [cg, ptr] = make_graph({false});
  const Inst g = p.add(std::move(cg).finalize(cg.add(take, ptr, {PassBy::Move}, 1), {PassBy::Move}));
  compare(5, execute(std::make_shared<Program>(std::move(p)), g, std::tuple(std::make_unique<int>(5)), {}));
}

BOOST_AUTO_TEST_CASE(fwd) {
  Program p;
  const Inst fwd = p.add([](Sentinal&& s) -> Sentinal&& { return std::move(s); });

  auto [cg, inputs] = make_graph({false});
  const Inst g = p.add(std::move(cg).finalize(cg.add(fwd, inputs, {PassBy::Move}, 1), {PassBy::Move}));

  Executor ex = make_seq_executor();
  std::vector<Future> results =
    execute(std::make_shared<Program>(std::move(p)), g, ex, make_vector(Future(Any(Sentinal{}))), {});
  BOOST_REQUIRE_EQUAL(1, results.size());
  const Sentinal result = any_cast<Sentinal>(await(std::move(results.front())));

  BOOST_CHECK_EQUAL(0, result.copies);
  BOOST_CHECK_EQUAL(3, result.moves); // into input any, through fwd, into result
}

BOOST_AUTO_TEST_CASE(borrow_fwd) {
  Program p;
  Executor ex = make_seq_executor();

  auto [cg, inputs] = make_graph({true, true});
  const auto outputs = cg.add(
    p.add([](const Sentinal&, Sentinal) {}), std::array{inputs[0], inputs[0]}, {PassBy::Borrow, PassBy::Copy}, 0);

  const Inst g = p.add(std::move(cg).finalize({}, {}));

  auto [b1, post_future1] = ooze::borrow(Future(Any(Sentinal{})));
  auto [b2, post_future2] = ooze::borrow(Future(Any(Sentinal{})));
  const auto results =
    execute(std::make_shared<Program>(std::move(p)), g, ex, {}, std::vector{std::move(b1), std::move(b2)});
  const Sentinal input1 = any_cast<Sentinal>(await(std::move(post_future1)));
  const Sentinal input2 = any_cast<Sentinal>(await(std::move(post_future2)));

  BOOST_CHECK_EQUAL(0, results.size());
  BOOST_CHECK_EQUAL(0, input1.copies);
  BOOST_CHECK_EQUAL(2, input1.moves);
  BOOST_CHECK_EQUAL(0, input2.copies);
  BOOST_CHECK_EQUAL(2, input2.moves);
}

BOOST_AUTO_TEST_CASE(timing, *boost::unit_test::disabled()) {
  const size_t COUNT = 5;

  Program p;
  auto [cg, input_terms] = make_graph(std::vector<bool>(COUNT, true));

  std::vector<Oterm> outputs;

  for(size_t i = 0; i < COUNT; i++) {
    outputs.push_back(cg.add(p.add([=](const std::string& s) {
      std::this_thread::sleep_for(std::chrono::duration<int, std::milli>(i));
      return s + " out";
    }),
                             {input_terms[i]},
                             {PassBy::Borrow},
                             1)[0]);
  }

  const Inst g = p.add(std::move(cg).finalize(outputs, std::vector<PassBy>(COUNT, PassBy::Move)));

  Executor ex = make_tbb_executor();

  std::vector<Promise> promises;
  std::vector<BorrowedFuture> inputs;
  std::vector<Future> input_futures;

  for(size_t i = 0; i < COUNT; i++) {
    auto [p, f] = make_promise_future();
    auto [b, bf] = ooze::borrow(std::move(f));
    promises.push_back(std::move(p));
    inputs.push_back(std::move(b));
    input_futures.push_back(std::move(bf));
  }

  auto futures = execute(share(p), g, ex, {}, std::move(inputs));

  futures.insert(
    futures.end(), std::make_move_iterator(input_futures.begin()), std::make_move_iterator(input_futures.end()));

  std::mutex m;
  std::vector<std::pair<std::string, std::chrono::time_point<std::chrono::steady_clock>>> ordered_results;
  ordered_results.reserve(futures.size());

  for(Future& f : futures) {
    std::move(f).then([&](Any a) {
      std::string str = any_cast<std::string>(std::move(a));
      const auto time = std::chrono::steady_clock::now();
      const std::lock_guard lk(m);
      ordered_results.emplace_back(std::move(str), time);
    });
  }

  auto start = std::chrono::steady_clock::now();

  for(size_t i = 0; i < COUNT; i++) {
    std::move(promises[i]).send(Any(std::string(1, char('A' + i))));
  }

  ex.wait();

  for(const auto& [string, time] : ordered_results) {
    fmt::print("({:05} us) {}\n", std::chrono::duration_cast<std::chrono::microseconds>(time - start).count(), string);
  }
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(inst)

BOOST_AUTO_TEST_CASE(value) {
  Program p;
  const Inst one = p.add(Any{1});
  const Inst abc = p.add(Any{std::string("abc")});
  compare(1, execute_tbb(share(p), one, {}, {}));
  compare(std::string("abc"), execute_tbb(share(p), abc, {}, {}));
}

BOOST_AUTO_TEST_CASE(fn) {
  const auto test_fn = [](auto exp, auto fn, auto values, auto borrows) {
    Program p;
    const Inst inst = p.add(std::move(fn));
    compare(exp, execute_tbb(share(p), inst, std::move(values), std::move(borrows)));
  };

  test_fn(
    std::tuple(), []() {}, std::tuple(), std::tuple());
  test_fn(
    3, []() { return 3; }, std::tuple(), std::tuple());
  test_fn(
    5, [](int x) { return x; }, std::tuple(5), std::tuple());
  test_fn(
    5, [](const int& x) { return x; }, std::tuple(), std::tuple(5));
  test_fn(
    std::tuple(3, 4), [](int x) { return std::tuple(x, x + 1); }, std::tuple(3), std::tuple());
  test_fn(
    7, [](int x, const int& y) { return x + y; }, std::tuple(3), std::tuple(4));
}

BOOST_AUTO_TEST_CASE(any_function_sentinal_value) {
  Program p;
  const Inst fn = p.add([](Sentinal x) { return x; });

  std::vector<Any> results = execute(share(p), fn, make_vector(Any(Sentinal{})), {});

  BOOST_REQUIRE_EQUAL(1, results.size());
  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(results[0]).copies);
  BOOST_CHECK_EQUAL(4,
                    any_cast<Sentinal>(results[0]).moves); // into any, into function, out of function, into result any
}

BOOST_AUTO_TEST_CASE(any_function_sentinal_rvalue) {
  Program p;
  const Inst fn = p.add([](Sentinal&& x) { return x; });

  std::vector<Any> results = execute(share(p), fn, make_vector(Any(Sentinal{})), {});

  BOOST_REQUIRE_EQUAL(1, results.size());
  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(results[0]).copies);
  BOOST_CHECK_EQUAL(3, any_cast<Sentinal>(results[0]).moves); // into any, out of function, into result any
}

BOOST_AUTO_TEST_CASE(any_function_sentinal_borrow) {
  Program p;
  const Inst fn = p.add([](const Sentinal& x) { return x; });

  auto [b, post_future] = ooze::borrow(Future(Any(Sentinal{})));
  std::vector<Future> results = execute(share(p), fn, make_seq_executor(), {}, std::vector{std::move(b)});

  BOOST_REQUIRE_EQUAL(1, results.size());

  const Any input = await(std::move(post_future));
  const Any result = await(std::move(results[0]));

  BOOST_CHECK_EQUAL(0, any_cast<Sentinal>(input).copies);
  BOOST_CHECK_EQUAL(1, any_cast<Sentinal>(result).copies);

  BOOST_CHECK_EQUAL(1, any_cast<Sentinal>(input).moves);  // into any
  BOOST_CHECK_EQUAL(2, any_cast<Sentinal>(result).moves); // out of function, into any
}

BOOST_AUTO_TEST_CASE(functional) {
  const auto test_fn = [](auto exp, auto fn, int output_count, auto values, auto borrows) {
    Program p;
    const Inst inst = p.add(std::move(fn));
    const Inst functional = p.add(FunctionalInst{output_count});
    compare(exp,
            execute_tbb(share(p), functional, std::tuple_cat(std::tuple(inst), std::move(values)), std::move(borrows)));
  };

  test_fn(
    std::tuple(), []() {}, 0, std::tuple(), std::tuple());
  test_fn(
    3, []() { return 3; }, 1, std::tuple(), std::tuple());
  test_fn(
    5, [](int x) { return x; }, 1, std::tuple(5), std::tuple());
  test_fn(
    5, [](const int& x) { return x; }, 1, std::tuple(), std::tuple(5));
  test_fn(
    std::tuple(3, 4), [](int x) { return std::tuple(x, x + 1); }, 2, std::tuple(3), std::tuple());
  test_fn(
    7, [](int x, const int& y) { return x + y; }, 1, std::tuple(3), std::tuple(4));
}

BOOST_AUTO_TEST_CASE(curry_fn) {
  Program p;
  const Inst add = p.add([](i32 x, i32 y) { return x + y; });
  const Inst add3 = p.curry(add, std::array{Any{3}});
  const Inst seven = p.curry(add3, std::array{Any{4}});

  compare(5, execute_tbb(share(p), add3, std::tuple(2), {}));
  compare(7, execute_tbb(share(p), seven, {}, {}));
}

BOOST_AUTO_TEST_CASE(select) {
  Program p;
  const Inst sel = p.add(SelectInst{});

  compare(std::tuple(), execute_tbb(share(p), sel, std::tuple(true), {}));
  compare(std::tuple(), execute_tbb(share(p), sel, std::tuple(false), {}));

  compare(1, execute_tbb(share(p), sel, std::tuple(true, 1, 2), {}));
  compare(2, execute_tbb(share(p), sel, std::tuple(false, 1, 2), {}));

  compare(std::tuple(1, 2), execute_tbb(share(p), sel, std::tuple(true, 1, 2, 3, 4), {}));
  compare(std::tuple(3, 4), execute_tbb(share(p), sel, std::tuple(false, 1, 2, 3, 4), {}));
}

BOOST_AUTO_TEST_CASE(if_) {
  Program p;

  const Inst one = p.add(Any{1});
  const Inst two = p.add(Any{2});
  const Inst identity = p.add([](int x) { return x; });
  const Inst add1 = p.add([](int x) { return x + 1; });
  const Inst identity_borrow = p.add([](const int& x) { return x; });
  const Inst add1_borrow = p.add([](const int& x) { return x + 1; });
  const Inst add = p.add([](int x, const int& y) { return x + y; });
  const Inst mul = p.add([](int x, const int& y) { return x * y; });

  const Inst if_val = p.add(IfInst{one, two, 1});
  compare(1, execute_tbb(share(p), if_val, std::tuple(true), {}));
  compare(2, execute_tbb(share(p), if_val, std::tuple(false), {}));

  const Inst if_fn = p.add(IfInst{identity, add1, 1, 1, 1});
  compare(5, execute_tbb(share(p), if_fn, std::tuple(true, 5), {}));
  compare(6, execute_tbb(share(p), if_fn, std::tuple(false, 5), {}));

  const Inst if_borrow = p.add(IfInst{identity_borrow, add1_borrow, 1, 0, 0, 1, 1});
  compare(5, execute_tbb(share(p), if_borrow, std::tuple(true), std::tuple(5)));
  compare(6, execute_tbb(share(p), if_borrow, std::tuple(false), std::tuple(5)));

  const Inst if_multi = p.add(IfInst{add, mul, 1, 1, 1, 1, 1});
  compare(7, execute_tbb(share(p), if_multi, std::tuple(true, 3), std::tuple(4)));
  compare(12, execute_tbb(share(p), if_multi, std::tuple(false, 3), std::tuple(4)));

  const Inst if_diff_value = p.add(IfInst{identity, identity, 1, 0, 1});
  compare(1, execute_tbb(share(p), if_diff_value, std::tuple(true, 1, 2), {}));
  compare(2, execute_tbb(share(p), if_diff_value, std::tuple(false, 1, 2), {}));

  const Inst if_diff_borrow = p.add(IfInst{identity_borrow, identity_borrow, 1, 0, 0, 0, 1});
  compare(1, execute_tbb(share(p), if_diff_borrow, std::tuple(true), std::tuple(1, 2)));
  compare(2, execute_tbb(share(p), if_diff_borrow, std::tuple(false), std::tuple(1, 2)));

  const Inst if_diff_cat = p.add(IfInst{identity, identity_borrow, 1, 0, 1, 0, 0});
  compare(1, execute_tbb(share(p), if_diff_cat, std::tuple(true, 1), std::tuple(2)));
  compare(2, execute_tbb(share(p), if_diff_cat, std::tuple(false, 1), std::tuple(2)));
}

BOOST_AUTO_TEST_CASE(converge) {
  Program p;

  const Inst empty_body = p.add(Any{true});
  const Inst body = p.add([](int x, const int& limit) { return std::tuple(x + 1 >= limit, x + 1); });
  const Inst converge = p.add(ConvergeInst{});

  compare(std::tuple(), execute_tbb(share(p), converge, std::tuple(empty_body, false), {}));
  compare(std::tuple(), execute_tbb(share(p), converge, std::tuple(empty_body, true), {}));
  compare(10, execute_tbb(share(p), converge, std::tuple(body, false, 5), std::tuple(10)));
  compare(5, execute_tbb(share(p), converge, std::tuple(body, true, 5), std::tuple(10)));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(stress)

constexpr int NUM_EXECUTIONS = 100;

BOOST_AUTO_TEST_CASE(any_function) {
  Program p;
  const Inst fn = p.add([](int x, const int& y) { return x + y; });
  const auto sp = share(std::move(p));
  for(int i = 0; i < NUM_EXECUTIONS; i++) {
    compare(i + 5, execute_tbb(sp, fn, std::tuple(5), std::tuple(i)));
  }
}

BOOST_AUTO_TEST_CASE(functional) {
  Program p;
  const Inst functional = p.add(FunctionalInst{1});
  for(int i = 0; i < NUM_EXECUTIONS; i++) {
    const Inst fn = p.add([i](int x, const int& y) { return x + y + i; });
    compare(i + 12, execute_tbb(share(p), functional, std::tuple(fn, 5), std::tuple(7)));
  }
}

BOOST_AUTO_TEST_CASE(select) {
  Program p;
  const Inst select = p.add(SelectInst{});
  for(int i = 0; i < NUM_EXECUTIONS; i++) {
    compare(i % 2, execute_tbb(share(p), select, std::tuple(i % 2 == 0, 0, 1), {}));
  }
}

BOOST_AUTO_TEST_CASE(converge) {
  Program p;
  const Inst body = p.add([](int x, const int& limit) { return std::tuple(x + 1 >= limit, x + 1); });
  const Inst converge = p.add(ConvergeInst{});

  for(int i = 0; i < NUM_EXECUTIONS; i++) {
    const int limit = i % 10;
    compare(limit, execute_tbb(share(p), converge, std::tuple(body, 0 >= limit, 0), std::tuple(limit)));
  }
}

BOOST_AUTO_TEST_CASE(if_) {
  Program p;
  const Inst identity = p.add([](int x) { return x; });
  const Inst add1 = p.add([](int x) { return x + 1; });
  const Inst if_inst = p.add(IfInst{identity, add1, 1, 1, 1});
  for(int i = 0; i < NUM_EXECUTIONS; i++) {
    compare(i % 2, execute_tbb(share(p), if_inst, std::tuple(i % 2 == 0, 0), {}));
  }
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
