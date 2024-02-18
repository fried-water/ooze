#include "test.h"

#include "ooze/future.h"

#include <random>
#include <thread>

namespace ooze {

BOOST_AUTO_TEST_SUITE(future)

BOOST_AUTO_TEST_CASE(future_promise_cleanup) { auto [p, f] = make_promise_future(); }

BOOST_AUTO_TEST_CASE(promise_future_cleanup) {
  auto [p, f] = make_promise_future();
  p = {};
}

BOOST_AUTO_TEST_CASE(then_no_send) {
  auto [p, f] = make_promise_future();

  std::move(f).then([&](Any) { BOOST_CHECK(false); });
}

BOOST_AUTO_TEST_CASE(no_send_then) {
  auto [p, f] = make_promise_future();

  p = {};

  std::move(f).then([&](Any) { BOOST_CHECK(false); });
}

BOOST_AUTO_TEST_CASE(send_no_receive) {
  auto [p, f] = make_promise_future();

  std::move(p).send(1);
}

BOOST_AUTO_TEST_CASE(no_receive_send) {
  auto [p, f] = make_promise_future();

  f = {};

  std::move(p).send(1);
}

BOOST_AUTO_TEST_CASE(send_then) {
  auto [p, f] = make_promise_future();

  std::move(p).send(1);
  std::move(f).then([](Any v) { BOOST_CHECK_EQUAL(1, any_cast<int>(v)); });
}

BOOST_AUTO_TEST_CASE(then_send) {
  auto [p, f] = make_promise_future();

  std::move(f).then([](Any v) { BOOST_CHECK_EQUAL(1, any_cast<int>(v)); });
  std::move(p).send(1);
}

BOOST_AUTO_TEST_CASE(then_then_send) {
  auto [p, f] = make_promise_future();

  std::move(f)
    .then([](Any v) {
      BOOST_CHECK_EQUAL(1, any_cast<int>(v));
      return 2;
    })
    .then([](Any v) { BOOST_CHECK_EQUAL(2, any_cast<int>(v)); });

  std::move(p).send(1);
}

BOOST_AUTO_TEST_CASE(stress) {
  constexpr int count = 1000;

  std::atomic<int> calls = 0;

  std::vector<std::function<void()>> functions;

  for(int i = 0; i < count; i++) {
    auto [p, f] = make_promise_future();
    functions.emplace_back([p = std::make_shared<Promise>(std::move(p))]() mutable { std::move(*p).send(1); });
    functions.emplace_back([f = std::make_shared<Future>(std::move(f)), &calls]() mutable {
      std::move(*f).then([&](Any v) {
        BOOST_CHECK_EQUAL(1, any_cast<int>(v));
        calls++;
      });
    });
  }

  std::random_device rd;
  std::mt19937 rng(rd());

  std::shuffle(functions.begin(), functions.end(), rng);

  std::vector<std::thread> threads = transform_to_vec(std::move(functions), Construct<std::thread>{});

  for(auto& thread : threads) {
    thread.join();
  }

  BOOST_CHECK_EQUAL(count, calls.load());
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
