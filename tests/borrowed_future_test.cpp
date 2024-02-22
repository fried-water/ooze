#include "test.h"

#include "ooze/borrowed_future.h"

#include <random>
#include <thread>

namespace ooze {

BOOST_AUTO_TEST_SUITE(borrowed_future)

BOOST_AUTO_TEST_CASE(cleanup) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));
}

BOOST_AUTO_TEST_CASE(cleanup_promise_first) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));
  p = {};
}

BOOST_AUTO_TEST_CASE(cleanup_future_first) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));
  f = {};
}

BOOST_AUTO_TEST_CASE(copy_cleanup) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));
  auto b2 = b;
}

BOOST_AUTO_TEST_CASE(forward) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));

  std::move(p).send(Any(1));
  b = {};

  int x = 0;
  std::move(f2).then([&](Any v) { x = any_cast<int>(v); });
  BOOST_CHECK_EQUAL(1, x);
}

BOOST_AUTO_TEST_CASE(send_then) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));

  auto b2 = b;

  std::move(p).send(Any(1));

  b.then([](Any v) { BOOST_CHECK_EQUAL(1, any_cast<int>(v)); });

  b2.then([](Any v) {
      BOOST_CHECK_EQUAL(1, any_cast<int>(v));
      return Any(2);
    })
    .then([](Any v) { BOOST_CHECK_EQUAL(2, any_cast<int>(v)); });
}

BOOST_AUTO_TEST_CASE(then_send) {
  auto [p, f] = make_promise_future();
  auto [b, f2] = borrow(std::move(f));

  auto b2 = b;

  b.then([](Any v) { BOOST_CHECK_EQUAL(1, any_cast<int>(v)); });

  b2.then([](Any v) {
      BOOST_CHECK_EQUAL(1, any_cast<int>(v));
      return Any(2);
    })
    .then([](Any v) { BOOST_CHECK_EQUAL(2, any_cast<int>(v)); });

  std::move(p).send(Any(1));
}

BOOST_AUTO_TEST_CASE(stress) {
  constexpr int count = 500;

  std::random_device rd;
  std::mt19937 rng(rd());

  std::atomic<int> calls = 0;

  std::vector<std::function<void()>> functions;
  for(int i = 0; i < count; i++) {
    auto [p, f] = make_promise_future();
    auto [b, f2] = borrow(std::move(f));

    functions.emplace_back([p = std::make_shared<Promise>(std::move(p))]() mutable { std::move(*p).send(Any(1)); });
    functions.emplace_back([f2 = std::make_shared<Future>(std::move(f2)), &calls]() mutable {
      std::move(*f2).then([&](Any v) {
        BOOST_CHECK_EQUAL(1, any_cast<int>(v));
        calls++;
      });
    });

    const int copies = int(rd()) % 4;

    for(int i = 0; i < copies; i++) {
      functions.emplace_back([b = b, &calls]() mutable {
        b.then([&](Any v) {
          BOOST_CHECK_EQUAL(1, any_cast<int>(v));
          calls++;
        });
        b = {};
      });
    }
  }

  std::shuffle(functions.begin(), functions.end(), rng);

  std::vector<std::thread> threads = transform_to_vec(std::move(functions), Construct<std::thread>{});

  for(auto& thread : threads) {
    thread.join();
  }

  BOOST_CHECK_EQUAL(functions.size() - count, calls.load());
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
