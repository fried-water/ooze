#pragma once

#include "algorithm.h"

#include "ooze/async_fn.h"
#include "ooze/executor/sequential_executor.h"
#include "ooze/span.h"

namespace ooze {

template <typename... Ts>
std::vector<Future> to_futures(std::tuple<Ts...> ts) {
  return tuple_to_vec<Future>(
    std::move(ts), Overloaded{[](Any a) { return Future{std::move(a)}; }, Construct<Future>{}});
}

template <typename... Ts>
std::vector<BorrowedFuture> to_borrowed_futures(std::tuple<Ts...> ts) {
  return tuple_to_vec<BorrowedFuture>(
    std::move(ts),
    Overloaded{[](Future f) { return borrow(std::move(f)).first; },
               [](Any a) { return borrow(Future{std::move(a)}).first; },
               [](auto x) { return borrow(Future{Any{std::move(x)}}).first; }});
}

inline std::vector<Any> invoke(ExecutorRef ex, AsyncFn fn, std::vector<Any> owned, std::vector<Any> borrowed = {}) {
  return transform_to_vec(
    fn(ex,
       transform_to_vec(std::move(owned), Construct<Future>{}),
       transform_to_vec(std::move(borrowed), [](Any a) { return borrow(Future{std::move(a)}).first; })),
    [](Future f) { return std::move(f).wait(); });
}

inline std::vector<Any> invoke(AsyncFn fn, std::vector<Any> owned, std::vector<Any> borrowed = {}) {
  return invoke(make_seq_executor(), std::move(fn), std::move(owned), std::move(borrowed));
}

template <typename... Ts, typename... Bs>
std::vector<Any> invoke(ExecutorRef ex, AsyncFn fn, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  return transform_to_vec(fn(ex, to_futures(std::move(ts)), to_borrowed_futures(std::move(bs))), [](Future f) {
    return std::move(f).wait();
  });
}

template <typename... Ts, typename... Bs>
std::vector<Any> invoke(AsyncFn fn, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  return invoke(make_seq_executor(), std::move(fn), std::move(ts), std::move(bs));
}

} // namespace ooze
