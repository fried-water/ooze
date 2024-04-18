#pragma once

#include "algorithm.h"
#include "bindings.h"
#include "runtime.h"

#include "ooze/core.h"
#include "ooze/executor.h"

namespace ooze {

template <typename... Ts>
std::vector<Future> to_futures(std::tuple<Ts...> ts) {
  return tuple_to_vec<Future>(
    std::move(ts), Overloaded{[](auto x) { return Future{Any(std::move(x))}; }, Construct<Future>{}});
}

template <typename... Ts>
std::vector<BorrowedFuture> to_borrowed_futures(std::tuple<Ts...> ts) {
  return tuple_to_vec<BorrowedFuture>(
    std::move(ts),
    Overloaded{[](Future f) { return borrow(std::move(f)).first; },
               [](Any a) { return borrow(Future{std::move(a)}).first; },
               [](auto x) { return borrow(Future{Any{std::move(x)}}).first; }});
}

// assumes seq executor or executor has destructed
inline Any await(Future f) {
  Any res;
  std::move(f).then([&](Any a) { res = std::move(a); });
  return res;
}

inline std::vector<Any> await(std::vector<Future> fs) {
  return transform_to_vec(std::move(fs), [](Future f) { return await(std::move(f)); });
}

// assumes seq executor
inline auto await(Binding b) {
  return std::pair(b.type,
                   transform_to_vec(std::move(b.values), [](AsyncValue v) { return await(take(std::move(v))); }));
}

inline std::vector<Future> execute(
  std::shared_ptr<const Program> p, Inst inst, Executor& ex, std::vector<Any> owned, std::vector<Any> borrowed = {}) {
  std::vector<Future> results(p->output_counts[inst.get()]);
  execute(std::move(p),
          inst,
          ex,
          transform_to_vec(std::move(owned), Construct<Future>{}),
          transform_to_vec(std::move(borrowed), [](Any a) { return borrow(Future{std::move(a)}).first; }),
          results);
  return results;
}

inline std::vector<Any>
execute(std::shared_ptr<const Program> p, Inst inst, std::vector<Any> owned, std::vector<Any> borrowed = {}) {
  Executor ex = make_seq_executor();
  return await(execute(std::move(p), inst, ex, std::move(owned), std::move(borrowed)));
}

template <typename... Ts, typename... Bs>
std::vector<Future>
execute(std::shared_ptr<const Program> p, Inst inst, Executor& ex, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  std::vector<Future> results(p->output_counts[inst.get()]);
  execute(std::move(p), inst, ex, to_futures(std::move(ts)), to_borrowed_futures(std::move(bs)), results);
  return results;
}

template <typename... Ts, typename... Bs>
std::vector<Any> execute(std::shared_ptr<const Program> p, Inst inst, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  Executor ex = make_seq_executor();
  return await(execute(std::move(p), inst, ex, std::move(ts), std::move(bs)));
}

template <typename... Ts, typename... Bs>
std::vector<Any> execute_tbb(std::shared_ptr<const Program> p, Inst inst, std::tuple<Ts...> ts, std::tuple<Bs...> bs) {
  std::vector<Future> fs;
  {
    Executor ex = make_tbb_executor();
    fs = execute(std::move(p), inst, ex, std::move(ts), std::move(bs));
  }
  return await(std::move(fs));
}

template <typename T>
auto share(T t) {
  return std::make_shared<const T>(std::move(t));
}

} // namespace ooze
