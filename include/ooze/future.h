#pragma once

#include "ooze/any.h"

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <variant>

namespace ooze {

inline bool decrement(std::atomic<int>& a) { return a.fetch_sub(1, std::memory_order_acq_rel) == 1; }
inline bool ready(const std::atomic<int>& a) { return a.load(std::memory_order_acquire) == 0; }

struct SharedBlock {
  Any value;
  std::mutex mutex;
  std::condition_variable cv;
  std::optional<std::function<void(Any)>> continuation;
  std::atomic<int> value_ready;

  SharedBlock(Any value, int ref_count = 0) : value(std::move(value)), value_ready(ref_count) {}
  SharedBlock() : SharedBlock(Any{}, 1) {}
};

class Future;
class Promise;

std::pair<Promise, Future> make_promise_future();

class Promise {
public:
  Promise() = default;
  ~Promise() {
    if(_block) std::move(*this).send(Any{});
  }

  Promise(const Promise&) = delete;
  Promise& operator=(const Promise&) = delete;

  Promise(Promise&&) = default;
  Promise& operator=(Promise&& p) {
    this->~Promise();
    _block = std::move(p._block);
    return *this;
  }

  void send(Any value) && {
    _block->value = std::move(value);

    std::unique_lock lk(_block->mutex);
    const bool ready = decrement(_block->value_ready);
    lk.unlock();

    if(ready) {
      _block->cv.notify_one();

      if(_block->continuation) {
        std::move (*_block->continuation)(std::move(_block->value));
      }
    }

    _block = nullptr;
  }

  explicit operator bool() const { return _block != nullptr; }
  bool valid() const { return static_cast<bool>(*this); }

private:
  std::shared_ptr<SharedBlock> _block;

  friend std::pair<Promise, Future> make_promise_future();

  Promise(std::shared_ptr<SharedBlock> block) : _block(std::move(block)) {}
};

class Future {
public:
  Future() = default;

  explicit Future(Any value) : _block(std::make_shared<SharedBlock>(std::move(value))) {}

  Future(Future&&) = default;
  Future& operator=(Future&&) = default;

  bool ready() const { return _block->value_ready.load(std::memory_order_relaxed) == 0; }

  Any wait() && {
    std::unique_lock lk(_block->mutex);
    _block->cv.wait(lk, [&]() { return ready(); });
    Any result = std::move(_block->value);
    _block = nullptr;
    return result;
  }

  template <typename F, typename = std::enable_if_t<std::is_same_v<void, std::invoke_result_t<F, Any>>>>
  void then(F f) && {
    if(_block->value_ready.fetch_add(1, std::memory_order_acquire) == 0) {
      std::move(f)(std::move(_block->value));
    } else {
      _block->continuation = std::move(f);

      if(decrement(_block->value_ready)) {
        std::move (*_block->continuation)(std::move(_block->value));
      }
    }

    _block = nullptr;
  }

  template <typename F, typename = std::enable_if_t<!std::is_same_v<void, std::invoke_result_t<F, Any>>>>
  Future then(F f) && {
    auto [p, new_future] = make_promise_future();

    if(_block->value_ready.fetch_add(1, std::memory_order_acquire) == 0) {
      std::move(p).send(std::move(f)(std::move(_block->value)));
    } else {
      // std::function requires copyable callables...
      auto shared_p = std::make_shared<Promise>(std::move(p));
      _block->continuation = [f = std::move(f), p = std::move(shared_p)](Any value) mutable {
        std::move(*p).send(std::move(f)(std::move(value)));
      };

      if(decrement(_block->value_ready)) {
        std::move (*_block->continuation)(std::move(_block->value));
      }
    }

    _block = nullptr;
    return std::move(new_future);
  }

  explicit operator bool() const { return _block != nullptr; }
  bool valid() const { return static_cast<bool>(*this); }

private:
  std::shared_ptr<SharedBlock> _block;

  friend std::pair<Promise, Future> make_promise_future();

  Future(std::shared_ptr<SharedBlock> block) : _block(std::move(block)) {}
};

inline std::pair<Promise, Future> make_promise_future() {
  auto block = std::make_shared<SharedBlock>();
  return {Promise(block), Future(std::move(block))};
}

} // namespace ooze
