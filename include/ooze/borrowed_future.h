#pragma once

#include "ooze/future.h"

namespace ooze {

struct BorrowedSharedBlock {
  Promise promise;
  Any value;
  std::mutex mutex;
  std::vector<std::function<void(const Any&)>> continuations;
  bool value_ready = false;

  BorrowedSharedBlock(Promise&& p) : promise(std::move(p)) {}
  ~BorrowedSharedBlock() noexcept(false) { std::move(promise).send(std::move(value)); }
};

class BorrowedFuture {
public:
  BorrowedFuture() = default;

  template <typename F, typename = std::enable_if_t<std::is_same_v<void, std::invoke_result_t<F, const Any&>>>>
  void then(F f) {
    std::unique_lock lk(_block->mutex);

    if(_block->value_ready) {
      lk.unlock();
      std::move(f)(_block->value);
    } else {
      _block->continuations.emplace_back(std::move(f));
    }
  }

  template <typename F, typename = std::enable_if_t<!std::is_same_v<void, std::invoke_result_t<F, const Any&>>>>
  Future then(F f) {
    auto [p, new_future] = make_promise_future();

    std::unique_lock lk(_block->mutex);

    if(_block->value_ready) {
      lk.unlock();
      std::move(p).send(std::move(f)(_block->value));
    } else {
      _block->continuations.emplace_back([f = std::move(f), p = std::make_shared<Promise>(std::move(p))](
                                           const Any& value) mutable { std::move(*p).send(std::move(f)(value)); });
    }

    return std::move(new_future);
  }

  // This is safe since weak_ptrs aren't used so long as no one shares the same BorrowedFuture object across threads
  bool unique() const { return _block.use_count() == 1; }

  explicit operator bool() const { return _block != nullptr; }
  bool valid() const { return static_cast<bool>(*this); }

private:
  std::shared_ptr<BorrowedSharedBlock> _block;

  friend std::pair<BorrowedFuture, Future> borrow(Future, int);

  BorrowedFuture(std::shared_ptr<BorrowedSharedBlock> block) : _block(std::move(block)) {}
};

inline std::pair<BorrowedFuture, Future> borrow(Future f, int expected_continuations = 4) {
  auto [new_p, new_f] = make_promise_future();

  auto block = std::make_shared<BorrowedSharedBlock>(std::move(new_p));
  block->continuations.reserve(expected_continuations);

  std::move(f).then([b = block](Any value) mutable {
    b->value = std::move(value);

    {
      const std::lock_guard lk(b->mutex);
      b->value_ready = true;
    }

    for(auto&& f : b->continuations) {
      std::move(f)(b->value);
    }
    b->continuations.clear();
    b = nullptr;
  });

  return {BorrowedFuture(std::move(block)), std::move(new_f)};
}

} // namespace ooze
