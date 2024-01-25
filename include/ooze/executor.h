#pragma once

#include <knot/type_traits.h>

#include <functional>
#include <memory>

namespace ooze {

struct ExecutorModel {
  virtual ~ExecutorModel() noexcept(false) = default;
  virtual void run(std::function<void()>) = 0;
  virtual void wait() = 0;
};

class ExecutorRef {
  ExecutorModel* _m;

public:
  ExecutorRef(ExecutorModel* m) : _m(m) {}
  void run(std::function<void()> f) { _m->run(std::move(f)); }
  void wait() { _m->wait(); }
};

class Executor {
  template <typename E>
  struct Concrete final : ExecutorModel {
    E e;
    template <typename... Args>
    Concrete(Args&&... args) : e(std::forward<Args>(args)...) {}
    void run(std::function<void()> f) override { e.run(std::move(f)); }
    void wait() override { e.wait(); }
  };

  std::unique_ptr<ExecutorModel> _e;

public:
  template <typename E, typename... Args>
  Executor(knot::Type<E>, Args&&... args) : _e(std::make_unique<Concrete<E>>(std::forward<Args>(args)...)) {}

  template <typename E>
  Executor(E e) : _e(std::unique_ptr<Concrete<E>>(std::move(e))) {}

  Executor(const Executor&) = delete;
  Executor& operator=(const Executor&) = delete;

  Executor(Executor&&) = delete;
  Executor& operator=(Executor&&) = delete;

  void wait() { _e->wait(); }

  ExecutorRef ref() { return {_e.get()}; }
  operator ExecutorRef() { return ref(); }
};

template <typename T, typename... Args>
Executor make_executor(Args&&... args) {
  return Executor(knot::Type<T>{}, std::forward<Args>(args)...);
}

} // namespace ooze
