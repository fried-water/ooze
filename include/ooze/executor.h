#pragma once

#include <tbb/task_arena.h>
#include <tbb/task_group.h>

#include <optional>

namespace ooze {

class Executor {
public:
  Executor() = default;

  Executor(int num_threads) : _tbb{std::in_place_t{}} {
    _tbb->arena.initialize(num_threads == -1 ? tbb::task_arena::automatic : num_threads);
  }

  Executor(const Executor&) = delete;
  Executor& operator=(const Executor&) = delete;
  Executor(Executor&&) = delete;
  Executor& operator=(Executor&&) = delete;

  ~Executor() { wait(); }

  template <typename F>
  void run(F&& f) {
    if(_tbb) {
      _tbb->arena.execute([&]() { _tbb->group.run(std::move(f)); });
    } else {
      std::forward<F>(f)();
    }
  }

  void wait() {
    if(_tbb) {
      _tbb->arena.execute([&]() { _tbb->group.wait(); });
    }
  }

private:
  struct TBBExecutor {
    tbb::task_arena arena;
    tbb::task_group group;
  };

  std::optional<TBBExecutor> _tbb;
};

inline Executor make_seq_executor() { return Executor(); }
inline Executor make_tbb_executor(int num_threads = -1) { return Executor(num_threads); }

} // namespace ooze
