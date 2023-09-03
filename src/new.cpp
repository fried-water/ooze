#include <fmt/core.h>

#include <new>

namespace {

struct MemoryTracker {
  int64_t allocation_count = 0;
  int64_t allocation_amount = 0;
  int64_t deallocation_count = 0;

  void track(size_t n) {
    allocation_count++;
    allocation_amount += n;
  }

  ~MemoryTracker() {
    fmt::print("\n{}Kb allocated (in {} allocations)\n", allocation_amount / 1024, allocation_count);
    if(allocation_count - deallocation_count != 0) {
      fmt::print("  {} leaked allocations\n", allocation_count - deallocation_count);
    }
  }
};

MemoryTracker s_tracker = {};

} // namespace

void* operator new(std::size_t n) noexcept(false) {
  s_tracker.track(n);
  return malloc(n);
}

void operator delete(void* p) noexcept {
  s_tracker.deallocation_count += p != nullptr;
  free(p);
}

void* operator new[](std::size_t n) noexcept(false) {
  s_tracker.track(n);
  return malloc(n);
}

void operator delete[](void* p) noexcept {
  s_tracker.deallocation_count += p != nullptr;
  free(p);
}
