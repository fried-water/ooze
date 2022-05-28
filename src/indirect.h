#pragma once

#include <memory>

namespace ooze {

template <typename T>
class Indirect {
  std::unique_ptr<T> _ptr;

public:
  Indirect() = default;
  Indirect(T t) : _ptr(std::make_unique<T>(std::move(t))) {}

  Indirect(Indirect&&) = default;
  Indirect& operator=(Indirect&&) = default;

  Indirect(const Indirect& i) : _ptr(std::make_unique<T>(*i._ptr)) {}
  Indirect& operator=(const Indirect& i) {
    _ptr = std::make_unique<T>(*i._ptr);
    return *this;
  }

  const T& operator*() const { return *_ptr; }
  T& operator*() { return *_ptr; }

  const T* operator->() const { return _ptr.get(); }
  T* operator->() { return _ptr.get(); }

  operator bool() const { return _ptr != nullptr; }

  friend const T& as_tie(const Indirect& i) { return *i; }
  friend T& as_tie(Indirect& i) { return *i; }

  KNOT_ORDERED(Indirect);
};

} // namespace ooze
