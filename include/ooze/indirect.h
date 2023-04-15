#pragma once

#include <knot/core.h>

#include <memory>

namespace ooze {

template <typename T>
class Indirect {
  std::unique_ptr<T> _ptr;

public:
  Indirect() : _ptr(std::make_unique<T>()) {}
  Indirect(T t) : _ptr(std::make_unique<T>(std::move(t))) {}

  Indirect(const Indirect& o) : _ptr(std::make_unique<T>(*o._ptr)) {}
  Indirect(Indirect&&) = default;

  Indirect& operator=(const Indirect& o) {
    if(_ptr) {
      *_ptr = *o._ptr;
    } else {
      _ptr = std::make_unique<T>(*o._ptr);
    }
    return *this;
  }

  Indirect& operator=(Indirect&&) = default;

  T& operator*() { return *_ptr; }
  const T& operator*() const { return *_ptr; }

  T* get() { return _ptr.get(); }
  const T* get() const { return _ptr.get(); }

  T* operator->() { return _ptr.get(); }
  const T* operator->() const { return _ptr.get(); }

  friend auto as_tie(const Indirect& i) { return std::tie(*i._ptr); }

  KNOT_ORDERED(Indirect);
};

} // namespace ooze
