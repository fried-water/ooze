#pragma once

#include <memory>

namespace ooze {

template <typename T>
class Opaque {
  using Deleter = void (*)(T*);
  std::unique_ptr<T, Deleter> _ptr;

public:
  Opaque() : _ptr(nullptr, [](T*) {}) {}
  explicit Opaque(T t) : _ptr(new T(std::move(t)), [](T* ptr) { delete ptr; }) {}

  T* get() { return _ptr.get(); }
  const T* get() const { return _ptr.get(); }

  T* operator->() { return _ptr.get(); }
  const T* operator->() const { return _ptr.get(); }

  T& operator*() { return *_ptr; }
  const T& operator*() const { return *_ptr; }

  explicit operator bool() { return _ptr; }
};

} // namespace ooze
