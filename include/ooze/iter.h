#pragma once

#include "ooze/primitives.h"

#include <cassert>
#include <iterator>
#include <tuple>

namespace ooze {

namespace details {

template <typename T>
struct ArrowProxy {
  T t;

  T operator*() { return t; }
};

} // namespace details

template <typename Iter, typename Value, typename Ref = Value&, typename Diff = i64>
struct ForwardIter {
  using value_type = Value;
  using reference = Ref;
  using pointer = Value*;
  using iterator_category = std::forward_iterator_tag;
  using difference_type = Diff;

  Ref operator*() const { return static_cast<const Iter*>(this)->deref(); }
  details::ArrowProxy<Ref> operator->() const { return {operator*()}; }

  Iter& operator++() {
    static_cast<Iter*>(this)->increment();
    return static_cast<Iter&>(*this);
  }
  Iter operator++(int) {
    Iter copy = static_cast<Iter&>(*this);
    operator++();
    return copy;
  }

  friend bool operator==(const Iter& lhs, const Iter& rhs) { return lhs.eq(rhs); }
  friend bool operator!=(const Iter& lhs, const Iter& rhs) { return !lhs.eq(rhs); }
};

template <typename Iter>
class IterRange {
  Iter _begin;
  Iter _end;

  template <size_t... Is>
  auto take(std::index_sequence<Is...>) const {
    auto it = begin();
    auto next = [&](size_t) {
      assert(it != end());
      return *it++;
    };

    return std::tuple(next(Is)...);
  }

public:
  IterRange() = default;
  IterRange(Iter begin, Iter end) : _begin(std::move(begin)), _end(std::move(end)) {}

  using value_type = typename std::iterator_traits<Iter>::value_type;
  using reference = typename std::iterator_traits<Iter>::reference;
  using pointer = typename std::iterator_traits<Iter>::pointer;
  using iterator_category = typename std::iterator_traits<Iter>::iterator_category;
  using difference_type = typename std::iterator_traits<Iter>::difference_type;

  using iterator = Iter;

  i64 ssize() const { return i64(std::distance(_begin, _end)); }
  size_t size() const { return size_t(std::distance(_begin, _end)); }
  bool empty() const { return _begin == _end; }

  Iter begin() const { return _begin; }
  Iter end() const { return _end; }

  template <size_t N>
  auto take() const {
    return take(std::make_index_sequence<N>());
  }

  auto match() const {
    assert(!empty());
    auto it = begin();
    return std::tuple(*_begin, IterRange(++it, end()));
  }

  friend bool operator==(const IterRange& lhs, const IterRange& rhs) {
    return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
  }

  friend bool operator!=(const IterRange& lhs, const IterRange& rhs) { return !(lhs == rhs); }

  friend bool operator<(const IterRange& lhs, const IterRange& rhs) {
    return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
  }

  friend bool operator>(const IterRange& lhs, const IterRange& rhs) { return rhs < lhs; }
  friend bool operator<=(const IterRange& lhs, const IterRange& rhs) { return !(lhs > rhs); }
  friend bool operator>=(const IterRange& lhs, const IterRange& rhs) { return !(lhs < rhs); }
};

} // namespace ooze
