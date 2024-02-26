#pragma once

#include "ooze/core.h"

namespace ooze {

inline BindingState find_binding_state(const AsyncValue& v) {
  const auto& [f, b] = v;
  return f.ready() || (b.valid() && b.unique())
           ? BindingState::Ready
           : (b.valid() ? BindingState::Borrowed : BindingState::NotReady);
}

inline Future take(AsyncValue v) { return std::move(v.future); }

inline BorrowedFuture borrow(AsyncValue& v) {
  if(!v.borrowed_future.valid()) {
    std::tie(v.borrowed_future, v.future) = borrow(std::move(v.future));
  }
  return v.borrowed_future;
}

inline AsyncValue await(AsyncValue v) { return {take(std::move(v))}; }

} // namespace ooze
