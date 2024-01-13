#pragma once

#include "ooze/core.h"

namespace ooze {

enum class BindingState { Ready, Borrowed, NotReady };

inline BindingState find_binding_state(const Binding& binding) {
  const auto& [t, f, b] = binding;
  return f.ready() || (b.valid() && b.unique())
           ? BindingState::Ready
           : (b.valid() ? BindingState::Borrowed : BindingState::NotReady);
}

inline Future take(Binding binding) { return std::move(binding.future); }

inline BorrowedFuture borrow(Binding& b) {
  if(!b.borrowed_future.valid()) {
    std::tie(b.borrowed_future, b.future) = borrow(std::move(b.future));
  }
  return b.borrowed_future;
}

inline Binding await(Binding b) { return Binding{std::move(b.type), take(std::move(b))}; }

template <typename Bindings>
StringResult<std::vector<BorrowedFuture>> borrow(Bindings& bindings, const std::string& name) {
  if(const auto var_it = bindings.find(name); var_it == bindings.end()) {
    return err(fmt::format("Binding {} not found", name));
  } else {
    return borrow(var_it->second);
  }
}

} // namespace ooze
