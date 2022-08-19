#pragma once

#include "ooze/core.h"

namespace ooze {

inline anyf::Future take(Binding binding) { return std::move(binding.future); }

inline anyf::BorrowedFuture borrow(Binding& b) {
  if(!b.borrowed_future.valid()) {
    std::tie(b.borrowed_future, b.future) = borrow(std::move(b.future));
  }
  return b.borrowed_future;
}

template <typename Bindings>
Result<std::pair<TypeID, anyf::Future>> take(Bindings& bindings, const std::string& name) {
  if(const auto var_it = bindings.find(name); var_it != bindings.end()) {
    Binding b = std::move(var_it->second);
    bindings.erase(var_it);
    return std::pair(b.type, std::move(b.future));
  } else {
    return err(fmt::format("Binding {} not found", name));
  }
}

template <typename Bindings>
Result<std::pair<TypeID, anyf::BorrowedFuture>> borrow(Bindings& bindings, const std::string& name) {
  if(const auto var_it = bindings.find(name); var_it == bindings.end()) {
    return err(fmt::format("Binding {} not found", name));
  } else {
    Binding& b = var_it->second;
    return std::pair(b.type, borrow(b));
  }
}

} // namespace ooze
