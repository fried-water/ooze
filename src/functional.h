#pragma once

namespace ooze {

template <typename T>
struct Construct {
  template <typename... Ts>
  T operator()(Ts&&... args) const {
    return T{std::forward<Ts>(args)...};
  };
};

} // namespace ooze
