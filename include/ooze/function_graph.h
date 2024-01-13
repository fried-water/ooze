#pragma once

#include <memory>

namespace ooze {

struct FunctionGraph {
  struct State;
  std::shared_ptr<const State> state;
};

} // namespace ooze
