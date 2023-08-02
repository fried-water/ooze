#pragma once

#include "ooze/type.h"

namespace ooze {

struct FunctionGraph {
  struct State;
  std::shared_ptr<const State> state;

  Span<TypeProperties> input_types() const;
  Span<TypeID> output_types() const;
};

} // namespace ooze
