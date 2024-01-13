#pragma once

#include "ooze/src_map.h"

namespace ooze {

struct ContextualError {
  SrcRef ref;
  std::string msg;
  std::vector<std::string> notes;

  KNOT_ORDERED(ContextualError);
};

template <typename T, typename... Ts>
using ContextualResult = Result<T, std::vector<ContextualError>, Ts...>;

std::vector<std::string> contextualize(Span<std::string_view>, std::vector<ContextualError>);

} // namespace ooze
