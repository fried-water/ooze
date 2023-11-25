#pragma once

#include "ooze/src_map.h"

namespace ooze {

struct ContextualError {
  Slice ref;
  std::string msg;
  std::vector<std::string> notes;

  KNOT_ORDERED(ContextualError);
};

struct ContextualError2 {
  SrcRef ref;
  std::string msg;
  std::vector<std::string> notes;

  KNOT_ORDERED(ContextualError2);
};

template <typename T, typename... Ts>
using ContextualResult = Result<T, std::vector<ContextualError>, Ts...>;

template <typename T, typename... Ts>
using ContextualResult2 = Result<T, std::vector<ContextualError2>, Ts...>;

std::vector<std::string> contextualize(std::string_view src, std::vector<ContextualError>);

} // namespace ooze
