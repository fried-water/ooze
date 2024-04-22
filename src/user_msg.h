#pragma once

#include "src_map.h"

namespace ooze {

struct ContextualError {
  SrcRef ref;
  std::string msg;
  std::vector<std::string> notes;

  friend auto operator<=>(const ContextualError&, const ContextualError&) = default;
};

template <typename T, typename... Ts>
using ContextualResult = Result<T, std::vector<ContextualError>, Ts...>;

std::vector<std::string> contextualize(Span<std::string_view>, std::vector<ContextualError>);

} // namespace ooze
