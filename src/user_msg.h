#pragma once

namespace ooze {

struct ContextualError {
  Slice ref;
  std::string msg;
  std::vector<std::string> notes;

  KNOT_ORDERED(ContextualError);
};

template <typename T>
using ContextualResult = Result<T, std::vector<ContextualError>>;

std::vector<std::string> contextualize(std::string_view src, std::vector<ContextualError>);

} // namespace ooze
