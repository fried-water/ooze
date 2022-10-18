#include "pch.h"

#include "user_msg.h"

namespace ooze {

std::vector<std::string> contextualize(std::string_view src, std::vector<ContextualError> errors) {
  std::vector<std::string> strings;

  strings.reserve(accumulate<size_t>(errors, [&](size_t acc, const auto& e) { return acc + 3 + e.notes.size(); }));

  // Order errors by position in src
  std::sort(errors.begin(), errors.end());

  for(ContextualError& e : errors) {
    if(e.ref != Slice{}) {
      const auto pos = src.begin() + e.ref.begin;
      const auto is_newline = [](char c) { return c == '\n'; };

      const auto line_begin =
        std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

      const auto line_no = std::count_if(src.begin(), line_begin, is_newline) + 1;
      const auto col_no = std::distance(line_begin, pos);

      const std::string error_line(line_begin, std::find_if(pos, src.end(), is_newline));

      std::string highlight = "";
      for(int i = 0; i < col_no; i++) highlight += " ";
      highlight += '^';
      for(int i = 0; i < static_cast<int>(size(e.ref)) - 1; i++) highlight += "~";

      strings.push_back(fmt::format("{}:{} error: {}", line_no, col_no, e.msg));
      strings.push_back(fmt::format(" | {}", error_line));
      strings.push_back(fmt::format(" | {}", highlight));
    } else {
      strings.push_back(fmt::format("error: {}", e.msg));
    }

    strings.insert(strings.end(), std::make_move_iterator(e.notes.begin()), std::make_move_iterator(e.notes.end()));
  }

  return strings;
}

} // namespace ooze
