#include "pch.h"

#include "user_msg.h"

namespace ooze {

std::vector<std::string> contextualize(Span<std::string_view> srcs, std::vector<ContextualError> errors) {
  return knot::accumulate(
    sorted(std::move(errors)), std::vector<std::string>{}, [&](std::vector<std::string> strings, ContextualError e) {
      if(e.ref.slice != Slice{}) {
        const std::string_view src{srcs[e.ref.file.get()]};
        const auto pos = src.begin() + e.ref.slice.begin;
        const auto is_newline = [](char c) { return c == '\n'; };

        const auto line_begin =
          std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

        const auto line_no = std::count_if(src.begin(), line_begin, is_newline) + 1;
        const auto col_no = std::distance(line_begin, pos);

        const std::string error_line(line_begin, std::find_if(pos, src.end(), is_newline));

        std::string highlight;
        for(int i = 0; i < col_no; i++) highlight += " ";
        highlight += '^';
        for(int i = 0; i < static_cast<int>(size(e.ref.slice)) - 1; i++) highlight += "~";

        strings.push_back(fmt::format("{}:{} error: {}", line_no, col_no, e.msg));
        strings.push_back(fmt::format(" | {}", error_line));
        strings.push_back(fmt::format(" | {}", highlight));
      } else {
        strings.push_back(fmt::format("error: {}", e.msg));
      }

      return to_vec(std::move(e.notes), std::move(strings));
    });
}

} // namespace ooze
