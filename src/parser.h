#pragma once

#include "ast.h"

namespace ooze {

struct ParseError {
  std::string msg;
  std::string error_token;
  std::string error_line;
  int line;
  int pos;
};

template <typename T>
using ParseResult = tl::expected<T, ParseError>;

ParseResult<UnTypedExpr> parse_expr(std::string_view);
ParseResult<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view);
ParseResult<UnTypedFunction> parse_function(std::string_view);
ParseResult<AST> parse(std::string_view);

inline std::vector<std::string> generate_error_msg(const ParseError& error) {
  std::string highlight = "";
  for(int i = 0; i < error.pos; i++) highlight += " ";
  highlight += '^';
  for(int i = 0; i < static_cast<int>(error.error_token.size()) - 1; i++) highlight += "~";

  return {fmt::format("{}:{} error: {}", error.line, error.pos, error.msg),
          fmt::format(" | {}", error.error_line),
          fmt::format(" | {}", highlight)};
}

} // namespace ooze
