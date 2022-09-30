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

inline std::string generate_error_msg(std::string_view src, const ParseError& error) {
  std::string highlight = "";
  for(int i = 0; i < error.pos; i++) highlight += " ";
  highlight += '^';
  for(int i = 0; i < static_cast<int>(error.error_token.size()) - 1; i++) highlight += "~";

  return fmt::format("{}:{}:{} {}\n  {}\n  {}", src, error.line, error.pos, error.msg, error.error_line, highlight);
}

template <typename T>
Result<T> convert_error(std::string_view src, ParseResult<T> result) {
  return std::move(result).map_error([&](const auto& error) { return std::vector{generate_error_msg(src, error)}; });
}

} // namespace ooze
