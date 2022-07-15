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

ParseResult<ast::Expr> parse_expr(std::string_view);
ParseResult<std::variant<ast::Expr, ast::Assignment>> parse_repl(std::string_view);
ParseResult<AST> parse(std::string_view);

inline std::string generate_error_msg(std::string_view src, const ParseError& error) {
  std::string highlight = "";
  for(int i = 0; i < error.pos; i++) highlight += " ";
  highlight += '^';
  for(int i = 0; i < static_cast<int>(error.error_token.size()) - 1; i++) highlight += "~";

  return fmt::format("{}:{}:{} {}\n  {}\n  {}", src, error.line, error.pos, error.msg, error.error_line, highlight);
}

} // namespace ooze
