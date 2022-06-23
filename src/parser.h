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
ParseResult<AST> parse(std::string_view);

} // namespace ooze
