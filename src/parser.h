#pragma once

#include "ast.h"

namespace ooze {

Result<ast::Expr> parse_expr(std::string_view);
Result<AST> parse(std::string_view);

} // namespace ooze
