#pragma once

#include "ast.h"

namespace ooze {

Result<UnTypedExpr> parse_expr(std::string_view);
Result<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view);
Result<UnTypedFunction> parse_function(std::string_view);
Result<AST> parse(std::string_view);

std::vector<std::string> contextualize(std::string_view src, std::string_view msg, Slice);

} // namespace ooze
