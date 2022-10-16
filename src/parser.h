#pragma once

#include "ast.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<UnTypedExpr> parse_expr(std::string_view);
ContextualResult<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view);
ContextualResult<UnTypedFunction> parse_function(std::string_view);
ContextualResult<AST> parse(std::string_view);

} // namespace ooze
