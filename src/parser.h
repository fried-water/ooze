#pragma once

#include "ooze/ast.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<UnTypedExpr> parse_expr(std::string_view);
ContextualResult<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view);
ContextualResult<UnTypedFunction> parse_function(std::string_view);
ContextualResult<UnTypedAST> parse(std::string_view);

// Exposed for unit testing
ContextualResult<UnTypedAssignment> parse_assignment(std::string_view);
ContextualResult<UnTypedPattern> parse_pattern(std::string_view);
ContextualResult<CompoundType<NamedType>> parse_type(std::string_view);

} // namespace ooze
