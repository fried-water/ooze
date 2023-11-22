#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_expr2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_repl2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_function2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse2(std::string_view);

// Exposed for unit testing
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_binding2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_assignment2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_type2(std::string_view);
ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_pattern2(std::string_view);

} // namespace ooze
