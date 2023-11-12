#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<std::tuple<AST, ASTTypes>> parse_expr2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse_repl2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse_function2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse2(std::string_view);

// Exposed for unit testing
ContextualResult<std::tuple<AST, ASTTypes>> parse_binding2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse_assignment2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse_type2(std::string_view);
ContextualResult<std::tuple<AST, ASTTypes>> parse_pattern2(std::string_view);

} // namespace ooze
