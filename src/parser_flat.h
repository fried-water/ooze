#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult2<std::tuple<AST, Types>> parse_expr2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse_repl2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse_function2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse2(AST, Types, SrcID, std::string_view);

// Exposed for unit testing
ContextualResult2<std::tuple<AST, Types>> parse_binding2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse_assignment2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse_type2(AST, Types, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, Types>> parse_pattern2(AST, Types, SrcID, std::string_view);

} // namespace ooze
