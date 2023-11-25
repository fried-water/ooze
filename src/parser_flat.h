#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_expr2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_repl2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_function2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse2(AST, UnresolvedTypes, SrcID, std::string_view);

// Exposed for unit testing
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_binding2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_assignment2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_type2(AST, UnresolvedTypes, SrcID, std::string_view);
ContextualResult2<std::tuple<AST, UnresolvedTypes>> parse_pattern2(AST, UnresolvedTypes, SrcID, std::string_view);

} // namespace ooze
