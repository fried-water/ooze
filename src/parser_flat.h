#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult2<void, AST, TypeGraph> parse_expr2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse_repl2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse_function2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse2(AST, TypeGraph, SrcID, std::string_view);

// Exposed for unit testing
ContextualResult2<void, AST, TypeGraph> parse_binding2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse_assignment2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse_type2(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<void, AST, TypeGraph> parse_pattern2(AST, TypeGraph, SrcID, std::string_view);

} // namespace ooze
