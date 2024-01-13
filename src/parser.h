#pragma once

#include "user_msg.h"

#include "ooze/ast.h"

namespace ooze {

ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_expr(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_repl(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_function(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph> parse(AST, TypeGraph, SrcID, std::string_view);

// Exposed for unit testing
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_binding(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_assignment(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_type(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<std::vector<std::pair<Type, SrcRef>>, AST, TypeGraph>
  parse_pattern(AST, TypeGraph, SrcID, std::string_view);

} // namespace ooze
