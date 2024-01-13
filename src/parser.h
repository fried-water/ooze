#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_expr(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_repl(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_function(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse(AST, TypeGraph, SrcID, std::string_view);

// Exposed for unit testing
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_binding(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_assignment(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_type(AST, TypeGraph, SrcID, std::string_view);
ContextualResult2<std::vector<std::pair<TypeRef, SrcRef>>, AST, TypeGraph>
  parse_pattern(AST, TypeGraph, SrcID, std::string_view);

} // namespace ooze
