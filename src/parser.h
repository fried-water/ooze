#pragma once

#include "user_msg.h"

#include "ooze/ast.h"

#include <knot/core.h>

namespace ooze {

template <typename T>
struct ParserResult {
  T parsed;
  std::vector<std::pair<Type, SrcRef>> type_srcs;

  KNOT_COMPAREABLE(ParserResult);
};

ContextualResult<ParserResult<std::vector<ASTID>>, AST, TypeGraph> parse(AST, TypeGraph, SrcID, std::string_view);

ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_repl(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_expr(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_fn(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_binding(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_assignment(AST, TypeGraph, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST, TypeGraph> parse_pattern(AST, TypeGraph, SrcID, std::string_view);

ContextualResult<ParserResult<Type>, AST, TypeGraph> parse_type(AST, TypeGraph, SrcID, std::string_view);

} // namespace ooze
