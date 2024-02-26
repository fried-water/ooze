#pragma once

#include "ast.h"
#include "user_msg.h"

#include <knot/core.h>

namespace ooze {

template <typename T>
struct ParserResult {
  T parsed;
  std::vector<std::pair<Type, SrcRef>> type_srcs;

  KNOT_COMPAREABLE(ParserResult);
};

ContextualResult<ParserResult<std::vector<ASTID>>, AST> parse(AST, SrcID, std::string_view);

ContextualResult<ParserResult<ASTID>, AST> parse_repl(AST, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST> parse_expr(AST, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST> parse_fn(AST, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST> parse_binding(AST, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST> parse_assignment(AST, SrcID, std::string_view);
ContextualResult<ParserResult<ASTID>, AST> parse_pattern(AST, SrcID, std::string_view);

ContextualResult<ParserResult<Type>, AST> parse_type(AST, SrcID, std::string_view);

} // namespace ooze
