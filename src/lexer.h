#pragma once

#include "literal.h"

namespace ooze {

enum class TokenType {
  Whitespace,
  Comment,
  Ident,
  Keyword,
  Symbol,
  LiteralFloat,
  LiteralInt,
  LiteralBool,
  LiteralString
};

struct Token {
  TokenType type = {};
  Slice ref;

  KNOT_COMPAREABLE(Token);
};

std::optional<Literal> to_literal(TokenType, std::string_view);

// Failure is represented with an empty whitespace token
std::pair<TokenType, u32> lex_one(std::string_view);

// Returns the tokens and the number of characters parsed
std::pair<std::vector<Token>, u32> lex(std::string_view);

} // namespace ooze
