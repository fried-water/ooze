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
  TokenType type;
  std::string_view sv;

  KNOT_COMPAREABLE(Token);
};

std::optional<Literal> to_literal(Token);

// Failure is represented with an empty whitespace token
Token lex_one(std::string_view);

// If it fails to parse a token the remaining string_view is returned
// on success the remaining string_view is empty
std::pair<std::vector<Token>, std::string_view> lex(std::string_view);

} // namespace ooze
