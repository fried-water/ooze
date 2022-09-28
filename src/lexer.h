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

struct SrcRef {
  u32 offset = 0;
  u32 size = 0;

  KNOT_COMPAREABLE(SrcRef);
};

struct Token {
  TokenType type = {};
  SrcRef ref;

  KNOT_COMPAREABLE(Token);
};

std::optional<Literal> to_literal(TokenType, std::string_view);

// Failure is represented with an empty whitespace token
std::pair<TokenType, u32> lex_one(std::string_view);

// If it fails to parse a token the remaining string_view is returned
// on success the remaining string_view is empty
std::pair<std::vector<Token>, u32> lex(std::string_view);

} // namespace ooze
