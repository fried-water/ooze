#pragma once

namespace ooze {

enum class TokenType {
  Whitespace,
  Comment,
  Ident,
  Keyword,
  Underscore,
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

// Failure is represented with an empty whitespace token
std::pair<TokenType, int> lex_one(std::string_view);

// Returns the tokens and the number of characters parsed
std::pair<std::vector<Token>, int> lex(std::string_view);

} // namespace ooze
