#include "pch.h"

#include "lexer.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

BOOST_AUTO_TEST_CASE(lexer_keywords) {
  BOOST_CHECK((std::pair{TokenType::Keyword, 3u}) == lex_one("let"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 2u}) == lex_one("fn"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 3u}) == lex_one("let!"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 3u}) == lex_one("let "));
}

BOOST_AUTO_TEST_CASE(lexer_idents) {
  BOOST_CHECK((std::pair{TokenType::Ident, 4u}) == lex_one("let_"));
  BOOST_CHECK((std::pair{TokenType::Ident, 4u}) == lex_one("abc9"));
  BOOST_CHECK((std::pair{TokenType::Ident, 1u}) == lex_one("_"));
  BOOST_CHECK((std::pair{TokenType::Ident, 5u}) == lex_one("__1__"));
  BOOST_CHECK((std::pair{TokenType::Ident, 1u}) == lex_one("_ _"));
}

BOOST_AUTO_TEST_CASE(lexer_symbols) {
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("("));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one(")"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("{"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("})"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one(","));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("."));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one(":"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("="));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one("&"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 2u}) == lex_one("->"));
}

BOOST_AUTO_TEST_CASE(lexer_whitespace) {
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1u}) == lex_one(" "));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1u}) == lex_one("\n"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1u}) == lex_one("\t"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1u}) == lex_one("\r"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 2u}) == lex_one("\r\n"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 7u}) == lex_one("\t \t \n \n"));
}

BOOST_AUTO_TEST_CASE(lexer_comment) {
  BOOST_CHECK((std::pair{TokenType::Comment, 1u}) == lex_one("#"));
  BOOST_CHECK((std::pair{TokenType::Comment, 5u}) == lex_one("# abc"));
  BOOST_CHECK((std::pair{TokenType::Comment, 11u}) == lex_one("#(){})!/.a\\"));
  BOOST_CHECK((std::pair{TokenType::Comment, 1u}) == lex_one("#\n"));
}

BOOST_AUTO_TEST_CASE(lexer_integers) {
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1u}) == lex_one("1"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 2u}) == lex_one("-1"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 10u}) == lex_one("1234567890"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 11u}) == lex_one("-1234567890"));

  BOOST_CHECK((std::pair{TokenType::LiteralInt, 3u}) == lex_one("1i8"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1i16"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1i32"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1i64"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("-1i8"));

  BOOST_CHECK((std::pair{TokenType::LiteralInt, 3u}) == lex_one("1u8"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1u16"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1u32"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("1u64"));

  // why does std::from_chars() succeed with a 0?
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4u}) == lex_one("-1u8"));
}

BOOST_AUTO_TEST_CASE(lexer_floats) {
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 4u}) == lex_one("1.0f"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 5u}) == lex_one("-1.0f"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 3u}) == lex_one("1.0"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 4u}) == lex_one("-1.0"));

  // TODO
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1u}) == lex_one("1.f"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1u}) == lex_one("1."));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one(".1"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1u}) == lex_one(".1f"));
}

BOOST_AUTO_TEST_CASE(lexer_other_literals) {
  BOOST_CHECK((std::pair{TokenType::LiteralBool, 4u}) == lex_one("true"));
  BOOST_CHECK((std::pair{TokenType::LiteralBool, 5u}) == lex_one("false"));

  BOOST_CHECK((std::pair{TokenType::LiteralString, 2u}) == lex_one("\"\""));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 5u}) == lex_one("\"abc\""));

  BOOST_CHECK((std::pair{TokenType::LiteralString, 2u}) == lex_one("''"));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 5u}) == lex_one("'abc'"));

  BOOST_CHECK((std::pair{TokenType::Whitespace, 0u}) == lex_one("@"));
}

BOOST_AUTO_TEST_CASE(lexer_multi) {
  // comment and whitespace tokens aren't returned
  const std::vector<Token> expected = {{TokenType::LiteralInt, 0u, 3u},
                                       {TokenType::Ident, 4u, 3u},
                                       {TokenType::Keyword, 17u, 3u},
                                       {TokenType::Keyword, 21u, 2u},
                                       {TokenType::LiteralBool, 24u, 4u},
                                       {TokenType::Ident, 29u, 6u},
                                       {TokenType::Symbol, 35u, 1u},
                                       {TokenType::Symbol, 36u, 1u}};

  const auto [actual_tokens, actual_offset] = lex("123 abc #comment\nlet fn true\tfalsey,=");

  BOOST_CHECK_EQUAL(37u, actual_offset);
  BOOST_CHECK(expected == actual_tokens);
}

BOOST_AUTO_TEST_CASE(lexer_trailing_whitespace) {
  // comment and whitespace tokens aren't returned
  const std::vector<Token> expected = {{TokenType::LiteralInt, 0u, 3u}};
  const auto [actual_tokens, actual_offset] = lex("123 ");

  BOOST_CHECK_EQUAL(4u, actual_offset);
  BOOST_CHECK(expected == actual_tokens);
}

} // namespace ooze
