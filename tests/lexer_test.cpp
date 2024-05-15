#include "test.h"

#include "lexer.h"

namespace ooze {

BOOST_AUTO_TEST_SUITE(lexer)

BOOST_AUTO_TEST_CASE(keywords) {
  BOOST_CHECK((std::pair{TokenType::Keyword, 3}) == lex_one("let"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 2}) == lex_one("fn"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 3}) == lex_one("let!"));
  BOOST_CHECK((std::pair{TokenType::Keyword, 3}) == lex_one("let "));
}

BOOST_AUTO_TEST_CASE(idents) {
  BOOST_CHECK((std::pair{TokenType::Ident, 4}) == lex_one("let_"));
  BOOST_CHECK((std::pair{TokenType::Ident, 4}) == lex_one("abc9"));
  BOOST_CHECK((std::pair{TokenType::Ident, 2}) == lex_one("_X"));
  BOOST_CHECK((std::pair{TokenType::Ident, 5}) == lex_one("__1__"));
  BOOST_CHECK((std::pair{TokenType::Ident, 2}) == lex_one("_a _"));
  BOOST_CHECK((std::pair{TokenType::Ident, 26}) == lex_one("abcdefghijklmnopqrstuvwxyz"));
}

BOOST_AUTO_TEST_CASE(underscore) { BOOST_CHECK((std::pair{TokenType::Underscore, 1}) == lex_one("_")); }

BOOST_AUTO_TEST_CASE(symbols) {
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("("));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(")"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("{"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("})"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(","));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(";"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("."));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(":"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("="));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one("&"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 2}) == lex_one("->"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 2}) == lex_one("=>"));
}

BOOST_AUTO_TEST_CASE(whitespace) {
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1}) == lex_one(" "));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1}) == lex_one("\n"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1}) == lex_one("\t"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 1}) == lex_one("\r"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 2}) == lex_one("\r\n"));
  BOOST_CHECK((std::pair{TokenType::Whitespace, 7}) == lex_one("\t \t \n \n"));
}

BOOST_AUTO_TEST_CASE(comment) {
  BOOST_CHECK((std::pair{TokenType::Comment, 2}) == lex_one("//"));
  BOOST_CHECK((std::pair{TokenType::Comment, 6}) == lex_one("// abc"));
  BOOST_CHECK((std::pair{TokenType::Comment, 12}) == lex_one("//(){})!/.a\\"));
  BOOST_CHECK((std::pair{TokenType::Comment, 2}) == lex_one("//\n"));
}

BOOST_AUTO_TEST_CASE(integers) {
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1}) == lex_one("1"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 2}) == lex_one("-1"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 10}) == lex_one("1234567890"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 11}) == lex_one("-1234567890"));

  BOOST_CHECK((std::pair{TokenType::LiteralInt, 3}) == lex_one("1i8"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1i16"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1i32"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1i64"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("-1i8"));

  BOOST_CHECK((std::pair{TokenType::LiteralInt, 3}) == lex_one("1u8"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1u16"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1u32"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("1u64"));

  // why does std::from_chars() succeed with a 0?
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 4}) == lex_one("-1u8"));
}

BOOST_AUTO_TEST_CASE(floats) {
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 4}) == lex_one("1.0f"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 5}) == lex_one("-1.0f"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 3}) == lex_one("1.0"));
  BOOST_CHECK((std::pair{TokenType::LiteralFloat, 4}) == lex_one("-1.0"));

  // TODO
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1}) == lex_one("1.f"));
  BOOST_CHECK((std::pair{TokenType::LiteralInt, 1}) == lex_one("1."));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(".1"));
  BOOST_CHECK((std::pair{TokenType::Symbol, 1}) == lex_one(".1f"));
}

BOOST_AUTO_TEST_CASE(other_literals) {
  BOOST_CHECK((std::pair{TokenType::LiteralBool, 4}) == lex_one("true"));
  BOOST_CHECK((std::pair{TokenType::LiteralBool, 5}) == lex_one("false"));

  BOOST_CHECK((std::pair{TokenType::LiteralString, 2}) == lex_one("\"\""));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 2}) == lex_one("\"\"\""));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 5}) == lex_one("\"abc\""));

  BOOST_CHECK((std::pair{TokenType::LiteralString, 2}) == lex_one("''"));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 2}) == lex_one("'''"));
  BOOST_CHECK((std::pair{TokenType::LiteralString, 5}) == lex_one("'abc'"));

  BOOST_CHECK((std::pair{TokenType::Whitespace, 0}) == lex_one("@"));
}

BOOST_AUTO_TEST_CASE(multi) {
  // comment and whitespace tokens aren't returned
  const std::vector<Token> expected = {
    {TokenType::LiteralInt, 0, 3},
    {TokenType::Ident, 4, 7},
    {TokenType::Keyword, 18, 21},
    {TokenType::Keyword, 22, 24},
    {TokenType::LiteralBool, 25, 29},
    {TokenType::Ident, 30, 36},
    {TokenType::Symbol, 36, 37},
    {TokenType::Symbol, 37, 38}};

  const auto [actual_tokens, actual_offset] = lex("123 abc //comment\nlet fn true\tfalsey,=");

  BOOST_CHECK_EQUAL(38, actual_offset);
  BOOST_CHECK(expected == actual_tokens);
}

BOOST_AUTO_TEST_CASE(trailing_whitespace) {
  // comment and whitespace tokens aren't returned
  const std::vector<Token> expected = {{TokenType::LiteralInt, 0, 3}};
  const auto [actual_tokens, actual_offset] = lex("123 ");

  BOOST_CHECK_EQUAL(4, actual_offset);
  BOOST_CHECK(expected == actual_tokens);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
