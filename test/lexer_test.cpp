#include "pch.h"

#include "lexer.h"

#include <boost/test/unit_test.hpp>

namespace ooze {

BOOST_AUTO_TEST_CASE(lexer_keywords) {
  BOOST_CHECK((Token{TokenType::Keyword, std::string_view("let")}) == lex_one("let"));
  BOOST_CHECK((Token{TokenType::Keyword, std::string_view("fn")}) == lex_one("fn"));
  BOOST_CHECK((Token{TokenType::Keyword, std::string_view("let")}) == lex_one("let!"));
  BOOST_CHECK((Token{TokenType::Keyword, std::string_view("let")}) == lex_one("let "));
}

BOOST_AUTO_TEST_CASE(lexer_idents) {
  BOOST_CHECK((Token{TokenType::Ident, std::string_view("let_")}) == lex_one("let_"));
  BOOST_CHECK((Token{TokenType::Ident, std::string_view("abc9")}) == lex_one("abc9"));
  BOOST_CHECK((Token{TokenType::Ident, std::string_view("_")}) == lex_one("_"));
  BOOST_CHECK((Token{TokenType::Ident, std::string_view("__1__")}) == lex_one("__1__"));
  BOOST_CHECK((Token{TokenType::Ident, std::string_view("_")}) == lex_one("_ _"));
}

BOOST_AUTO_TEST_CASE(lexer_symbols) {
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("(")}) == lex_one("("));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(")")}) == lex_one(")"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("{")}) == lex_one("{"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("}")}) == lex_one("})"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(",")}) == lex_one(","));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(".")}) == lex_one("."));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(":")}) == lex_one(":"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("=")}) == lex_one("="));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("&")}) == lex_one("&"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view("->")}) == lex_one("->"));
}

BOOST_AUTO_TEST_CASE(lexer_whitespace) {
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view(" ")}) == lex_one(" "));
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("\n")}) == lex_one("\n"));
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("\t")}) == lex_one("\t"));
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("\r")}) == lex_one("\r"));
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("\r\n")}) == lex_one("\r\n"));
  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("\t \t \n \n")}) == lex_one("\t \t \n \n"));
}

BOOST_AUTO_TEST_CASE(lexer_comment) {
  BOOST_CHECK((Token{TokenType::Comment, std::string_view("#")}) == lex_one("#"));
  BOOST_CHECK((Token{TokenType::Comment, std::string_view("# abc")}) == lex_one("# abc"));
  BOOST_CHECK((Token{TokenType::Comment, std::string_view("#(){})!/.a\\")}) == lex_one("#(){})!/.a\\"));
  BOOST_CHECK((Token{TokenType::Comment, std::string_view("#")}) == lex_one("#\n"));
}

BOOST_AUTO_TEST_CASE(lexer_integers) {
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1")}) == lex_one("1"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("-1")}) == lex_one("-1"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1234567890")}) == lex_one("1234567890"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("-1234567890")}) == lex_one("-1234567890"));

  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1i8")}) == lex_one("1i8"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1i16")}) == lex_one("1i16"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1i32")}) == lex_one("1i32"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1i64")}) == lex_one("1i64"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("-1i8")}) == lex_one("-1i8"));

  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1u8")}) == lex_one("1u8"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1u16")}) == lex_one("1u16"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1u32")}) == lex_one("1u32"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1u64")}) == lex_one("1u64"));

  // why does std::from_chars() succeed with a 0?
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("-1u8")}) == lex_one("-1u8"));
}

BOOST_AUTO_TEST_CASE(lexer_floats) {
  BOOST_CHECK((Token{TokenType::LiteralFloat, std::string_view("1.0f")}) == lex_one("1.0f"));
  BOOST_CHECK((Token{TokenType::LiteralFloat, std::string_view("-1.0f")}) == lex_one("-1.0f"));
  BOOST_CHECK((Token{TokenType::LiteralFloat, std::string_view("1.0")}) == lex_one("1.0"));
  BOOST_CHECK((Token{TokenType::LiteralFloat, std::string_view("-1.0")}) == lex_one("-1.0"));

  // TODO
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1")}) == lex_one("1.f"));
  BOOST_CHECK((Token{TokenType::LiteralInt, std::string_view("1")}) == lex_one("1."));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(".")}) == lex_one(".1"));
  BOOST_CHECK((Token{TokenType::Symbol, std::string_view(".")}) == lex_one(".1f"));
}

BOOST_AUTO_TEST_CASE(lexer_other_literals) {
  BOOST_CHECK((Token{TokenType::LiteralBool, std::string_view("true")}) == lex_one("true"));
  BOOST_CHECK((Token{TokenType::LiteralBool, std::string_view("false")}) == lex_one("false"));

  BOOST_CHECK((Token{TokenType::LiteralString, std::string_view("\"\"")}) == lex_one("\"\""));
  BOOST_CHECK((Token{TokenType::LiteralString, std::string_view("\"abc\"")}) == lex_one("\"abc\""));

  BOOST_CHECK((Token{TokenType::LiteralString, std::string_view("''")}) == lex_one("''"));
  BOOST_CHECK((Token{TokenType::LiteralString, std::string_view("'abc'")}) == lex_one("'abc'"));

  BOOST_CHECK((Token{TokenType::Whitespace, std::string_view("")}) == lex_one("@"));
}

BOOST_AUTO_TEST_CASE(lexer_multi) {
  const std::string_view data = "123 abc #comment\nlet fn true\tfalsey,=";

  // comment and whitespace tokens aren't returned
  const std::vector<Token> expected = {{TokenType::LiteralInt, "123"},
                                       {TokenType::Ident, "abc"},
                                       {TokenType::Keyword, "let"},
                                       {TokenType::Keyword, "fn"},
                                       {TokenType::LiteralBool, "true"},
                                       {TokenType::Ident, "falsey"},
                                       {TokenType::Symbol, ","},
                                       {TokenType::Symbol, "="}};

  BOOST_CHECK(std::pair(expected, std::string_view()) == lex(data));
}

} // namespace ooze
