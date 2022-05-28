#include "pch.h"

#include "lexer.h"

#include <ctre.h>

#include <charconv>
#include <tuple>

namespace ooze {

namespace {

constexpr auto whitespace_re = ctll::fixed_string{"^\\s+"};
constexpr auto comment_re = ctll::fixed_string{"^#[^\\n]*"};
constexpr auto keyword_re = ctll::fixed_string{"^let|^fn"};
constexpr auto ident_re = ctll::fixed_string{"^[a-zA-Z_][a-zA-Z0-9_]*"};
constexpr auto symbol_re = ctll::fixed_string{"^\\(|^\\)|^\\{|^\\}|^,|^:|^=|^&|^->"};

constexpr auto int_re = ctll::fixed_string{"^-?\\d+(i8|i16|i32|i64|u8|u16|u32|u64)?"};
constexpr auto float_re = ctll::fixed_string{"^-?\\d+?\\.\\d+f?"};
constexpr auto bool_re = ctll::fixed_string{"^true|^false"};
constexpr auto string_re = ctll::fixed_string{"^\".*\""};
constexpr auto file_re = ctll::fixed_string{"^@[^\\s]+"};

template <auto& T>
using matcher = ctre::regex_search_t<typename ctre::regex_builder<T>::type>;

constexpr std::tuple MATCHERS = {std::tuple(matcher<whitespace_re>{}, TokenType::Whitespace),
                                 std::tuple(matcher<comment_re>{}, TokenType::Comment),
                                 std::tuple(matcher<keyword_re>{}, TokenType::Keyword),
                                 std::tuple(matcher<symbol_re>{}, TokenType::Symbol),
                                 std::tuple(matcher<float_re>{}, TokenType::LiteralFloat),
                                 std::tuple(matcher<int_re>{}, TokenType::LiteralInt),
                                 std::tuple(matcher<bool_re>{}, TokenType::LiteralBool),
                                 std::tuple(matcher<file_re>{}, TokenType::LiteralFile),
                                 std::tuple(matcher<string_re>{}, TokenType::LiteralString),
                                 std::tuple(matcher<ident_re>{}, TokenType::Ident)};

template <typename T>
std::optional<T> from_sv(std::string_view sv) {
  T value;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), value);
  return result.ec == std::errc() ? std::optional(value) : std::nullopt;
}

template <typename... Ts>
Token lex_one(const std::tuple<Ts...>& ts, std::string_view sv) {
  const std::array<Token, sizeof...(Ts)> matches{
    {{std::get<1>(std::get<Ts>(MATCHERS)), std::get<0>(std::get<Ts>(MATCHERS))(sv).to_view()}...}};

  return *std::max_element(
    matches.begin(), matches.end(), [](const auto& a, const auto& b) { return a.sv.size() < b.sv.size(); });
}

} // namespace

std::optional<Literal> to_literal(Token token) {
  switch(token.type) {
  case TokenType::Whitespace:
  case TokenType::Comment:
  case TokenType::Keyword:
  case TokenType::Ident:
  case TokenType::Symbol: return std::nullopt;
  case TokenType::LiteralInt: {
    const auto suffix_pos = std::distance(
      token.sv.begin(), std::find_if(token.sv.begin(), token.sv.end(), [](char c) { return c == 'u' || c == 'i'; }));

    const std::string_view number(token.sv.data(), suffix_pos);
    const std::string_view suffix(token.sv.data() + suffix_pos, token.sv.size() - suffix_pos);

    if(suffix == "i8") return Literal{*from_sv<i8>(number)};
    if(suffix == "i16") return Literal{*from_sv<i16>(number)};
    if(suffix == "i32") return Literal{*from_sv<i32>(number)};
    if(suffix == "i64") return Literal{*from_sv<i64>(number)};
    if(suffix == "u8") return Literal{*from_sv<u8>(number)};
    if(suffix == "u16") return Literal{*from_sv<u16>(number)};
    if(suffix == "u32") return Literal{*from_sv<u32>(number)};
    if(suffix == "u64") return Literal{*from_sv<u64>(number)};

    return Literal{*from_sv<i32>(number)};
  }
  case TokenType::LiteralFloat:
    return token.sv.back() == 'f'
             ? Literal{static_cast<f32>(atof(std::string(token.sv.begin(), token.sv.end() - 1).c_str()))}
             : Literal{atof(std::string(token.sv.begin(), token.sv.end()).c_str())};
  case TokenType::LiteralBool: return Literal{token.sv[0] == 't'};
  case TokenType::LiteralString: return Literal{std::string(token.sv.begin() + 1, token.sv.end() - 1)};
  case TokenType::LiteralFile: return Literal{FileLiteral{std::string(token.sv.begin() + 1, token.sv.end())}};
  }
}

Token lex_one(std::string_view sv) { return lex_one(MATCHERS, sv); }

std::pair<std::vector<Token>, std::string_view> lex(std::string_view sv) {
  std::vector<Token> tokens;

  while(!sv.empty()) {
    const Token token = lex_one(sv);

    if(token.sv.empty()) {
      break;
    } else {
      if(token.type != TokenType::Whitespace && token.type != TokenType::Comment) {
        tokens.push_back(token);
      }

      sv.remove_prefix(token.sv.size());
    }
  }

  return {std::move(tokens), sv};
}

} // namespace ooze
