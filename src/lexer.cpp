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
constexpr auto symbol_re = ctll::fixed_string{"^\\(|^\\)|^\\{|^\\}|^,|^\\.|^:|^=|^&|^->"};

constexpr auto int_re = ctll::fixed_string{"^-?\\d+(i8|i16|i32|i64|u8|u16|u32|u64)?"};
constexpr auto float_re = ctll::fixed_string{"^-?\\d+?\\.\\d+f?"};
constexpr auto bool_re = ctll::fixed_string{"^true|^false"};
constexpr auto string_re = ctll::fixed_string{"^\".*\"|^'.*'"};

template <auto& T>
using matcher = ctre::regex_search_t<typename ctre::regex_builder<T>::type>;

constexpr std::tuple MATCHERS = {std::tuple(TokenType::Whitespace, matcher<whitespace_re>{}),
                                 std::tuple(TokenType::Comment, matcher<comment_re>{}),
                                 std::tuple(TokenType::Keyword, matcher<keyword_re>{}),
                                 std::tuple(TokenType::Symbol, matcher<symbol_re>{}),
                                 std::tuple(TokenType::LiteralFloat, matcher<float_re>{}),
                                 std::tuple(TokenType::LiteralInt, matcher<int_re>{}),
                                 std::tuple(TokenType::LiteralBool, matcher<bool_re>{}),
                                 std::tuple(TokenType::LiteralString, matcher<string_re>{}),
                                 std::tuple(TokenType::Ident, matcher<ident_re>{})};

template <typename T>
std::optional<T> from_sv(std::string_view sv) {
  T value;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), value);
  return result.ec == std::errc() ? std::optional(value) : std::nullopt;
}

template <typename... Ts>
auto lex_one(const std::tuple<Ts...>& ts, std::string_view sv) {
  const std::array<std::pair<TokenType, u32>, sizeof...(Ts)> matches{
    {{std::get<0>(std::get<Ts>(MATCHERS)), (u32)std::get<1>(std::get<Ts>(MATCHERS))(sv).to_view().size()}...}};

  return *std::max_element(
    matches.begin(), matches.end(), [](const auto& a, const auto& b) { return a.second < b.second; });
}

} // namespace

std::optional<Literal> to_literal(TokenType type, std::string_view sv) {
  switch(type) {
  case TokenType::Whitespace:
  case TokenType::Comment:
  case TokenType::Keyword:
  case TokenType::Ident:
  case TokenType::Symbol: return std::nullopt;
  case TokenType::LiteralInt: {
    const auto suffix_pos =
      std::distance(sv.begin(), std::find_if(sv.begin(), sv.end(), [](char c) { return c == 'u' || c == 'i'; }));

    const std::string_view number(sv.data(), suffix_pos);
    const std::string_view suffix(sv.data() + suffix_pos, sv.size() - suffix_pos);

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
    return sv.back() == 'f' ? Literal{static_cast<f32>(atof(std::string(sv.begin(), sv.end() - 1).c_str()))}
                            : Literal{atof(std::string(sv.begin(), sv.end()).c_str())};
  case TokenType::LiteralBool: return Literal{sv[0] == 't'};
  case TokenType::LiteralString: return Literal{std::string(sv.begin() + 1, sv.end() - 1)};
  }
}

std::pair<TokenType, u32> lex_one(std::string_view sv) { return lex_one(MATCHERS, sv); }

std::pair<std::vector<Token>, u32> lex(std::string_view sv) {
  std::vector<Token> tokens;

  u32 offset = 0;
  while(!sv.empty()) {
    const auto [type, size] = lex_one(sv);

    if(size == 0) {
      break;
    } else {
      if(type != TokenType::Whitespace && type != TokenType::Comment) {
        tokens.push_back(Token{type, {offset, offset + size}});
      }

      offset += size;
      sv.remove_prefix(size);
    }
  }

  return {std::move(tokens), offset};
}

} // namespace ooze
