#include "pch.h"

#include "lexer.h"

#include <ctre.h>

namespace ooze {

namespace {

constexpr auto whitespace_re = ctll::fixed_string{"^\\s+"};
constexpr auto comment_re = ctll::fixed_string{"^//[^\\n]*"};
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

template <typename... Ts>
auto lex_one(const std::tuple<Ts...>& ts, std::string_view sv) {
  const std::array<std::pair<TokenType, u32>, sizeof...(Ts)> matches{
    {{std::get<0>(std::get<Ts>(MATCHERS)), (u32)std::get<1>(std::get<Ts>(MATCHERS))(sv).to_view().size()}...}};

  return *std::max_element(
    matches.begin(), matches.end(), [](const auto& a, const auto& b) { return a.second < b.second; });
}

} // namespace

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
