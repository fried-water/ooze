#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

namespace {

auto symbol(std::string_view sv) { return pc::constant(fmt::format("'{}'", sv), Token{TokenType::Symbol, sv}); }
auto keyword(std::string_view sv) { return pc::constant(fmt::format("{}", sv), Token{TokenType::Keyword, sv}); }

auto ident() {
  return pc::transform_if(
    "identifier", [](Token t) { return t.type == TokenType::Ident ? std::optional(std::string(t.sv)) : std::nullopt; });
}

template <typename P>
auto list(P p) {
  return pc::transform(
    pc::seq(symbol("("), pc::choose(symbol(")"), pc::seq(pc::n(pc::seq(p, symbol(","))), p, symbol(")")))),
    [](auto v) -> std::vector<pc::parser_result_t<Token, P>> {
      if(v.index() == 0) {
        return {};
      } else {
        auto [vec, last] = std::move(std::get<1>(v));
        vec.push_back(std::move(last));
        return std::move(vec);
      }
    });
}

template <typename P>
auto list_or_one(P p) {
  return pc::transform(pc::choose(list(p), p), [](auto v) {
    return v.index() == 0 ? std::get<0>(std::move(v)) : std::vector{std::move(std::get<1>(v))};
  });
}

auto parameter() {
  return pc::transform(pc::seq(ident(), symbol(":"), pc::maybe(symbol("&")), ident()),
                       [](std::string name, std::optional<std::tuple<>> opt, std::string type) {
                         return ast::Parameter{std::move(name), std::move(type), opt.has_value()};
                       });
}

auto binding() { return pc::construct<ast::Binding>(seq(ident(), pc::maybe(seq(symbol(":"), ident())))); }

pc::ParseResult<ast::Expr, Token> expr(Span<Token> s) {
  return pc::transform(
    pc::choose(pc::seq(ident(), maybe(list(expr))), pc::transform_if("literal", to_literal)), [](auto v) {
      return std::visit(Overloaded{[](auto tuple) {
                                     return std::get<1>(tuple)
                                              ? ast::Expr{Indirect{ast::Call{std::move(std::get<0>(tuple)),
                                                                             std::move(*std::get<1>(tuple))}}}
                                              : ast::Expr{std::move(std::get<0>(tuple))};
                                   },
                                   [](Literal l) { return ast::Expr{std::move(l)}; }},
                        std::move(v));
    })(s);
}

auto assignment() {
  return pc::construct<ast::Assignment>(pc::seq(keyword("let"), list_or_one(binding()), symbol("="), expr));
}

auto function() {
  return pc::construct<ast::Function>(pc::seq(keyword("fn"),
                                              ident(),
                                              list(parameter()),
                                              symbol("->"),
                                              list_or_one(ident()),
                                              symbol("{"),
                                              pc::n(assignment()),
                                              list_or_one(expr),
                                              symbol("}")));
}

} // namespace

std::optional<Literal> parse_literal(std::string_view sv) {
  const auto token = lex_one(sv);

  return token.type == TokenType::Ident && sv == token.sv ? Literal{std::string(token.sv)} : to_literal(token);
}

Result<AST> parse(std::string_view script) {
  const auto [tokens, remaining] = lex(script);

  if(!remaining.empty()) {
    return tl::unexpected{std::vector<std::string>{remaining.size() > 10
                                                     ? fmt::format("Lexer failed: {}...", remaining.substr(0, 10))
                                                     : fmt::format("Lexer failed: {}", remaining)}};
  } else {
    Span<Token> token_span{tokens};
    auto res = parse(pc::n(function()), token_span);

    if(res) {
      return std::move(*res);
    } else {
      auto errors = std::move(res.error());
      std::reverse(errors.begin(), errors.end());

      const Token* error_tok = errors.front().second;

      const auto errors_end =
        std::find_if(errors.begin() + 1, errors.end(), [&](const auto& p) { return p.second != error_tok; });

      const std::string_view error_tok_sv =
        error_tok == token_span.end() ? script.substr(script.size()) : error_tok->sv;

      const auto line = 1 + std::count_if(script.data(), error_tok_sv.data(), [](char c) { return c == '\n'; });

      const auto context_begin =
        std::find_if(std::make_reverse_iterator(error_tok_sv.data()),
                     std::make_reverse_iterator(std::max(script.data(), error_tok_sv.data() - 10)),
                     [](char c) { return c == '\n'; })
          .base();

      const auto context_end =
        std::find_if(error_tok_sv.data() + error_tok_sv.size(),
                     std::min(script.data() + script.size(), error_tok_sv.data() + error_tok_sv.size() + 10),
                     [](char c) { return c == '\n'; });

      const std::string_view context(context_begin, (context_end - context_begin));

      const auto preamble_distance = std::distance(context_begin, error_tok_sv.data());
      std::string highlight = "";
      for(int i = 0; i < preamble_distance; i++) highlight += " ";
      highlight += '^';
      for(int i = 0; i < error_tok_sv.size() - 1; i++) highlight += "~";

      return tl::unexpected{std::vector<std::string>{fmt::format(
        "Parsing error at line {}: expected {}\n  {}\n  {}", line, errors.front().first, context, highlight)}};
    }
  }
}

} // namespace ooze
