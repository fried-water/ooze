#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

namespace {

auto symbol(std::string_view sv) { return pc::constant(Token{TokenType::Symbol, sv}); }
auto keyword(std::string_view sv) { return pc::constant(Token{TokenType::Keyword, sv}); }

template <typename P>
auto list(P p) {
  return pc::transform(
    pc::seq(symbol("("), pc::choose(symbol(")"), pc::seq(pc::n(pc::seq(p, symbol(","))), p, symbol(")")))),
    [](auto v) -> std::vector<pc::parser_result_t<Span<Token>, P>> {
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

auto ident() {
  return pc::transform_if(
    [](Token t) { return t.type == TokenType::Ident ? std::optional(std::string(t.sv)) : std::nullopt; });
}

auto literal() { return pc::transform_if(to_literal); }

auto type() {
  return pc::transform(pc::seq(pc::maybe(symbol("&")), ident()),
                       [](std::optional<std::tuple<>> opt, std::string_view name) {
                         return ast::Type{std::string(name), opt.has_value()};
                       });
}

auto binding() { return pc::construct<ast::Binding>(seq(ident(), pc::maybe(seq(symbol(":"), type())))); }

std::optional<std::pair<Span<Token>, ast::Expr>> expr(Span<Token>);

auto call() { return pc::construct<ast::Call>(pc::seq(ident(), list(expr))); }

std::optional<std::pair<Span<Token>, ast::Expr>> expr(Span<Token> s) {
  return pc::construct<ast::Expr>(
    pc::choose(list(expr),
               transform(call(), [](ast::Call c) { return std::make_unique<ast::Call>(std::move(c)); }),
               ident(),
               literal()))(s);
}

auto assignment() {
  return pc::construct<ast::Assignment>(pc::seq(keyword("let"), list_or_one(binding()), symbol("="), expr));
}

auto function() {
  return pc::construct<ast::Function>(pc::seq(keyword("fn"),
                                              ident(),
                                              list(binding()),
                                              symbol("->"),
                                              list_or_one(type()),
                                              symbol("{"),
                                              pc::n(assignment()),
                                              expr,
                                              symbol("}")));
}

} // namespace

std::optional<Literal> parse_literal(std::string_view sv) {
  const auto token = lex_one(sv);

  return token.type == TokenType::Ident && sv == token.sv ? Literal{std::string(token.sv)} : to_literal(token);
}

std::optional<AST> parse(std::string_view script) {
  const auto [tokens, remaining] = lex(script);

  if(!remaining.empty()) {
    fmt::print("Lexer failed: {}\n", remaining);
    return std::nullopt;
  } else {
    return parse(n(function()), Span<Token>{tokens});
  }
}

} // namespace ooze
