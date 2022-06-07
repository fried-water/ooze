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

auto parameter() {
  return pc::transform(pc::seq(ident(), symbol(":"), pc::maybe(symbol("&")), ident()),
                       [](std::string name, std::optional<std::tuple<>> opt, std::string type) {
                         return ast::Parameter{std::move(name), std::move(type), opt.has_value()};
                       });
}

auto binding() { return pc::construct<ast::Binding>(seq(ident(), pc::maybe(seq(symbol(":"), ident())))); }

std::optional<std::pair<Span<Token>, ast::Expr>> expr(Span<Token>);

auto call() { return pc::construct<Indirect<ast::Call>>(pc::construct<ast::Call>(pc::seq(ident(), list(expr)))); }

std::optional<std::pair<Span<Token>, ast::Expr>> expr(Span<Token> s) {
  return pc::construct<ast::Expr>(pc::choose(call(), ident(), pc::transform_if(to_literal)))(s);
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
    std::optional<AST> ast = pc::parse(pc::n(function()), Span<Token>{tokens});
    return ast ? Result<AST>{std::move(*ast)} : tl::unexpected{std::vector<std::string>{"Parsing failed"}};
  }
}

} // namespace ooze
