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

auto type_ident() {
  return pc::transform_if("type identifier", [](Token t) {
    return t.type == TokenType::Ident ? std::optional(NamedType{std::string(t.sv)}) : std::nullopt;
  });
}

auto function_ident() {
  return pc::transform_if("function identifier", [](Token t) {
    return t.type == TokenType::Ident ? std::optional(NamedFunction{std::string(t.sv)}) : std::nullopt;
  });
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
  return pc::transform(pc::seq(ident(), symbol(":"), pc::maybe(symbol("&")), type_ident()),
                       [](std::string name, std::optional<std::tuple<>> opt, NamedType type) {
                         return ast::Parameter<NamedType>{std::move(name), std::move(type), opt.has_value()};
                       });
}

auto binding() {
  return pc::construct<ast::Binding<NamedType>>(seq(ident(), pc::maybe(seq(symbol(":"), type_ident()))));
}

pc::ParseResult<UnTypedExpr, Token> expr(Span<Token> s) {
  return pc::transform(
    pc::choose(pc::seq(ident(), pc::maybe(list(expr)), pc::n(pc::seq(symbol("."), ident(), list(expr)))),
               pc::transform_if("literal", to_literal)),
    [](auto v) {
      return std::visit(
        Overloaded{[](auto&& tuple) {
                     auto&& [binding, parameters, chain] = std::move(tuple);

                     UnTypedExpr acc =
                       parameters
                         ? UnTypedExpr{Indirect{ast::Call<NamedFunction>{std::move(binding), std::move(*parameters)}}}
                         : UnTypedExpr{std::move(binding)};

                     for(auto&& [binding, parameters] : chain) {
                       parameters.insert(parameters.begin(), std::move(acc));
                       acc = UnTypedExpr{Indirect{ast::Call<NamedFunction>{std::move(binding), std::move(parameters)}}};
                     }

                     return acc;
                   },
                   [](Literal l) { return UnTypedExpr{std::move(l)}; }},
        std::move(v));
    })(s);
}

auto assignment() {
  return pc::construct<UnTypedAssignment>(pc::seq(keyword("let"), list_or_one(binding()), symbol("="), expr));
}

auto function() {
  return pc::construct<UnTypedFunction>(pc::seq(keyword("fn"),
                                                ident(),
                                                list(parameter()),
                                                symbol("->"),
                                                list_or_one(type_ident()),
                                                symbol("{"),
                                                pc::n(assignment()),
                                                list_or_one(expr),
                                                symbol("}")));
}

template <typename Parser>
ParseResult<pc::parser_result_t<Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, remaining] = lex(src);

  Span<Token> token_span{tokens};
  auto res = p(token_span);

  if(res.value && res.tokens.empty() && remaining.empty()) {
    return std::move(*res.value);
  } else {
    std::sort(
      res.errors.begin(), res.errors.end(), [](const auto& lhs, const auto& rhs) { return lhs.second > rhs.second; });

    const std::string_view error_sv = res.errors.empty() ? remaining.substr(0, 1)
                                                         : (res.errors.front().second == token_span.end()
                                                              ? remaining.substr(0, remaining.empty() ? 0 : 1)
                                                              : res.errors.front().second->sv);

    const auto pos = src.begin() + (error_sv.data() - src.data());

    const auto is_newline = [](char c) { return c == '\n'; };

    const auto line_begin =
      std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

    return tl::unexpected{ParseError{res.errors.empty() ? fmt::format("Unknown token")
                                                        : fmt::format("Error expected {}", res.errors.front().first),
                                     std::string(error_sv.begin(), error_sv.end()),
                                     std::string(line_begin, std::find_if(pos, src.end(), is_newline)),
                                     static_cast<int>(std::count_if(src.begin(), line_begin, is_newline)) + 1,
                                     static_cast<int>(std::distance(line_begin, pos))}};
  }
}

} // namespace

ParseResult<UnTypedExpr> parse_expr(std::string_view src) { return parse_string(expr, src); }

ParseResult<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view src) {
  return parse_string(pc::choose(expr, assignment()), src);
}

ParseResult<UnTypedFunction> parse_function(std::string_view src) { return parse_string(function(), src); }

ParseResult<AST> parse(std::string_view src) { return parse_string(pc::n(function()), src); }

} // namespace ooze
