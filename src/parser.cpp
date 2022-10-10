#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

using namespace pc;

namespace {

Slice src_slice(Span<Token> tokens, Slice s) { return Slice{tokens[s.begin].ref.begin, tokens[s.end - 1].ref.end}; }

std::string_view src_sv(std::string_view src, Slice s) {
  return std::string_view(src.begin() + s.begin, s.end - s.begin);
}

auto token_parser(TokenType type) {
  return transform(filter(any(), "token", [=](Token t) { return t.type == type; }),
                   [](std::string_view src, Token t) { return src_sv(src, t.ref); });
}

auto symbol(std::string_view sv) {
  return nullify(
    filter(token_parser(TokenType::Symbol), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

auto keyword(std::string_view sv) {
  return nullify(
    filter(token_parser(TokenType::Keyword), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

auto ident() { return construct<std::string>(token_parser(TokenType::Ident)); }
auto type_ident() { return construct<NamedType>(ident()); }
auto function_ident() { return construct<NamedFunction>(ident()); }

template <typename P>
auto list(P p) {
  return transform(seq(symbol("("), choose(symbol(")"), seq(n(seq(p, symbol(","))), p, symbol(")")))),
                   [](auto v) -> std::vector<parser_result_t<std::string_view, Token, P>> {
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
  return transform(choose(list(p), p), [](auto v) {
    return v.index() == 0 ? std::get<0>(std::move(v)) : std::vector{std::move(std::get<1>(v))};
  });
}

auto parameter() {
  return transform(seq(ident(), symbol(":"), maybe(symbol("&")), type_ident()),
                   [](std::string name, std::optional<std::tuple<>> opt, NamedType type) {
                     return ast::Parameter<NamedType>{std::move(name), std::move(type), opt.has_value()};
                   });
}

auto binding() { return construct<ast::Binding<NamedType>>(seq(ident(), maybe(seq(symbol(":"), type_ident())))); }

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return transform(
    choose(seq(ident(), maybe(list(expr)), n(seq(symbol("."), ident(), list(expr)))),
           transform_if(
             any(), "literal", [](std::string_view src, Token t) { return to_literal(t.type, src_sv(src, t.ref)); })),
    [](auto v) {
      return std::visit(Overloaded{[](auto&& tuple) {
                                     auto&& [binding, parameters, chain] = std::move(tuple);

                                     UnTypedExpr acc = parameters ? UnTypedExpr{ast::Call<NamedFunction>{
                                                                      std::move(binding), std::move(*parameters)}}
                                                                  : UnTypedExpr{std::move(binding)};

                                     for(auto&& [binding, parameters] : chain) {
                                       parameters.insert(parameters.begin(), std::move(acc));
                                       acc = UnTypedExpr{ast::Call<NamedFunction>{binding, std::move(parameters)}};
                                     }

                                     return acc;
                                   },
                                   [](Literal l) { return UnTypedExpr{std::move(l)}; }},
                        std::move(v));
    })(s, loc);
}

auto assignment() {
  return construct<UnTypedAssignment>(seq(keyword("let"), list_or_one(binding()), symbol("="), expr));
}

auto function_header() {
  return construct<UnTypedHeader>(seq(list(parameter()), symbol("->"), list_or_one(type_ident())));
}

auto function_body() {
  return construct<UnTypedBody>(seq(symbol("{"), n(assignment()), list_or_one(expr), symbol("}")));
}

auto function() { return construct<UnTypedFunction>(seq(keyword("fn"), ident(), function_header(), function_body())); }

template <typename Parser>
Result<parser_result_t<std::string_view, Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  auto [parse_slice, value, errors] = p(ParseState<std::string_view, Token>{src, tokens}, {});

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    std::sort(errors.begin(), errors.end(), [](const auto& lhs, const auto& rhs) {
      return std::tuple(-lhs.second.pos, lhs.second.depth) < std::tuple(-rhs.second.pos, lhs.second.depth);
    });

    return tl::unexpected{
      contextualize(src,
                    errors.empty() ? fmt::format("unknown token") : fmt::format("expected {}", errors.front().first),
                    (errors.empty() || errors.front().second.pos == tokens.size())
                      ? Slice{lex_end, lex_end == src.size() ? lex_end : lex_end + 1}
                      : tokens[errors.front().second.pos].ref)};
  }
}

} // namespace

Result<UnTypedExpr> parse_expr(std::string_view src) { return parse_string(expr, src); }

Result<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view src) {
  return parse_string(choose(expr, assignment()), src);
}

Result<UnTypedFunction> parse_function(std::string_view src) { return parse_string(function(), src); }

Result<AST> parse(std::string_view src) { return parse_string(n(function()), src); }

std::vector<std::string> contextualize(std::string_view src, std::string_view msg, Slice slice) {
  const auto pos = src.begin() + slice.begin;
  const auto is_newline = [](char c) { return c == '\n'; };

  const auto line_begin =
    std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

  const auto line_no = std::count_if(src.begin(), line_begin, is_newline) + 1;
  const auto col_no = std::distance(line_begin, pos);

  const std::string error_line(line_begin, std::find_if(pos, src.end(), is_newline));

  std::string highlight = "";
  for(int i = 0; i < col_no; i++) highlight += " ";
  highlight += '^';
  for(int i = 0; i < static_cast<int>(size(slice)) - 1; i++) highlight += "~";

  return {fmt::format("{}:{} error: {}", line_no, col_no, msg),
          fmt::format(" | {}", error_line),
          fmt::format(" | {}", highlight)};
}

} // namespace ooze
