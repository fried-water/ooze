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

auto literal_expr() {
  return transform_if(any(), "literal", [](std::string_view src, Token t) {
    auto opt_literal = to_literal(t.type, src_sv(src, t.ref));
    return opt_literal ? std::optional(UnTypedExpr{std::move(*opt_literal), t.ref}) : std::nullopt;
  });
}

auto binding_expr() {
  return transform(ident(), [](std::string_view src, Span<Token> tokens, Slice slice, std::string binding) {
    return UnTypedExpr{std::move(binding), src_slice(tokens, slice)};
  });
}

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>&, ParseLocation);

auto call_expr() {
  return transform(
    seq(ident(), list(expr)),
    [](
      std::string_view src, Span<Token> tokens, Slice slice, std::string binding, std::vector<UnTypedExpr> parameters) {
      return UnTypedExpr{ast::Call<NamedFunction>{std::move(binding), std::move(parameters)}, src_slice(tokens, slice)};
    });
}

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return transform(seq(choose(call_expr(), binding_expr(), literal_expr()), n(seq(symbol("."), call_expr()))),
                   [](UnTypedExpr acc, std::vector<UnTypedExpr> chain) {
                     return accumulate(
                       std::move(chain),
                       [](UnTypedExpr acc, UnTypedExpr next) {
                         auto& params = std::get<ast::Call<NamedFunction>>(next.v).parameters;
                         params.insert(params.begin(), std::move(acc));
                         next.ref.begin = acc.ref.begin;
                         return next;
                       },
                       std::move(acc));
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

  auto [parse_slice, value, error] = p(ParseState<std::string_view, Token>{src, tokens}, {});

  assert(value || error);

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    return tl::unexpected{contextualize(src,
                                        error ? fmt::format("expected {}", error->first) : fmt::format("unknown token"),
                                        (error && error->second.pos < tokens.size())
                                          ? tokens[error->second.pos].ref
                                          : Slice{lex_end, lex_end == src.size() ? lex_end : lex_end + 1})};
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
