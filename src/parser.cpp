#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

namespace {

Slice src_slice(Span<Token> tokens, Slice s) { return Slice{tokens[s.begin].ref.begin, tokens[s.end - 1].ref.end}; }

std::string_view src_sv(std::string_view src, Slice s) {
  return std::string_view(src.begin() + s.begin, s.end - s.begin);
}

auto token_parser(TokenType type) {
  return pc::transform(pc::filter(pc::any(), "token", [=](Token t) { return t.type == type; }),
                       [](std::string_view src, Token t) { return src_sv(src, t.ref); });
}

auto symbol(std::string_view sv) {
  return pc::nullify(
    pc::filter(token_parser(TokenType::Symbol), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

auto keyword(std::string_view sv) {
  return pc::nullify(
    pc::filter(token_parser(TokenType::Keyword), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

auto ident() { return pc::construct<std::string>(token_parser(TokenType::Ident)); }
auto type_ident() { return pc::construct<NamedType>(ident()); }
auto function_ident() { return pc::construct<NamedFunction>(ident()); }

template <typename P>
auto list(P p) {
  return pc::transform(
    pc::seq(symbol("("), pc::choose(symbol(")"), pc::seq(pc::n(pc::seq(p, symbol(","))), p, symbol(")")))),
    [](auto v) -> std::vector<pc::parser_result_t<std::string_view, Token, P>> {
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

pc::ParseResult<UnTypedExpr> expr(const pc::ParseState<std::string_view, Token>& s, pc::ParseLocation loc) {
  return pc::transform(
    pc::choose(pc::seq(ident(), pc::maybe(list(expr)), pc::n(pc::seq(symbol("."), ident(), list(expr)))),
               pc::transform_if(pc::any(),
                                "literal",
                                [](std::string_view src, Token t) { return to_literal(t.type, src_sv(src, t.ref)); })),
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
  return pc::construct<UnTypedAssignment>(pc::seq(keyword("let"), list_or_one(binding()), symbol("="), expr));
}

auto function_header() {
  return pc::construct<UnTypedHeader>(pc::seq(list(parameter()), symbol("->"), list_or_one(type_ident())));
}

auto function_body() {
  return pc::construct<UnTypedBody>(pc::seq(symbol("{"), pc::n(assignment()), list_or_one(expr), symbol("}")));
}

auto function() {
  return pc::construct<UnTypedFunction>(pc::seq(keyword("fn"), ident(), function_header(), function_body()));
}

template <typename Parser>
ParseResult<pc::parser_result_t<std::string_view, Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  auto [parse_slice, value, errors] = p(pc::ParseState<std::string_view, Token>{src, tokens}, {});

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    std::sort(errors.begin(), errors.end(), [](const auto& lhs, const auto& rhs) {
      return std::tuple(-lhs.second.pos, lhs.second.depth) < std::tuple(-rhs.second.pos, lhs.second.depth);
    });

    const Slice err_ref = (errors.empty() || errors.front().second.pos == tokens.size())
                            ? Slice{lex_end, lex_end == src.size() ? lex_end : lex_end + 1}
                            : tokens[errors.front().second.pos].ref;

    const auto pos = src.begin() + err_ref.begin;
    const auto is_newline = [](char c) { return c == '\n'; };

    const auto line_begin =
      std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

    return tl::unexpected{
      ParseError{errors.empty() ? fmt::format("unknown token") : fmt::format("expected {}", errors.front().first),
                 std::string(src_sv(src, err_ref)),
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
