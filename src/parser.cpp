#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

namespace {

auto src_sv(std::string_view src, SrcRef r) { return std::string_view(src.begin() + r.offset, r.size); }

auto token_parser(TokenType type) {
  return pc::transform(pc::filter(pc::any(), "token", [=](std::string_view src, Token t) { return t.type == type; }),
                       [](std::string_view src, Token t) { return src_sv(src, t.ref); });
}

auto symbol(std::string_view sv) {
  return pc::nullify(pc::filter(token_parser(TokenType::Symbol),
                                fmt::format("'{}'", sv),
                                [=](std::string_view src, std::string_view t) { return t == sv; }));
}

auto keyword(std::string_view sv) {
  return pc::nullify(pc::filter(token_parser(TokenType::Keyword),
                                fmt::format("'{}'", sv),
                                [=](std::string_view src, std::string_view t) { return t == sv; }));
}

auto ident() { return pc::construct<std::string>(token_parser(TokenType::Ident)); }
auto type_ident() { return pc::construct<NamedType>(ident()); }
auto function_ident() { return pc::construct<NamedFunction>(ident()); }

template <typename P>
auto list(P p) {
  return pc::transform(
    pc::seq(symbol("("), pc::choose(symbol(")"), pc::seq(pc::n(pc::seq(p, symbol(","))), p, symbol(")")))),
    [](auto, auto v) -> std::vector<pc::parser_result_t<std::string_view, Token, P>> {
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
  return pc::transform(pc::choose(list(p), p), [](auto, auto v) {
    return v.index() == 0 ? std::get<0>(std::move(v)) : std::vector{std::move(std::get<1>(v))};
  });
}

auto parameter() {
  return pc::transform(pc::seq(ident(), symbol(":"), pc::maybe(symbol("&")), type_ident()),
                       [](auto, std::string name, std::optional<std::tuple<>> opt, NamedType type) {
                         return ast::Parameter<NamedType>{std::move(name), std::move(type), opt.has_value()};
                       });
}

auto binding() {
  return pc::construct<ast::Binding<NamedType>>(seq(ident(), pc::maybe(seq(symbol(":"), type_ident()))));
}

pc::ParseResult<UnTypedExpr> expr(const pc::ParseState<std::string_view, Token>& s, u32 pos) {
  return pc::transform(
    pc::choose(pc::seq(ident(), pc::maybe(list(expr)), pc::n(pc::seq(symbol("."), ident(), list(expr)))),
               pc::transform_if(pc::any(),
                                "literal",
                                [](std::string_view src, Token t) { return to_literal(t.type, src_sv(src, t.ref)); })),
    [](const auto&, auto v) {
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
    })(s, pos);
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
ParseResult<pc::parser_result_t<std::string_view, Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  auto [parse_pos, value, errors] = p(pc::ParseState<std::string_view, Token>{src, tokens}, 0);

  if(value && parse_pos == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    std::sort(errors.begin(), errors.end(), [](const auto& lhs, const auto& rhs) { return lhs.second > rhs.second; });

    const SrcRef err_ref = (errors.empty() || errors.front().second == tokens.size())
                             ? SrcRef{lex_end, lex_end == src.size() ? 0u : 1u}
                             : tokens[errors.front().second].ref;

    const auto pos = src.begin() + err_ref.offset;
    const auto is_newline = [](char c) { return c == '\n'; };

    const auto line_begin =
      std::find_if(std::make_reverse_iterator(pos), std::make_reverse_iterator(src.begin()), is_newline).base();

    return tl::unexpected{
      ParseError{errors.empty() ? fmt::format("Unknown token") : fmt::format("Error expected {}", errors.front().first),
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
