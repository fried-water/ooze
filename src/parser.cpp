#include "pch.h"

#include "lexer.h"
#include "literal.h"
#include "parser.h"
#include "parser_combinators.h"

namespace ooze {

using namespace pc;
using namespace ast;

namespace {

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

auto underscore() { return nullify(token_parser(TokenType::Underscore)); }

template <typename T, typename P>
auto construct_with_ref(P p) {
  return transform(p, [](const auto&, auto tokens, Slice s, auto&&... ts) {
    return T{std::forward<decltype(ts)>(ts)..., Slice{tokens[s.begin].ref.begin, tokens[s.end - 1].ref.end}};
  });
}

auto ident_string() { return construct<std::string>(token_parser(TokenType::Ident)); }
auto ident_expr() { return construct<IdentExpr>(ident_string()); }
auto type_ident() { return construct_with_ref<NamedType>(ident_string()); }
auto function_ident() { return construct<NamedFunction>(ident_string()); }

template <typename T, typename T2, typename... Ts>
auto split_vec_from_remaining(std::tuple<T, T2, Ts...>&& tuple) {
  if constexpr(sizeof...(Ts) == 0) {
    return std::tuple(std::get<0>(std::move(tuple)), std::get<1>(std::move(tuple)));
  } else {
    return std::tuple(std::get<0>(std::move(tuple)),
                      std::tuple(std::get<1>(std::move(tuple)), std::get<Ts>(std::move(tuple))...));
  }
}

template <typename P>
auto tuple(P p) {
  using T = parser_result_t<std::string_view, Token, P>;

  return transform(seq(symbol("("), choose(symbol(")"), seq(n(seq(p, symbol(","))), p, symbol(")")))),
                   [](auto v) -> std::vector<T> {
                     if(v.index() == 0) {
                       return {};
                     } else {
                       auto [vec, last] = split_vec_from_remaining(std::move(std::get<1>(v)));
                       vec.push_back(std::move(last));
                       return std::move(vec);
                     }
                   });
}

ParseResult<Pattern> pattern(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return construct_with_ref<Pattern>(
    choose(tuple(pattern), construct<WildCard>(underscore()), construct<Ident>(ident_string())))(s, loc);
}

using TypeVar =
  std::variant<std::vector<CompoundType<NamedType>>, FunctionType<NamedType>, Floating, Borrow<NamedType>, NamedType>;

ParseResult<CompoundType<NamedType>> type(const ParseState<std::string_view, Token>& s, ParseLocation loc);

auto borrow_type() { return construct<Borrow<NamedType>>(seq(symbol("&"), type)); }

auto leaf_type() {
  return transform(choose(underscore(), construct_with_ref<NamedType>(ident_string())),
                   [](auto v) { return v.index() == 0 ? TypeVar{Floating{}} : TypeVar{std::get<1>(std::move(v))}; });
}

ParseResult<CompoundType<NamedType>> type(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return construct<CompoundType<NamedType>>(
    choose(construct<TypeVar>(tuple(type)), construct<TypeVar>(borrow_type()), leaf_type()))(s, loc);
}

auto binding() {
  return seq(pattern, transform(maybe(seq(symbol(":"), type)), [](auto opt_pattern) {
               return opt_pattern ? std::move(*opt_pattern) : floating_type<NamedType>();
             }));
}

auto literal() {
  return transform_if(
    any(), "literal", [](std::string_view src, Token t) { return to_literal(t.type, src_sv(src, t.ref)); });
}

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>&, ParseLocation);

auto call() { return construct<UnTypedCallExpr>(seq(function_ident(), tuple(expr))); }

auto borrow_expr() { return construct<UnTypedBorrowExpr>(seq(symbol("&"), expr)); }

auto assignment() { return construct_with_ref<UnTypedAssignment>(seq(keyword("let"), binding(), symbol("="), expr)); }

auto scope() {
  return construct<UnTypedScopeExpr>(seq(symbol("{"), n(seq(assignment(), symbol(";"))), expr, symbol("}")));
}

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return transform(
    seq(construct_with_ref<UnTypedExpr>(choose(tuple(expr), scope(), call(), borrow_expr(), ident_expr(), literal())),
        n(seq(symbol("."), construct_with_ref<UnTypedExpr>(call())))),
    [](UnTypedExpr acc, std::vector<UnTypedExpr> chain) {
      return knot::accumulate(std::move(chain), std::move(acc), [](UnTypedExpr acc, UnTypedExpr next) {
        auto& call = std::get<UnTypedCallExpr>(next.v);
        call.parameters.insert(call.parameters.begin(), std::move(acc));
        next.ref.begin = acc.ref.begin;
        return next;
      });
    })(s, loc);
}

auto function_header() {
  return transform(
    seq(transform(tuple(seq(binding())),
                  [](const auto&, auto tokens, Slice s, auto bindings) {
                    const Slice char_slice{tokens[s.begin].ref.begin, tokens[s.end - 1].ref.end};
                    return std::tuple(
                      Pattern{transform_to_vec(std::move(bindings), [](auto&& b) { return std::move(std::get<0>(b)); }),
                              char_slice},
                      transform_to_vec(std::move(bindings), [](auto&& b) { return std::move(std::get<1>(b)); }));
                  }),
        symbol("->"),
        type),
    [](const auto&, auto tokens, Slice s, auto pattern, auto inputs, auto result_type) {
      return UnTypedHeader{std::move(pattern),
                           {tuple_type(std::move(inputs)), std::move(result_type)},
                           Slice{tokens[s.begin].ref.begin, tokens[s.end - 1].ref.end}};
    });
}

auto function() {
  return construct<UnTypedFunction>(
    seq(function_header(), choose(construct_with_ref<UnTypedExpr>(scope()), seq(symbol("="), expr))));
}

template <typename Parser>
ContextualResult<parser_result_t<std::string_view, Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  auto [parse_slice, value, error] = p(ParseState<std::string_view, Token>{src, tokens}, {});

  assert(value || error);

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    return tl::unexpected{std::vector<ContextualError>{
      {(error && error->second.pos < tokens.size()) ? tokens[error->second.pos].ref
                                                    : Slice{lex_end, lex_end == src.size() ? lex_end : lex_end + 1},
       error ? fmt::format("expected {}", error->first) : fmt::format("unknown token")}}};
  }
}

} // namespace

ContextualResult<UnTypedExpr> parse_expr(std::string_view src) { return parse_string(expr, src); }

ContextualResult<std::variant<UnTypedExpr, UnTypedAssignment>> parse_repl(std::string_view src) {
  return parse_string(choose(expr, assignment()), src);
}

ContextualResult<UnTypedFunction> parse_function(std::string_view src) { return parse_string(function(), src); }

ContextualResult<AST> parse(std::string_view src) {
  return parse_string(n(seq(keyword("fn"), ident_string(), function())), src);
}

ContextualResult<UnTypedHeader> parse_header(std::string_view src) { return parse_string(function_header(), src); }
ContextualResult<UnTypedAssignment> parse_assignment(std::string_view src) { return parse_string(assignment(), src); }
ContextualResult<Pattern> parse_pattern(std::string_view src) { return parse_string(pattern, src); }
ContextualResult<CompoundType<NamedType>> parse_type(std::string_view src) { return parse_string(type, src); }

} // namespace ooze
