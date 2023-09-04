#include "pch.h"

#include "lexer.h"
#include "parser.h"
#include "parser_combinators.h"

#include <charconv>

namespace ooze {

using namespace pc;
using namespace ast;

namespace {

template <typename T>
std::optional<T> from_sv(std::string_view sv) {
  T value;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), value);
  return result.ec == std::errc() ? std::optional(value) : std::nullopt;
}

std::optional<Literal> to_literal(TokenType type, std::string_view sv) {
  switch(type) {
  case TokenType::Whitespace:
  case TokenType::Comment:
  case TokenType::Keyword:
  case TokenType::Underscore:
  case TokenType::Ident:
  case TokenType::Symbol: return std::nullopt;
  case TokenType::LiteralInt: {
    const auto suffix_pos =
      std::distance(sv.begin(), std::find_if(sv.begin(), sv.end(), [](char c) { return c == 'u' || c == 'i'; }));

    const std::string_view number(sv.data(), suffix_pos);
    const std::string_view suffix(sv.data() + suffix_pos, sv.size() - suffix_pos);

    if(suffix == "i8") return Literal{*from_sv<i8>(number)};
    if(suffix == "i16") return Literal{*from_sv<i16>(number)};
    if(suffix == "i32") return Literal{*from_sv<i32>(number)};
    if(suffix == "i64") return Literal{*from_sv<i64>(number)};
    if(suffix == "u8") return Literal{*from_sv<u8>(number)};
    if(suffix == "u16") return Literal{*from_sv<u16>(number)};
    if(suffix == "u32") return Literal{*from_sv<u32>(number)};
    if(suffix == "u64") return Literal{*from_sv<u64>(number)};

    return Literal{*from_sv<i32>(number)};
  }
  case TokenType::LiteralFloat:
    return sv.back() == 'f' ? Literal{static_cast<f32>(atof(std::string(sv.begin(), sv.end() - 1).c_str()))}
                            : Literal{atof(std::string(sv.begin(), sv.end()).c_str())};
  case TokenType::LiteralBool: return Literal{sv[0] == 't'};
  case TokenType::LiteralString: return Literal{std::string(sv.begin() + 1, sv.end() - 1)};
  }
}

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

Slice char_slice(Span<Token> tokens, Slice token_slice) {
  return {tokens[token_slice.begin].ref.begin, tokens[token_slice.end - 1].ref.end};
}

template <typename T, typename P>
auto construct_with_ref(P p) {
  return transform(p, [](const auto&, auto tokens, Slice s, auto&&... ts) {
    return T{std::forward<decltype(ts)>(ts)..., char_slice(tokens, s)};
  });
}

template <typename T, typename P>
auto construct_with_type_ref(P p) {
  return transform(p, [](const auto&, auto tokens, Slice s, auto t) {
    return T{std::move(t), floating_type<NamedType>(), char_slice(tokens, s)};
  });
}

auto ident_string() { return construct<std::string>(token_parser(TokenType::Ident)); }
auto ident() { return construct<Ident>(ident_string()); }
auto type_ident() { return construct<NamedType>(ident_string()); }

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

  return transform(
    seq(symbol("("), choose(symbol(")"), seq(n(seq(p, symbol(","))), p, symbol(")")))), [](auto v) -> std::vector<T> {
      if(v.index() == 0) {
        return {};
      } else {
        auto [vec, last] = split_vec_from_remaining(std::move(std::get<1>(v)));
        vec.push_back(std::move(last));
        return std::move(vec);
      }
    });
}

ParseResult<UnTypedPattern> pattern(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return construct_with_type_ref<UnTypedPattern>(choose(tuple(pattern), construct<WildCard>(underscore()), ident()))(
    s, loc);
}

using TypeVar =
  std::variant<std::vector<Type<NamedType>>, FunctionType<NamedType>, Floating, Borrow<NamedType>, NamedType>;

ParseResult<Type<NamedType>> type(const ParseState<std::string_view, Token>& s, ParseLocation loc);

auto borrow_type() { return construct<Borrow<NamedType>>(seq(symbol("&"), type)); }

auto fn_type() {
  return construct<FunctionType<NamedType>>(
    seq(keyword("fn"), construct_with_ref<Type<NamedType>>(tuple(type)), symbol("->"), type));
}

auto leaf_type() {
  return transform(choose(underscore(), type_ident()),
                   [](auto v) { return v.index() == 0 ? TypeVar{Floating{}} : TypeVar{std::get<1>(std::move(v))}; });
}

ParseResult<Type<NamedType>> type(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return construct_with_ref<Type<NamedType>>(choose(
    construct<TypeVar>(tuple(type)), construct<TypeVar>(borrow_type()), construct<TypeVar>(fn_type()), leaf_type()))(
    s, loc);
}

auto binding() {
  return transform(seq(pattern, maybe(seq(symbol(":"), type))), [](auto pattern, auto opt_type) {
    if(opt_type) {
      pattern.type = std::move(*opt_type);
    }
    return pattern;
  });
}

auto literal() {
  return transform_if(any(), "literal", [](std::string_view src, Token t) {
    return to_literal(t.type, src_sv(src, t.ref));
  });
}

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>&, ParseLocation);
ParseResult<UnTypedExpr> call_expr(const ParseState<std::string_view, Token>&, ParseLocation);
ParseResult<UnTypedExpr> non_call_expr(const ParseState<std::string_view, Token>&, ParseLocation);

ParseResult<UnTypedExpr> expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return transform(
    seq(call_expr,
        n(seq(symbol("."),
              construct_with_type_ref<UnTypedExpr>(
                construct<UnTypedCallExpr>(seq(non_call_expr, construct_with_type_ref<UnTypedExpr>(tuple(expr)))))))),
    [](UnTypedExpr acc, std::vector<UnTypedExpr> chain) {
      return knot::accumulate(std::move(chain), std::move(acc), [](auto acc, auto call) {
        auto& args = std::get<std::vector<UnTypedExpr>>(std::get<UnTypedCallExpr>(call.v).arg->v);
        args.insert(args.begin(), std::move(acc));
        call.ref.begin = acc.ref.begin;
        return call;
      });
    })(s, loc);
}

ParseResult<UnTypedExpr> call_expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return transform(
    seq(non_call_expr, n(construct_with_type_ref<UnTypedExpr>(tuple(expr)))),
    [](UnTypedExpr callee, std::vector<UnTypedExpr> arg_sets) {
      return knot::accumulate(std::move(arg_sets), std::move(callee), [](UnTypedExpr acc, UnTypedExpr args) {
        return UnTypedExpr{
          UnTypedCallExpr{std::move(acc), std::move(args)}, floating_type<NamedType>(), {acc.ref.begin, args.ref.end}};
      });
    })(s, loc);
}

auto borrow_expr() { return construct<UnTypedBorrowExpr>(seq(symbol("&"), expr)); }

auto assignment() {
  return transform(seq(keyword("let"), binding(), symbol("="), expr), [](UnTypedPattern pattern, UnTypedExpr expr) {
    expr.type = pattern.type;
    return UnTypedAssignment{std::move(pattern), std::move(expr)};
  });
}

auto scope() {
  return construct<UnTypedScopeExpr>(seq(symbol("{"), n(seq(assignment(), symbol(";"))), expr, symbol("}")));
}

auto select_expr() {
  return construct<UnTypedSelectExpr>(
    seq(keyword("select"),
        expr,
        construct_with_type_ref<UnTypedExpr>(scope()),
        keyword("else"),
        construct_with_type_ref<UnTypedExpr>(scope())));
}

ParseResult<UnTypedExpr> non_call_expr(const ParseState<std::string_view, Token>& s, ParseLocation loc) {
  return construct_with_type_ref<UnTypedExpr>(
    transform(choose(tuple(expr), scope(), select_expr(), borrow_expr(), ident(), literal()), [](auto v) {
      return std::visit([](auto&& ele) { return ast::ExprVariant<NamedType>{std::move(ele)}; }, std::move(v));
    }))(s, loc);
}

auto function() {
  return transform(seq(construct_with_type_ref<UnTypedPattern>(tuple(seq(binding()))),
                       symbol("->"),
                       type,
                       choose(construct_with_type_ref<UnTypedExpr>(scope()), seq(symbol("="), expr))),
                   [](UnTypedPattern pattern, Type<NamedType> output_type, UnTypedExpr expr) {
                     expr.type = std::move(output_type);
                     return UnTypedFunction{std::move(pattern), std::move(expr)};
                   });
}

template <typename Parser>
ContextualResult<parser_result_t<std::string_view, Token, Parser>> parse_string(Parser p, const std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  auto [parse_slice, value, error] = p(ParseState<std::string_view, Token>{src, tokens}, {});

  assert(value || error);

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::move(*value);
  } else {
    return Failure{std::vector<ContextualError>{
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

ContextualResult<UnTypedAST> parse(std::string_view src) {
  return parse_string(n(seq(keyword("fn"), ident_string(), function())), src);
}

ContextualResult<UnTypedAssignment> parse_assignment(std::string_view src) { return parse_string(assignment(), src); }
ContextualResult<UnTypedPattern> parse_pattern(std::string_view src) { return parse_string(pattern, src); }
ContextualResult<Type<NamedType>> parse_type(std::string_view src) { return parse_string(type, src); }

} // namespace ooze
