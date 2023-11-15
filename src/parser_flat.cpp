#include "pch.h"

#include "lexer.h"
#include "parser_combinators.h"
#include "parser_flat.h"

#include <charconv>

namespace ooze {

using namespace pc;

namespace {

struct State {
  AST ast;
  ASTTypes types;
  std::string_view src;
};

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

Slice char_slice(Span<Token> tokens, Slice token_slice) {
  return token_slice.begin == token_slice.end
           ? Slice{}
           : Slice{tokens[token_slice.begin].ref.begin, tokens[token_slice.end - 1].ref.end};
}

Slice join(Slice x, Slice y) { return {x.begin, y.end}; }

auto token_parser(TokenType type) {
  return transform(filter(any(), "token", [=](Token t) { return t.type == type; }), [](State& s, Token t) {
    return std::string_view(s.src.begin() + t.ref.begin, t.ref.end - t.ref.begin);
  });
}

auto symbol(std::string_view sv) {
  return nullify(
    filter(token_parser(TokenType::Symbol), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

auto keyword(std::string_view sv) {
  return nullify(
    filter(token_parser(TokenType::Keyword), fmt::format("'{}'", sv), [=](std::string_view t) { return t == sv; }));
}

template <typename P>
auto debug(std::string_view msg, P p) {
  return transform(p, [=](auto v) {
    fmt::print("{}\n", msg);
    return v;
  });
}

template <typename P>
auto tuple(P p) {
  return transform(seq(symbol("("), maybe(seq(p, n(seq(symbol(","), p)))), symbol(")")), [](auto opt) {
    if(opt) {
      auto&& [first, vec] = std::move(*opt);
      vec.insert(vec.begin(), std::move(first));
      return std::move(vec);
    } else {
      return std::decay_t<decltype(std::get<1>(*opt))>{};
    }
  });
}

struct ASTAppender {
  ASTTag _tag;
  TypeRef _type;

  ASTID operator()(State& s, Span<Token> tokens, Slice ref) const { return (*this)(s, tokens, ref, Span<ASTID>{}); }

  template <typename... IDs>
  ASTID operator()(State& s, Span<Token> tokens, Slice ref, ASTID id, IDs... ids) const {
    return (*this)(s, tokens, ref, std::array{id, ids...});
  }

  ASTID operator()(State& s, Span<Token> tokens, Slice ref, Span<ASTID> ids) const {
    return (*this)(s, char_slice(tokens, ref), ids);
  }

  ASTID operator()(State& s, Slice char_slice, Span<ASTID> ids) const {
    s.ast.srcs.push_back(char_slice);
    s.types.ast_types.push_back(_type);
    return s.ast.forest.append_root_post_order(_tag, ids);
  }
};

struct TypeAppender {
  TypeTag _tag;

  TypeRef operator()(State& s, Span<Token> tokens, Slice ref) const { return (*this)(s, tokens, ref, Span<TypeRef>{}); }

  template <typename... IDs>
  TypeRef operator()(State& s, Span<Token> tokens, Slice ref, TypeRef id, IDs... ids) const {
    return (*this)(s, tokens, ref, std::array{id, ids...});
  }

  TypeRef operator()(State& s, Span<Token> tokens, Slice ref, Span<TypeRef> ids) const {
    s.types.srcs.push_back(char_slice(tokens, ref));
    s.types.tags.push_back(_tag);
    const TypeRef n = s.types.graph.add_node();
    for(TypeRef child : ids) {
      s.types.graph.add_fanout_to_last_node(child);
    }
    return n;
  }
};

auto leaf_ast(TokenType type, ASTTag tag) { return transform(nullify(token_parser(type)), ASTAppender{tag}); }

auto ident() { return leaf_ast(TokenType::Ident, ASTTag::PatternIdent); }

ParseResult<ASTID> pattern(State& s, Span<Token> tokens, ParseLocation loc) {
  return choose(transform(tuple(pattern), ASTAppender{ASTTag::PatternTuple}),
                leaf_ast(TokenType::Underscore, ASTTag::PatternWildCard),
                ident())(s, tokens, loc);
}

ParseResult<TypeRef> type(State& s, Span<Token> tokens, ParseLocation loc) {
  const auto tuple_type = []() { return transform(tuple(type), TypeAppender{TypeTag::Tuple}); };

  const auto leaf_type = [](TokenType type, TypeTag tag) {
    return transform(nullify(token_parser(type)), TypeAppender{tag});
  };

  return choose(tuple_type(),
                transform(seq(symbol("&"), type), TypeAppender{TypeTag::Borrow}),
                transform(seq(keyword("fn"), tuple_type(), symbol("->"), type), TypeAppender{TypeTag::Fn}),
                leaf_type(TokenType::Underscore, TypeTag::Floating),
                leaf_type(TokenType::Ident, TypeTag::Leaf))(s, tokens, loc);
}

auto binding() {
  return transform(
    seq(pattern, maybe(seq(symbol(":"), type))), [&](State& s, ASTID pattern, std::optional<TypeRef> opt_type) {
      s.types.ast_types[pattern.get()] = opt_type.value_or(TypeRef{});
      return pattern;
    });
}

auto literal() {
  return transform_if(any(), "literal", [](State& s, Span<Token> tokens, Slice ref, Token t) -> std::optional<ASTID> {
    if(auto opt_lit = to_literal(t.type, sv(s.src, char_slice(tokens, ref))); opt_lit) {
      const ASTID id = ASTAppender{ASTTag::ExprLiteral}(s, tokens, ref);
      s.ast.literals.emplace_back(id, std::move(*opt_lit));
      return id;
    } else {
      return std::nullopt;
    }
  });
}

ParseResult<ASTID> expr(State&, Span<Token>, ParseLocation);
ParseResult<ASTID> call_expr(State&, Span<Token>, ParseLocation);
ParseResult<ASTID> non_call_expr(State&, Span<Token>, ParseLocation);

auto assignment() {
  return transform(seq(keyword("let"), binding(), symbol("="), expr),
                   [](State& s, Span<Token> tokens, Slice ref, ASTID pattern, ASTID expr) {
                     return ASTAppender{ASTTag::Assignment}(s, tokens, ref, std::array{pattern, expr});
                   });
}

auto tuple_expr() { return transform(tuple(expr), ASTAppender{ASTTag::ExprTuple}); }

ParseResult<ASTID> expr(State& s, Span<Token> tokens, ParseLocation loc) {
  return transform(
    seq(call_expr, n(seq(symbol("."), transform(seq(non_call_expr, tuple_expr()), ASTAppender{ASTTag::ExprCall})))),
    [](State& s, ASTID acc, std::vector<ASTID> chain) {
      return knot::accumulate(std::move(chain), std::move(acc), [&](ASTID acc, ASTID call) {
        const ASTID args = *s.ast.forest.next_sibling(*s.ast.forest.first_child(call));
        s.ast.forest.move_first_child(args, acc);
        s.ast.srcs[call.get()] = join(s.ast.srcs[acc.get()], s.ast.srcs[call.get()]);
        return call;
      });
    })(s, tokens, loc);
}

ParseResult<ASTID> call_expr(State& s, Span<Token> tokens, ParseLocation loc) {
  return transform(seq(non_call_expr, n(tuple_expr())), [](State& s, ASTID callee, std::vector<ASTID> arg_sets) {
    return knot::accumulate(std::move(arg_sets), std::move(callee), [&](ASTID acc, ASTID args) {
      return ASTAppender{ASTTag::ExprCall}(
        s, join(s.ast.srcs[acc.get()], s.ast.srcs[args.get()]), std::array{acc, args});
    });
  })(s, tokens, loc);
}

auto scope() {
  return transform(
    seq(symbol("{"), n(seq(assignment(), symbol(";"))), expr, symbol("}")),
    [](State& s, std::vector<ASTID> assignments, ASTID expr) {
      return std::accumulate(assignments.rbegin(), assignments.rend(), expr, [&](ASTID acc, ASTID assign) {
        return ASTAppender{ASTTag::ExprWith}(
          s, join(s.ast.srcs[assign.get()], s.ast.srcs[acc.get()]), std::array{assign, acc});
      });
    });
}

ParseResult<ASTID> non_call_expr(State& s, Span<Token> tokens, ParseLocation loc) {
  return choose(
    tuple_expr(),
    scope(),
    transform(seq(keyword("select"), expr, scope(), keyword("else"), scope()), ASTAppender{ASTTag::ExprSelect}),
    transform(seq(symbol("&"), expr), ASTAppender{ASTTag::ExprBorrow}),
    leaf_ast(TokenType::Ident, ASTTag::ExprIdent),
    literal())(s, tokens, loc);
}

auto function() {
  return transform(seq(transform(tuple(binding()), ASTAppender{ASTTag::PatternTuple}),
                       symbol("->"),
                       type,
                       choose(scope(), seq(symbol("="), expr))),
                   [](State& s, Span<Token> tokens, Slice ref, ASTID pattern, TypeRef type, ASTID expr) {
                     s.types.ast_types[expr.get()] = type;
                     return ASTAppender{ASTTag::Fn}(s, tokens, ref, pattern, expr);
                   });
}

auto root() { return n(transform(seq(keyword("fn"), ident(), function()), ASTAppender{ASTTag::Assignment})); }

template <typename Parser>
ContextualResult<std::tuple<AST, ASTTypes>> parse_ast(Parser p, std::string_view src) {
  const auto [tokens, lex_end] = lex(src);

  State state = {AST{}, ASTTypes{}, src};
  auto [parse_slice, value, error] = p(state, Span<Token>{tokens}, {});

  assert(value || error);

  if(value && size(parse_slice) == tokens.size() && lex_end == src.size()) {
    return std::tuple(std::move(state.ast), std::move(state.types));
  } else {
    return Failure{std::vector<ContextualError>{
      {(error && error->second.pos < tokens.size()) ? tokens[error->second.pos].ref
                                                    : Slice{lex_end, lex_end == src.size() ? lex_end : lex_end + 1},
       error ? fmt::format("expected {}", error->first) : fmt::format("unknown token")}}};
  }
}

} // namespace

ContextualResult<std::tuple<AST, ASTTypes>> parse_expr2(std::string_view src) { return parse_ast(expr, src); }
ContextualResult<std::tuple<AST, ASTTypes>> parse_repl2(std::string_view src);
ContextualResult<std::tuple<AST, ASTTypes>> parse_function2(std::string_view src) { return parse_ast(function(), src); }
ContextualResult<std::tuple<AST, ASTTypes>> parse2(std::string_view src) { return parse_ast(root(), src); }

// Exposed for unit testing
ContextualResult<std::tuple<AST, ASTTypes>> parse_binding2(std::string_view src) { return parse_ast(binding(), src); }
ContextualResult<std::tuple<AST, ASTTypes>> parse_assignment2(std::string_view src) {
  return parse_ast(assignment(), src);
}
ContextualResult<std::tuple<AST, ASTTypes>> parse_type2(std::string_view src) { return parse_ast(type, src); }
ContextualResult<std::tuple<AST, ASTTypes>> parse_pattern2(std::string_view src) { return parse_ast(pattern, src); }

} // namespace ooze
