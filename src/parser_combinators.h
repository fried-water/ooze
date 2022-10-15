#pragma once

#include "traits.h"

namespace ooze::pc {

template <typename State, typename Token>
struct ParseState {
  using token_type = Token;
  using state_type = State;

  State state;
  Span<Token> tokens;
};

struct ParseLocation {
  u32 pos = 0;
  u32 depth = 0;

  KNOT_ORDERED(ParseLocation);
};

template <typename Result>
struct ParseResult {
  Slice slice = {};
  std::optional<Result> value;
  std::vector<std::pair<std::string, ParseLocation>> errors;

  explicit operator bool() const { return value.has_value(); }

  KNOT_ORDERED(ParseResult);
};

template <typename T>
std::vector<T> merge(std::vector<T> a, std::vector<T> b) {
  a.insert(a.end(), std::make_move_iterator(b.begin()), std::make_move_iterator(b.end()));
  return a;
}

template <typename R>
ParseResult<R> passing_result(Slice s, R r, std::vector<std::pair<std::string, ParseLocation>> errors = {}) {
  return {s, std::move(r), std::move(errors)};
}

template <typename R>
ParseResult<R> failing_result(Slice s, std::vector<std::pair<std::string, ParseLocation>> errors) {
  return {s, std::nullopt, std::move(errors)};
}

template <typename State, typename Token, typename P>
using parser_result_t = std::decay_t<decltype(*std::declval<P>()(ParseState<State, Token>{}, ParseLocation{}).value)>;

namespace details {

template <typename S, typename R, typename T>
auto seq_helper(const ParseState<S, T>&, u32, ParseResult<R> r) {
  if constexpr(std::tuple_size_v<R> == 1) {
    using R2 = std::tuple_element_t<0, R>;
    return r ? passing_result(r.slice, std::move(std::get<0>(*r.value)), std::move(r.errors))
             : failing_result<R2>(r.slice, std::move(r.errors));
  } else {
    return r;
  }
}

template <typename S, typename R, typename T, typename P, typename... Ps>
auto seq_helper(const ParseState<S, T>& s, u32 depth, ParseResult<R> r, P p, Ps... ps) {
  if constexpr(std::is_same_v<std::tuple<>, std::decay_t<decltype(*p(s, ParseLocation{}).value)>>) {
    if(r) {
      auto r2 = p(s, {r.slice.end, depth + 1});
      return seq_helper(s,
                        depth,
                        ParseResult<R>{{r.slice.begin, r2.slice.end},
                                       r2 ? std::move(r.value) : std::nullopt,
                                       merge(std::move(r.errors), std::move(r2.errors))},
                        ps...);
    } else {
      return seq_helper(s, depth, std::move(r), ps...);
    }
  } else {
    using R2 = decltype(std::tuple_cat(std::tuple(*r.value), std::tuple(*p(s, ParseLocation{}).value)));

    if(r) {
      auto r2 = p(s, {r.slice.end, depth + 1});
      return seq_helper(s,
                        depth,
                        ParseResult<R2>{{r.slice.begin, r2.slice.end},
                                        r2 ? std::optional(std::tuple_cat(std::tuple(std::move(*r.value)),
                                                                          std::tuple(std::move(*r2.value))))
                                           : std::nullopt,
                                        merge(std::move(r.errors), std::move(r2.errors))},
                        ps...);
    } else {
      return seq_helper(s, depth, failing_result<R2>(r.slice, std::move(r.errors)), ps...);
    }
  }
}

template <typename R, typename S, typename T>
auto choose_helper(const ParseState<S, T>&,
                   ParseLocation loc,
                   std::vector<std::pair<std::string, ParseLocation>> errs) {
  return failing_result<R>({loc.pos, loc.pos}, std::move(errs));
}

template <typename R, typename S, typename T, typename P, typename... Ps>
auto choose_helper(const ParseState<S, T>& s,
                   ParseLocation loc,
                   std::vector<std::pair<std::string, ParseLocation>> errs,
                   P p,
                   Ps... ps) {
  auto r = p(s, {loc.pos, loc.depth + 1});
  return r ? passing_result(r.slice, R{std::move(*r.value)}, merge(std::move(errs), std::move(r.errors)))
           : choose_helper<R>(s, loc, merge(std::move(errs), std::move(r.errors)), ps...);
}

} // namespace details

template <typename... Ps>
auto seq(Ps... ps) {
  return [=](const auto& s, ParseLocation loc) {
    return details::seq_helper(s, loc.depth, passing_result({loc.pos, loc.pos}, std::tuple()), ps...);
  };
}

template <typename... Ps>
auto choose(Ps... ps) {
  return [=](const auto& s, ParseLocation loc) {
    constexpr auto list = knot::TypeList<decltype(*ps(s, loc).value)...>{};
    constexpr auto decayed_list = knot::map(list, [](auto t) { return decay(t); });
    constexpr auto variant_type = as_variant(uniquify(decayed_list));
    return details::choose_helper<knot::type_t<decltype(variant_type)>>(s, loc, {}, ps...);
  };
}

template <typename P>
auto n(P p) {
  return [=](const auto& s, ParseLocation loc) {
    using T = typename std::decay_t<decltype(s)>::token_type;

    Slice slice{loc.pos, loc.pos};

    std::vector<std::decay_t<decltype(*p(s, loc).value)>> results;
    std::vector<std::pair<std::string, ParseLocation>> errs;

    while(true) {
      auto r = p(s, {slice.end, loc.depth + 1});
      errs = merge(std::move(errs), std::move(r.errors));
      if(!r) break;
      results.push_back(std::move(*r.value));
      slice.end = r.slice.end;
    }

    return passing_result(slice, std::move(results), std::move(errs));
  };
}

inline auto any() {
  return [](const auto& s, ParseLocation loc) {
    using T = typename std::decay_t<decltype(s)>::token_type;

    return loc.pos == s.tokens.size() ? failing_result<T>({loc.pos, loc.pos}, {{"anything", loc}})
                                      : passing_result({loc.pos, loc.pos + 1}, std::move(s.tokens[loc.pos]));
  };
}

template <typename P>
auto maybe(P p) {
  return [=](const auto& s, ParseLocation loc) {
    auto r = p(s, {loc.pos, loc.depth + 1});

    using T = typename std::decay_t<decltype(s)>::token_type;
    using R = std::optional<std::decay_t<decltype(*r.value)>>;

    return r ? passing_result(r.slice, std::optional(std::move(*r.value)), std::move(r.errors))
             : passing_result({loc.pos, loc.pos}, std::optional(R{}), std::move(r.errors));
  };
}

struct Invoker {
  template <typename F, typename S, typename Token, typename... Args>
  auto operator()(F f, const S& s, Span<Token> tokens, Slice slice, Args... args) {
    if constexpr(std::is_invocable_v<F, const S&, Span<Token>, Slice, Args...>) {
      return f(s, tokens, slice, std::move(args)...);
    } else if constexpr(std::is_invocable_v<F, const S&, Args...>) {
      return f(s, std::move(args)...);
    } else {
      return f(std::move(args)...);
    }
  }

  template <typename F, typename S, typename Token, typename... Args>
  auto operator()(F f, const S& s, Span<Token> tokens, Slice slice, std::tuple<Args...> arg) {
    return std::apply(*this, std::tuple_cat(std::tuple(std::move(f), std::cref(s), tokens, slice), std::move(arg)));
  }
};

template <typename P, typename F>
auto transform(P p, F f) {
  return [=](const auto& s, ParseLocation loc) {
    auto r = p(s, {loc.pos, loc.depth + 1});
    return r ? passing_result(
                 r.slice, Invoker{}(f, s.state, s.tokens, r.slice, std::move(*r.value)), std::move(r.errors))
             : failing_result<decltype(Invoker{}(f, s.state, s.tokens, r.slice, *r.value))>(r.slice,
                                                                                            std::move(r.errors));
  };
}

template <typename P, typename F>
auto filter(P p, std::string msg, F f) {
  return [=](const auto& s, ParseLocation loc) {
    auto r = p(s, {loc.pos, loc.depth + 1});
    return r && Invoker{}(f, s.state, s.tokens, r.slice, *r.value)
             ? r
             : decltype(r){{loc.pos, loc.pos}, std::nullopt, {{msg, loc}}};
  };
}

template <typename P, typename F>
auto transform_if(P p, std::string msg, F f) {
  return [=](const auto& s, ParseLocation loc) {
    auto r = p(s, {loc.pos, loc.depth + 1});

    using R = std::decay_t<decltype(*Invoker{}(f, s.state, s.tokens, r.slice, *r.value))>;

    if(r) {
      ParseResult<R> r2{r.slice, Invoker{}(f, s.state, s.tokens, r.slice, std::move(*r.value)), std::move(r.errors)};

      if(!r2) {
        r2.errors.emplace_back(msg, loc);
      }

      return r2;
    } else {
      return failing_result<R>(r.slice, std::move(r.errors));
    }
  };
}

template <typename P>
auto nullify(P p) {
  return [=](const auto& s, ParseLocation loc) {
    auto r = p(s, {loc.pos, loc.depth + 1});
    return r ? passing_result(r.slice, std::tuple(), std::move(r.errors))
             : failing_result<std::tuple<>>(r.slice, std::move(r.errors));
  };
}

template <typename T, typename P>
auto construct(P p) {
  return transform(p, [](const auto&, auto, Slice, auto&&... ts) { return T{std::forward<decltype(ts)>(ts)...}; });
}

template <typename T>
auto constant(std::string msg, T t) {
  return nullify(filter(any(), std::move(msg), [t = std::move(t)](const auto&, const auto& t2) { return t == t2; }));
}

template <typename P, typename T, typename S = int>
auto parse(P p, Span<T> tokens, S s = {}) {
  auto r = p(ParseState<S, T>{s, tokens}, {});
  return r && size(r.slice) == tokens.size()
           ? tl::expected<parser_result_t<S, T, P>, std::vector<std::pair<std::string, ParseLocation>>>{std::move(
               *r.value)}
           : tl::unexpected{std::move(r.errors)};
}

} // namespace ooze::pc
