#pragma once

#include "traits.h"

namespace ooze::pc {

struct ParseLocation {
  int pos = 0;
  int depth = 0;

  KNOT_ORDERED(ParseLocation);
};

template <typename Result>
struct ParseResult {
  using value_type = Result;

  Slice slice = {};
  std::optional<Result> value;
  std::optional<std::pair<std::string, ParseLocation>> error;

  explicit operator bool() const { return value.has_value(); }

  KNOT_ORDERED(ParseResult);
};

inline auto merge(std::optional<std::pair<std::string, ParseLocation>> x,
                  std::optional<std::pair<std::string, ParseLocation>> y) {
  if(x && y) {
    return std::tuple(-static_cast<int>(x->second.pos), x->second.depth) <=
               std::tuple(-static_cast<int>(y->second.pos), y->second.depth)
             ? x
             : y;
  } else {
    return x ? x : y;
  }
}

template <typename R>
ParseResult<R> passing_result(Slice s, R r, std::optional<std::pair<std::string, ParseLocation>> error = {}) {
  return {s, std::move(r), std::move(error)};
}

template <typename R>
ParseResult<R> failing_result(Slice s, std::optional<std::pair<std::string, ParseLocation>> error) {
  return {s, std::nullopt, std::move(error)};
}

template <typename S, typename T, typename P>
using parser_result_t =
  knot::type_t<decltype(value_type(invoke_result(knot::Type<P>{}, knot::TypeList<S&&, Span<T>, ParseLocation>{})))>;

namespace details {

template <typename S, typename T, typename R>
auto seq_helper(S&&, Span<T>, int, ParseResult<R> r) {
  if constexpr(std::tuple_size_v<R> == 1) {
    using R2 = std::tuple_element_t<0, R>;
    return r ? passing_result(r.slice, std::move(std::get<0>(*r.value)), std::move(r.error))
             : failing_result<R2>(r.slice, std::move(r.error));
  } else {
    return r;
  }
}

template <typename S, typename T, typename R, typename P, typename... Ps>
auto seq_helper(S&& s, Span<T> tokens, int depth, ParseResult<R> r, P p, Ps... ps) {
  if constexpr(std::is_same_v<std::tuple<>, std::decay_t<decltype(*p(s, tokens, ParseLocation{}).value)>>) {
    if(r) {
      auto r2 = p(s, tokens, {r.slice.end, depth + 1});
      return seq_helper(
        s,
        tokens,
        depth,
        ParseResult<R>{{r.slice.begin, r2.slice.end},
                       r2 ? std::move(r.value) : std::nullopt,
                       merge(std::move(r.error), std::move(r2.error))},
        ps...);
    } else {
      return seq_helper(s, tokens, depth, std::move(r), ps...);
    }
  } else {
    using R2 = decltype(std::tuple_cat(std::tuple(*r.value), std::tuple(*p(s, tokens, ParseLocation{}).value)));

    if(r) {
      auto r2 = p(s, tokens, {r.slice.end, depth + 1});
      return seq_helper(
        s,
        tokens,
        depth,
        ParseResult<R2>{
          {r.slice.begin, r2.slice.end},
          r2 ? std::optional(std::tuple_cat(std::tuple(std::move(*r.value)), std::tuple(std::move(*r2.value))))
             : std::nullopt,
          merge(std::move(r.error), std::move(r2.error))},
        ps...);
    } else {
      return seq_helper(s, tokens, depth, failing_result<R2>(r.slice, std::move(r.error)), ps...);
    }
  }
}

template <typename R, typename S, typename T>
auto choose_helper(
  knot::Type<R>, S&&, Span<T>, ParseLocation loc, std::optional<std::pair<std::string, ParseLocation>> err) {
  return failing_result<R>({loc.pos, loc.pos}, std::move(err));
}

template <typename R, typename S, typename T, typename P, typename... Ps>
auto choose_helper(knot::Type<R> result_type,
                   S&& s,
                   Span<T> tokens,
                   ParseLocation loc,
                   std::optional<std::pair<std::string, ParseLocation>> err,
                   P p,
                   Ps... ps) {
  auto r = p(s, tokens, {loc.pos, loc.depth + 1});
  return r ? passing_result(r.slice, R{std::move(*r.value)}, merge(std::move(err), std::move(r.error)))
           : choose_helper(result_type, s, tokens, loc, merge(std::move(err), std::move(r.error)), ps...);
}

} // namespace details

template <typename... Ps>
auto seq(Ps... ps) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    return details::seq_helper(s, tokens, loc.depth, passing_result({loc.pos, loc.pos}, std::tuple()), ps...);
  };
}

template <typename... Ps>
auto choose(Ps... ps) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    return details::choose_helper(
      as_variant(
        uniquify(map(knot::TypeList<decltype(*ps(s, tokens, loc).value)...>{}, [](auto t) { return decay(t); }))),
      s,
      tokens,
      loc,
      {},
      ps...);
  };
}

template <typename P>
auto n(P p) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    using T = typename decltype(tokens)::value_type;

    Slice slice{loc.pos, loc.pos};

    std::vector<std::decay_t<decltype(*p(s, tokens, loc).value)>> results;
    std::optional<std::pair<std::string, ParseLocation>> error;

    while(true) {
      auto r = p(s, tokens, {slice.end, loc.depth + 1});
      error = merge(std::move(error), std::move(r.error));
      if(!r) break;
      results.push_back(std::move(*r.value));
      slice.end = r.slice.end;
    }

    return passing_result(slice, std::move(results), std::move(error));
  };
}

inline auto any() {
  return [](auto&&, auto tokens, ParseLocation loc) {
    using T = typename decltype(tokens)::value_type;

    return loc.pos == tokens.size() ? failing_result<T>({loc.pos, loc.pos}, {{"token", loc}})
                                    : passing_result({loc.pos, loc.pos + 1}, std::move(tokens[loc.pos]));
  };
}

template <typename P>
auto maybe(P p) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    auto r = p(s, tokens, {loc.pos, loc.depth + 1});

    using T = typename decltype(tokens)::value_type;
    using R = std::optional<std::decay_t<decltype(*r.value)>>;

    return r ? passing_result(r.slice, std::optional(std::move(*r.value)), std::move(r.error))
             : passing_result({loc.pos, loc.pos}, std::optional(R{}), std::move(r.error));
  };
}

struct Invoker {
  template <typename F, typename S, typename Token, typename... Args>
  auto operator()(F f, S&& s, Span<Token> tokens, Slice slice, Args... args) {
    if constexpr(std::is_invocable_v<F, S&&, Span<Token>, Slice, Args...>) {
      return f(s, tokens, slice, std::move(args)...);
    } else if constexpr(std::is_invocable_v<F, S&&, Args...>) {
      return f(s, std::move(args)...);
    } else {
      return f(std::move(args)...);
    }
  }

  template <typename F, typename S, typename Token, typename... Args>
  auto operator()(F f, S&& s, Span<Token> tokens, Slice slice, std::tuple<Args...> arg) {
    return std::apply(*this, std::tuple_cat(std::tuple(std::move(f), std::ref(s), tokens, slice), std::move(arg)));
  }
};

template <typename P, typename F>
auto transform(P p, F f) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    auto r = p(s, tokens, {loc.pos, loc.depth + 1});
    return r ? passing_result(r.slice, Invoker{}(f, s, tokens, r.slice, std::move(*r.value)), std::move(r.error))
             : failing_result<decltype(Invoker{}(f, s, tokens, r.slice, std::move(*r.value)))>(
                 r.slice, std::move(r.error));
  };
}

template <typename P, typename F>
auto filter(P p, std::string msg, F f) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    auto r = p(s, tokens, {loc.pos, loc.depth + 1});
    return r && Invoker{}(f, s, tokens, r.slice, *r.value)
             ? r
             : decltype(r){{loc.pos, loc.pos}, std::nullopt, merge(std::move(r.error), {{msg, loc}})};
  };
}

template <typename P, typename F>
auto transform_if(P p, std::string msg, F f) {
  return [=](auto&& s, auto tokens, ParseLocation loc) {
    auto r = p(s, tokens, {loc.pos, loc.depth + 1});

    using R = std::decay_t<decltype(*Invoker{}(f, s, tokens, r.slice, *r.value))>;

    if(r) {
      std::optional<R> result = Invoker{}(f, s, tokens, r.slice, std::move(*r.value));
      return result ? passing_result(r.slice, std::move(*result), std::move(r.error))
                    : failing_result<R>(r.slice, merge(std::move(r.error), {{msg, loc}}));
    } else {
      return failing_result<R>(r.slice, std::move(r.error));
    }
  };
}

template <typename P>
auto nullify(P p) {
  return transform(p, [](auto&&...) { return std::tuple(); });
}

template <typename T, typename P>
auto construct(P p) {
  return transform(p, [](auto&&, auto, Slice, auto&&... ts) { return T{std::forward<decltype(ts)>(ts)...}; });
}

template <typename T>
auto constant(std::string msg, T t) {
  return nullify(filter(any(), std::move(msg), [t = std::move(t)](const auto&, const auto& t2) { return t == t2; }));
}

template <typename P, typename T, typename S = int>
auto parse(P p, Span<T> tokens, S&& s = {}) {
  auto r = p(s, tokens, {});
  return r && size(r.slice) == tokens.size()
           ? Result<parser_result_t<S, T, P>, std::optional<std::pair<std::string, ParseLocation>>>{std::move(*r.value)}
           : Failure{std::move(r.error)};
}

} // namespace ooze::pc
