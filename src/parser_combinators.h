#pragma once

namespace ooze::pc {

template <typename State, typename Token>
struct ParseState {
  using token_type = Token;
  using state_type = State;

  State state;
  Span<Token> tokens;
};

template <typename Result>
struct ParseResult {
  u32 pos = {};
  std::optional<Result> value;
  std::vector<std::pair<std::string, u32>> errors;

  explicit operator bool() const { return value.has_value(); }
};

template <typename T>
std::vector<T> merge(std::vector<T> a, std::vector<T> b) {
  a.insert(a.end(), std::make_move_iterator(b.begin()), std::make_move_iterator(b.end()));
  return a;
}

template <typename R>
ParseResult<R> passing_result(u32 pos, R r, std::vector<std::pair<std::string, u32>> errors = {}) {
  return {pos, std::move(r), std::move(errors)};
}

template <typename R>
ParseResult<R> failing_result(u32 pos, std::vector<std::pair<std::string, u32>> errors) {
  return {pos, std::nullopt, std::move(errors)};
}

template <typename State, typename Token, typename P>
using parser_result_t = std::decay_t<decltype(*std::declval<P>()(ParseState<State, Token>{}, 0).value)>;

namespace details {

template <typename S, typename R, typename T>
auto seq_helper(const ParseState<S, T>&, ParseResult<R> res) {
  if constexpr(std::tuple_size_v<R> == 1) {
    using R2 = std::tuple_element_t<0, R>;
    return res ? passing_result(res.pos, std::move(std::get<0>(*res.value)), std::move(res.errors))
               : failing_result<R2>(res.pos, std::move(res.errors));
  } else {
    return res;
  }
}

template <typename S, typename R, typename T, typename P, typename... Ps>
auto seq_helper(const ParseState<S, T>& s, ParseResult<R> res, P p, Ps... ps) {
  if constexpr(std::is_same_v<std::tuple<>, std::decay_t<decltype(*p(s, res.pos).value)>>) {
    if(res) {
      auto opt = p(s, res.pos);
      return seq_helper(s,
                        ParseResult<R>{opt.pos,
                                       opt ? std::move(res.value) : std::nullopt,
                                       merge(std::move(res.errors), std::move(opt.errors))},
                        ps...);
    } else {
      return seq_helper(s, std::move(res), ps...);
    }
  } else {
    using R2 = decltype(std::tuple_cat(std::tuple(*res.value), std::tuple(*p(s, res.pos).value)));

    if(res) {
      auto opt = p(s, res.pos);
      return seq_helper(s,
                        ParseResult<R2>{opt.pos,
                                        opt ? std::optional(std::tuple_cat(std::tuple(std::move(*res.value)),
                                                                           std::tuple(std::move(*opt.value))))
                                            : std::nullopt,
                                        merge(std::move(res.errors), std::move(opt.errors))},
                        ps...);
    } else {
      return seq_helper(s, failing_result<R2>(res.pos, std::move(res.errors)), ps...);
    }
  }
}

template <typename R, typename S, typename T>
auto choose_helper(const ParseState<S, T>&, u32 pos, std::vector<std::pair<std::string, u32>> errs) {
  return failing_result<R>(pos, std::move(errs));
}

template <typename R, typename S, typename T, typename P, typename... Ps>
auto choose_helper(const ParseState<S, T>& s, u32 pos, std::vector<std::pair<std::string, u32>> errs, P p, Ps... ps) {
  auto opt = p(s, pos);
  return opt ? passing_result(opt.pos, R{std::move(*opt.value)}, std::move(opt.errors))
             : choose_helper<R>(s, pos, merge(std::move(errs), std::move(opt.errors)), ps...);
}

} // namespace details

template <typename... Ps>
auto seq(Ps... ps) {
  return [=](const auto& s, u32 pos) { return details::seq_helper(s, passing_result(pos, std::tuple()), ps...); };
}

template <typename... Ps>
auto choose(Ps... ps) {
  return [=](const auto& s, u32 pos) {
    return details::choose_helper<std::variant<std::decay_t<decltype(*ps(s, pos).value)>...>>(s, pos, {}, ps...);
  };
}

template <typename P>
auto n(P p) {
  return [=](const auto& s, u32 pos) {
    using T = typename std::decay_t<decltype(s)>::token_type;

    std::vector<std::decay_t<decltype(*p(s, pos).value)>> results;
    std::vector<std::pair<std::string, u32>> errs;

    while(true) {
      auto opt = p(s, pos);
      errs = merge(std::move(errs), std::move(opt.errors));
      if(!opt) break;
      results.push_back(std::move(*opt.value));
      pos = opt.pos;
    }

    return passing_result(pos, std::move(results), std::move(errs));
  };
}

inline auto any() {
  return [](const auto& s, u32 pos) {
    using T = typename std::decay_t<decltype(s)>::token_type;

    return pos == s.tokens.size() ? failing_result<T>(pos, {{"anything", pos}})
                                  : passing_result(pos + 1, std::move(s.tokens[pos]));
  };
}

template <typename P>
auto maybe(P p) {
  return [=](const auto& s, u32 pos) {
    auto opt = p(s, pos);

    using T = typename std::decay_t<decltype(s)>::token_type;
    using R = std::optional<std::decay_t<decltype(*opt.value)>>;

    return opt ? passing_result(opt.pos, std::optional(std::move(*opt.value)), std::move(opt.errors))
               : passing_result(pos, std::optional(R{}), std::move(opt.errors));
  };
}

template <typename P, typename F>
auto transform(P p, F f) {
  return [=](const auto& s, u32 pos) {
    auto opt = p(s, pos);

    using T = typename std::decay_t<decltype(s)>::token_type;

    if constexpr(knot::is_tuple_like(decay(knot::Type<decltype(*opt.value)>{}))) {
      using R = std::decay_t<decltype(std::apply(f, std::tuple_cat(std::tie(s.state), std::move(*opt.value))))>;
      return opt ? passing_result(opt.pos,
                                  std::apply(f, std::tuple_cat(std::tie(s.state), std::move(*opt.value))),
                                  std::move(opt.errors))
                 : failing_result<R>(opt.pos, std::move(opt.errors));
    } else {
      using R = std::decay_t<decltype(std::invoke(f, s.state, std::move(*opt.value)))>;
      return opt ? passing_result(opt.pos, std::invoke(f, s.state, std::move(*opt.value)), std::move(opt.errors))
                 : failing_result<R>(opt.pos, std::move(opt.errors));
    }
  };
}

template <typename P, typename F>
auto filter(P p, std::string msg, F f) {
  return [=](const auto& s, u32 pos) {
    auto opt = p(s, pos);

    return opt && f(s.state, *opt.value) ? opt : decltype(opt){pos, std::nullopt, {{msg, pos}}};
  };
}

template <typename P, typename F>
auto transform_if(P p, std::string msg, F f) {
  return [=](const auto& s, u32 pos) {
    auto opt = p(s, pos);

    using R = std::decay_t<decltype(*f(s.state, *opt.value))>;

    if(opt) {
      ParseResult<R> r{opt.pos, f(s.state, *opt.value), std::move(opt.errors)};

      if(!r) {
        r.errors.emplace_back(msg, pos);
      }

      return r;
    } else {
      return failing_result<R>(opt.pos, std::move(opt.errors));
    }
  };
}

template <typename P>
auto nullify(P p) {
  return [=](const auto& s, u32 pos) {
    auto opt = p(s, pos);
    return opt ? passing_result(opt.pos, std::tuple(), std::move(opt.errors))
               : failing_result<std::tuple<>>(opt.pos, std::move(opt.errors));
  };
}

template <typename T, typename P>
auto construct(P p) {
  return transform(p, [](const auto&, auto&&... ts) { return T{std::forward<decltype(ts)>(ts)...}; });
}

template <typename T>
auto constant(std::string msg, T t) {
  return nullify(filter(any(), std::move(msg), [t = std::move(t)](const auto&, const auto& t2) { return t == t2; }));
}

template <typename Token, typename Parser, typename State = int>
auto parse(const Parser& p, Span<Token> tokens, State state = {}) {
  auto res = p(ParseState<State, Token>{std::move(state), tokens}, 0);

  using R = std::decay_t<decltype(*res.value)>;

  std::sort(
    res.errors.begin(), res.errors.end(), [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; });

  res.errors.erase(std::unique(res.errors.begin(), res.errors.end()), res.errors.end());

  return (res && res.pos == tokens.size())
           ? tl::expected<R, std::vector<std::pair<std::string, u32>>>{std::move(*res.value)}
           : tl::unexpected{std::move(res.errors)};
}

} // namespace ooze::pc
