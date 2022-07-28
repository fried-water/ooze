#pragma once

namespace ooze::pc {

template <typename R, typename Token>
struct ParseResult {
  Span<Token> tokens;
  std::optional<R> value;
  std::vector<std::pair<std::string, const Token*>> errors;

  explicit operator bool() const { return value.has_value(); }
};

template <typename T>
std::vector<T> merge(std::vector<T> a, std::vector<T> b) {
  a.insert(a.end(), std::make_move_iterator(b.begin()), std::make_move_iterator(b.end()));
  return a;
}

template <typename R, typename T>
ParseResult<R, T> passing_result(Span<T> s, R r, std::vector<std::pair<std::string, const T*>> errors = {}) {
  return {s, std::move(r), std::move(errors)};
}

template <typename R, typename T>
ParseResult<R, T> failing_result(Span<T> s, std::vector<std::pair<std::string, const T*>> errors) {
  return {s, std::nullopt, std::move(errors)};
}

template <typename Token, typename P>
using parser_result_t = std::decay_t<decltype(*std::declval<P>()(Span<Token>{}).value)>;

namespace details {

template <typename R, typename T>
auto seq_helper(ParseResult<R, T> res) {
  if constexpr(std::tuple_size_v<R> == 1) {
    using R2 = std::tuple_element_t<0, R>;
    return res ? passing_result(res.tokens, std::move(std::get<0>(*res.value)), std::move(res.errors))
               : failing_result<R2>(res.tokens, std::move(res.errors));
  } else {
    return res;
  }
}

template <typename R, typename T, typename P, typename... Ps>
auto seq_helper(ParseResult<R, T> res, P p, Ps... ps) {
  if constexpr(std::is_same_v<std::tuple<>, std::decay_t<decltype(*p(res.tokens).value)>>) {
    if(res) {
      auto opt = p(res.tokens);
      return seq_helper(ParseResult<R, T>{opt.tokens,
                                          opt ? std::move(res.value) : std::nullopt,
                                          merge(std::move(res.errors), std::move(opt.errors))},
                        ps...);
    } else {
      return seq_helper(std::move(res), ps...);
    }
  } else {
    using R2 = decltype(std::tuple_cat(*res.value, std::tuple(*p(res.tokens).value)));

    if(res) {
      auto opt = p(res.tokens);
      return seq_helper(
        ParseResult<R2, T>{opt.tokens,
                           opt ? std::optional(std::tuple_cat(std::move(*res.value), std::tuple(std::move(*opt.value))))
                               : std::nullopt,
                           merge(std::move(res.errors), std::move(opt.errors))},
        ps...);
    } else {
      return seq_helper(failing_result<R2>(res.tokens, std::move(res.errors)), ps...);
    }
  }
}

template <typename R, typename T>
auto choose_helper(Span<T> s, std::vector<std::pair<std::string, const T*>> errs) {
  return failing_result<R>(s, std::move(errs));
}

template <typename R, typename T, typename P, typename... Ps>
auto choose_helper(Span<T> s, std::vector<std::pair<std::string, const T*>> errs, P p, Ps... ps) {
  auto opt = p(s);
  return opt ? passing_result(opt.tokens, R{std::move(*opt.value)}, std::move(opt.errors))
             : choose_helper<R>(std::move(s), merge(std::move(errs), std::move(opt.errors)), ps...);
}

} // namespace details

template <typename... Ps>
auto seq(Ps... ps) {
  return [=](auto s) { return details::seq_helper(passing_result(s, std::tuple()), ps...); };
}

template <typename... Ps>
auto choose(Ps... ps) {
  return
    [=](auto s) { return details::choose_helper<std::variant<std::decay_t<decltype(*ps(s).value)>...>>(s, {}, ps...); };
}

template <typename P>
auto n(P p) {
  return [=](auto s) {
    using T = typename decltype(s)::value_type;

    std::vector<std::decay_t<decltype(*p(s).value)>> results;
    std::vector<std::pair<std::string, const T*>> errs;

    while(true) {
      auto opt = p(s);
      errs = merge(std::move(errs), std::move(opt.errors));
      if(!opt) break;
      results.push_back(std::move(*opt.value));
      s = opt.tokens;
    }

    return passing_result(s, std::move(results), std::move(errs));
  };
}

inline auto any() {
  return [](auto s) {
    using T = typename decltype(s)::value_type;

    return s.empty() ? failing_result<T>(s, {{"anything", s.data()}})
                     : passing_result(s.subspan(1), std::move(s.front()));
  };
}

template <typename P>
auto maybe(P p) {
  return [=](auto s) {
    auto opt = p(s);

    using T = typename decltype(s)::value_type;
    using R = std::optional<std::decay_t<decltype(*opt.value)>>;

    return opt ? passing_result(opt.tokens, std::optional(std::move(*opt.value)), std::move(opt.errors))
               : passing_result(s, std::optional(R{}), std::move(opt.errors));
  };
}

template <typename P, typename F>
auto transform(P p, F f) {
  return [=](auto s) {
    auto opt = p(s);

    using T = typename decltype(s)::value_type;

    if constexpr(knot::is_tuple_like(decay(knot::Type<decltype(*opt.value)>{}))) {
      using R = std::decay_t<decltype(std::apply(f, std::move(*opt.value)))>;
      return opt ? passing_result(opt.tokens, std::apply(f, std::move(*opt.value)), std::move(opt.errors))
                 : failing_result<R>(opt.tokens, std::move(opt.errors));
    } else {
      using R = std::decay_t<decltype(std::invoke(f, std::move(*opt.value)))>;
      return opt ? passing_result(opt.tokens, std::invoke(f, std::move(*opt.value)), std::move(opt.errors))
                 : failing_result<R>(opt.tokens, std::move(opt.errors));
    }
  };
}

template <typename F>
auto transform_if(std::string msg, F f) {
  return [=](auto s) {
    auto opt = s.empty() ? std::nullopt : f(std::move(s.front()));

    using T = typename decltype(s)::value_type;
    using R = std::decay_t<decltype(*opt)>;

    return opt ? ParseResult<R, T>{s.subspan(1), std::move(*opt)} : failing_result<R>(s, {{msg, s.data()}});
  };
}

template <typename T, typename P>
auto construct(P p) {
  return transform(p, [](auto&&... ts) { return T{std::forward<decltype(ts)>(ts)...}; });
}

template <typename T>
auto constant(std::string msg, T t) {
  return transform_if(std::move(msg), [t = std::move(t)](const auto& t2) {
    return t == t2 ? std::optional(std::tuple()) : std::nullopt;
  });
}

template <typename Token, typename Parser>
auto parse(const Parser& p, Span<Token> s) {
  auto res = p(s);

  using R = std::decay_t<decltype(*res.value)>;

  const Token* start = s.begin();
  const Token* passing_end = s.end() - res.tokens.size();

  std::sort(
    res.errors.begin(), res.errors.end(), [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; });

  res.errors.erase(std::unique(res.errors.begin(), res.errors.end()), res.errors.end());

  return (res && res.tokens.empty())
           ? tl::expected<R, std::vector<std::pair<std::string, const Token*>>>{std::move(*res.value)}
           : tl::unexpected{std::move(res.errors)};
}

} // namespace ooze::pc
