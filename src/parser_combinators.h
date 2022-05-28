#pragma once

namespace ooze::pc {

template <typename S, typename P>
using parser_result_t = decltype(std::declval<P>()(std::declval<S>())->second);

namespace details {

template <typename Result, typename State>
auto seq_helper(State s, Result r) {
  if constexpr(std::tuple_size_v<Result> == 1) {
    return std::optional(std::pair(s, std::move(std::get<0>(r))));
  } else {
    return std::optional(std::pair(s, std::move(r)));
  }
}

template <typename Result, typename State, typename P, typename... Ps>
auto seq_helper(State s, Result r, P p, Ps... ps) {
  auto opt = p(s);

  if constexpr(std::is_same_v<std::tuple<>, parser_result_t<State, P>>) {
    return opt ? seq_helper(opt->first, std::move(r), ps...) : std::nullopt;
  } else {
    return opt ? seq_helper(opt->first, std::tuple_cat(std::move(r), std::tuple(std::move(opt->second))), ps...)
               : std::nullopt;
  }
}

template <typename Result, typename Token>
auto choose_helper(Span<Token>) {
  return std::optional<std::pair<Span<Token>, Result>>{};
}

template <typename Result, typename Token, typename P, typename... Ps>
auto choose_helper(Span<Token> s, P p, Ps... ps) {
  auto opt = p(s);
  return opt ? std::optional(std::pair(opt->first, Result(std::move(opt->second)))) : choose_helper<Result>(s, ps...);
}

template <typename>
struct is_tuple : std::false_type {};
template <typename... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};

} // namespace details

template <typename... Ps>
auto seq(Ps... ps) {
  return [=](auto s) { return details::seq_helper(s, std::tuple(), ps...); };
}

template <typename... Ps>
auto choose(Ps... ps) {
  return [=](auto s) { return details::choose_helper<std::variant<parser_result_t<decltype(s), Ps>...>>(s, ps...); };
}

template <typename P>
auto n(P p) {
  return [=](auto s) {
    std::vector<parser_result_t<decltype(s), P>> results;

    while(true) {
      auto opt = p(s);
      if(!opt) break;
      results.push_back(std::move(opt->second));
      s = opt->first;
    }

    return std::optional(std::pair(s, std::move(results)));
  };
}

inline auto any() {
  return [](auto s) { return s.empty() ? std::nullopt : std::optional(std::pair(s.subspan(1), std::move(s.front()))); };
}

template <typename P>
auto maybe(P p) {
  return [=](auto s) {
    auto opt = p(s);
    return std::optional(opt ? std::pair(opt->first, std::optional(opt->second)) : std::pair(s, std::nullopt));
  };
}

template <typename P, typename F>
auto transform(P p, F f) {
  return [=](auto s) {
    auto opt = p(s);

    if constexpr(details::is_tuple<parser_result_t<decltype(s), P>>::value) {
      return opt ? std::optional(std::pair(opt->first, std::apply(f, std::move(opt->second)))) : std::nullopt;
    } else {
      return opt ? std::optional(std::pair(opt->first, std::invoke(f, std::move(opt->second)))) : std::nullopt;
    }
  };
}

template <typename F>
auto transform_if(F f) {
  return [=](auto s) {
    auto opt = s.empty() ? std::nullopt : f(s.front());
    return opt ? std::optional(std::pair(s.subspan(1), std::move(*opt))) : std::nullopt;
  };
}

template <typename T, typename P>
auto construct(P p) {
  return transform(p, [](auto&&... ts) { return T{std::forward<decltype(ts)>(ts)...}; });
}

template <typename T>
auto constant(T t) {
  return transform_if(
    [t = std::move(t)](const auto& t2) { return t == t2 ? std::optional(std::tuple()) : std::nullopt; });
}

template <typename Token, typename Parser>
auto parse(const Parser& p, Span<Token> s) {
  auto res = p(s);
  return (res && res->first.empty()) ? std::optional(std::move(res->second)) : std::nullopt;
}

} // namespace ooze::pc
