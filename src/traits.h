#pragma once

namespace ooze {

template <typename... Ts, typename... Us>
constexpr auto uniquify(knot::TypeList<Ts...> x, knot::TypeList<Us...> y) {
  if constexpr(size(y) == 0) {
    return x;
  } else if constexpr(((knot::Type<Ts>{} == head(y)) || ...)) {
    return uniquify(x, tail(y));
  } else {
    return uniquify(knot::TypeList<Ts..., knot::type_t<decltype(head(y))>>{}, tail(y));
  }
}

template <typename... Ts>
constexpr auto uniquify(knot::TypeList<Ts...> tl) {
  return uniquify(knot::TypeList<>{}, tl);
}

template <typename... Ts>
constexpr auto as_variant(knot::TypeList<Ts...> tl) {
  if constexpr(size(tl) == 1) {
    return head(tl);
  } else {
    return knot::Type<std::variant<Ts...>>{};
  }
}

static_assert(std::is_same_v<decltype(uniquify(knot::TypeList<int, char, int>{})), knot::TypeList<int, char>>);

} // namespace ooze
