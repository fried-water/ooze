#pragma once

#include <cassert>
#include <variant>
#include <vector>

namespace ooze {

template <typename... Ts>
struct Tree {
  std::variant<std::vector<Tree<Ts...>>, Ts...> v;

  friend auto as_tie(const Tree& t) { return std::tie(t.v); }
  friend auto as_tie(Tree& t) { return std::tie(t.v); }
  friend auto as_tie(Tree&& t) { return std::forward_as_tuple(std::move(t).v); }
};

template <typename T1, typename T2, typename F>
void co_visit(T1& t1, T2& t2, F f) {
  if(t1.v.index() == 0 && t2.v.index() == 0 && std::get<0>(t1.v).size() == std::get<0>(t2.v).size()) {
    auto& v1 = std::get<0>(t1.v);
    auto& v2 = std::get<0>(t2.v);

    for(size_t i = 0; i < v1.size(); i++) {
      co_visit(v1[i], v2[i], f);
    }
  } else {
    std::visit(
      [&](auto& a, auto& b) {
        if constexpr(knot::is_invocable(knot::Type<F>{}, knot::TypeList<decltype(a), decltype(b)>{})) {
          f(a, b);
        } else if constexpr(knot::is_invocable(knot::Type<F>{}, knot::TypeList<T1&, T2&, decltype(a), decltype(b)>{})) {
          f(t1, t2, a, b);
        }
      },
      t1.v,
      t2.v);
  }
}

template <typename Tree, typename F>
void tree_to_string(std::ostream& os, const Tree& t, F f) {
  if(t.v.index() == 0) {
    const auto& v = std::get<0>(t.v);
    os << '(';
    if(!v.empty()) {
      std::for_each(v.begin(), v.end() - 1, [&](const Tree& ele) {
        tree_to_string(os, ele, f);
        os << ", ";
      });
      tree_to_string(os, v.back(), f);
    }
    os << ')';
  } else {
    std::visit(
      [&](const auto& a) {
        if constexpr(knot::is_invocable(knot::Type<F>{}, knot::TypeList<std::ostream&, decltype(a)>{})) {
          return f(os, a);
        } else {
          assert(false);
        }
      },
      t.v);
  }
}

} // namespace ooze
