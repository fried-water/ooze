#pragma once

#include <knot/map.h>
#include <knot/traversals.h>

#include <optional>
#include <variant>
#include <vector>

namespace ooze {

// Tree with values only on the leaves
template <typename T>
struct Tree : std::variant<std::vector<Tree<T>>, T> {
  using Base = std::variant<std::vector<Tree<T>>, T>;

  Slice ref;

  bool is_leaf() const { return this->index() == 1; }
  const T* value() const { return std::get_if<T>(this); }
  const auto& children() const { return std::get<0>(*this); }

  friend auto as_tie(const Tree& t) { return std::tie(static_cast<const Base&>(t), t.ref); }

  KNOT_ORDERED(Tree);
};

template <typename T, typename F>
auto map(const Tree<T>& tree, F f) {
  return knot::map<Tree<decltype(f(std::declval<T>()))>>(tree, std::move(f));
}

template <typename T>
std::optional<T> value(Tree<T> tree) {
  return tree.index() == 1 ? std::optional(std::move(std::get<T>(tree))) : std::nullopt;
}

template <typename T>
std::vector<T> leaves(const Tree<T>& tree) {
  return knot::preorder_accumulate(
    tree,
    [](std::vector<T> acc, const T& t) {
      acc.push_back(t);
      return acc;
    },
    std::vector<T>{});
}

} // namespace ooze
