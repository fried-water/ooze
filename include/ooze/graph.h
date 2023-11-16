#pragma once

#include "ooze/iter.h"
#include "ooze/primitives.h"
#include "ooze/span.h"
#include "ooze/strong_id.h"

namespace ooze {

template <typename ID, typename... Ts>
class Graph : std::vector<Ts>... {
  using underlying_type = decltype(as_integral(ID{}));

  std::vector<i32> _indices = {0};
  std::vector<ID> _fanout;

public:
  Graph() = default;

  Graph(std::vector<i32> indices, std::vector<ID> fanout, std::vector<Ts>... columns)
      : std::vector<Ts>{std::move(columns)}..., _indices(std::move(indices)), _fanout(std::move(fanout)) {}

  i32 num_nodes() const { return int(_indices.size() - 1); }
  i32 num_edges() const { return int(_fanout.size()); }

  auto nodes() const { return id_range(ID(underlying_type(num_nodes()))); }
  auto fanout(ID n) const {
    return Span(_fanout.data() + _indices[as_integral(n)], _fanout.data() + _indices[as_integral(n) + 1]);
  }

  template <typename T>
  T get(ID id) const {
    return static_cast<const std::vector<T>&>(*this)[as_integral(id)];
  }

  ID add_node(Ts... ts) {
    _indices.push_back(int(_fanout.size()));
    (static_cast<std::vector<Ts>&>(*this).push_back(std::move(ts)), ...);
    return ID{underlying_type(num_nodes() - 1)};
  }

  void add_fanout_to_last_node(ID fanout) {
    _indices.back()++;
    _fanout.push_back(fanout);
  }

  void pop_node() {
    _indices.pop_back();
    _fanout.erase(_fanout.begin() + _indices.back(), _fanout.end());
    (static_cast<std::vector<Ts>&>(*this).pop_back(), ...);
  }

  void reserve_nodes(size_t s) {
    _indices.reserve(s + 1);
    (static_cast<std::vector<Ts>&>(*this).reserve(s), ...);
  }

  void reserve_edges(size_t s) { _fanout.reserve(s); }

  void shrink_to_fit() {
    _indices.shrink_to_fit();
    _fanout.shrink_to_fit();
    (static_cast<std::vector<Ts>&>(*this).shrink_to_fit(), ...);
  }

  template <typename T>
  Graph<ID, Ts..., T> append_column(std::vector<T> c) && {
    return {std::move(_indices), std::move(_fanout), static_cast<std::vector<Ts>&&>(*this)..., std::move(c)};
  }

  std::tuple<Graph<ID>, std::vector<Ts>...> decompose() && {
    return {{std::move(_indices), std::move(_fanout)}, static_cast<std::vector<Ts>&&>(*this)...};
  }

  friend auto as_tie(const Graph& g) {
    return std::tie(g._indices, g._fanout, static_cast<const std::vector<Ts>&>(g)...);
  }
  KNOT_COMPAREABLE(Graph);
};

} // namespace ooze
