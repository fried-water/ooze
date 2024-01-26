#pragma once

#include "ooze/iter.h"
#include "ooze/primitives.h"
#include "ooze/result.h"
#include "ooze/span.h"
#include "ooze/strong_id.h"

#include <vector>

namespace ooze {

template <typename ID, typename... Ts>
class Graph : std::vector<Ts>... {
  using underlying_type = decltype(as_integral(ID{}));

  std::vector<i32> _indices;
  std::vector<ID> _fanout;

public:
  Graph() : _indices{0} {}

  Graph(const std::vector<std::vector<ID>>& fanouts, std::vector<Ts>... columns)
      : std::vector<Ts>{std::move(columns)}... {
    _indices.reserve(fanouts.size() + 1);
    size_t offset = 0;

    for(const auto& fanout : fanouts) {
      _indices.push_back(i32(offset));
      offset += fanout.size();
    }
    _indices.push_back(i32(offset));

    _fanout.reserve(_indices.back());
    for(const auto& fanout : fanouts) {
      _fanout.insert(_fanout.end(), fanout.begin(), fanout.end());
    }
  }

  Graph(std::vector<i32> indices, std::vector<ID> fanout, std::vector<Ts>... columns)
      : std::vector<Ts>{std::move(columns)}..., _indices(std::move(indices)), _fanout(std::move(fanout)) {}

  i32 num_nodes() const { return int(_indices.size() - 1); }
  i32 num_edges() const { return int(_fanout.size()); }
  i32 num_fanout(ID n) const { return _indices[as_integral(n) + 1] - _indices[as_integral(n)]; }
  bool has_fanout(ID n) const { return num_fanout(n) > 0; }

  auto nodes() const { return id_range(ID(underlying_type(num_nodes()))); }
  auto fanout(ID n) const {
    return Span(_fanout.data() + _indices[as_integral(n)], _fanout.data() + _indices[as_integral(n) + 1]);
  }

  template <typename T>
  T get(ID id) const {
    return static_cast<const std::vector<T>&>(*this)[as_integral(id)];
  }

  template <typename T>
  void set(ID id, T t) {
    static_cast<std::vector<T>&>(*this)[as_integral(id)] = std::move(t);
  }

  void add_graph(const Graph& g) {
    const auto node_offset = _indices.size() - 1;
    const auto edge_offset = _fanout.size();

    for(int idx : IterRange(g._indices.begin() + 1, g._indices.end())) {
      _indices.push_back(idx + edge_offset);
    }

    for(ID node : g._fanout) {
      _fanout.push_back(ID(underlying_type(as_integral(node) + node_offset)));
    }

    (static_cast<std::vector<Ts>&>(*this).insert(static_cast<std::vector<Ts>&>(*this).end(),
                                                 static_cast<const std::vector<Ts>&>(g).begin(),
                                                 static_cast<const std::vector<Ts>&>(g).end()),
     ...);
  }

  ID add_node(Span<ID> fanout, Ts... ts) {
    _indices.push_back(int(_fanout.size() + fanout.size()));
    _fanout.insert(_fanout.end(), fanout.begin(), fanout.end());
    (static_cast<std::vector<Ts>&>(*this).push_back(std::move(ts)), ...);
    return ID{underlying_type(num_nodes() - 1)};
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

  template <typename... Rs>
  Graph<ID, Ts..., Rs...> append_column(std::vector<Rs>... rs) && {
    return {std::move(_indices), std::move(_fanout), static_cast<std::vector<Ts>&&>(*this)..., std::move(rs)...};
  }

  std::tuple<Graph<ID>, std::vector<Ts>...> decompose() && {
    return {{std::move(_indices), std::move(_fanout)}, static_cast<std::vector<Ts>&&>(*this)...};
  }

  friend bool compare_dags(const Graph& g, ID x, ID y) {
    const auto x_fanout = g.fanout(x);
    const auto y_fanout = g.fanout(y);

    return x == y || (((g.get<Ts>(x) == g.get<Ts>(y)) && ...) &&
                      std::equal(x_fanout.begin(), x_fanout.end(), y_fanout.begin(), y_fanout.end(), [&](ID xf, ID yf) {
                        return compare_dags(g, xf, yf);
                      }));
  }

  friend bool compare_dags(const Graph& g1, const Graph& g2, ID x, ID y) {
    const auto x_fanout = g1.fanout(x);
    const auto y_fanout = g2.fanout(y);

    return ((g1.get<Ts>(x) == g2.get<Ts>(y)) && ...) &&
           std::equal(x_fanout.begin(), x_fanout.end(), y_fanout.begin(), y_fanout.end(), [&](ID xf, ID yf) {
             return compare_dags(g1, g2, xf, yf);
           });
  }

  friend auto as_tie(const Graph& g) {
    return std::tie(g._indices, g._fanout, static_cast<const std::vector<Ts>&>(g)...);
  }
  KNOT_COMPAREABLE(Graph);
};

template <typename ID, typename... Ts, typename F>
void preorder(const Graph<ID, Ts...>& g, ID start, F f) {
  std::vector<ID> to_visit{start};

  while(!to_visit.empty()) {
    const ID id = to_visit.back();
    to_visit.pop_back();

    if(f(id)) {
      const auto fanout = g.fanout(id);
      to_visit.insert(to_visit.end(), fanout.rbegin(), fanout.rend());
    }
  }
}

template <typename ID, typename... Ts>
Graph<ID, Ts...> invert(const Graph<ID, Ts...>& g) {
  std::vector<std::vector<ID>> inverted(g.num_nodes());

  for(ID node : g.nodes()) {
    for(ID fanout : g.fanout(node)) {
      inverted[fanout.get()].push_back(node);
    }
  }

  return Graph<ID, Ts...>(inverted, static_cast<const std::vector<Ts>&>(g)...);
}

// Return a topographical ordering if the graph is acyclic or all nodes in any cycles
template <typename ID, typename... Ts>
Result<std::vector<ID>, std::vector<ID>> topographical_ordering(const Graph<ID, Ts...>& g) {
  std::vector<ID> traversal;
  std::vector<int> fanins(g.num_nodes(), 0);

  for(ID id : g.nodes()) {
    for(ID fanout : g.fanout(id)) {
      fanins[as_integral(fanout)]++;
    }
  }

  std::vector<ID> to_visit = filter_to_vec(g.nodes(), [&](ID id) { return fanins[as_integral(id)] == 0; });

  while(!to_visit.empty()) {
    const ID id = to_visit.back();
    to_visit.pop_back();
    traversal.push_back(id);

    const auto fanout = g.fanout(id);
    for(ID fanout : g.fanout(id)) {
      fanins[as_integral(fanout)]--;
      if(fanins[as_integral(fanout)] == 0) {
        to_visit.push_back(fanout);
      }
    }
  }

  return traversal.size() == g.num_nodes()
           ? Result<std::vector<ID>, std::vector<ID>>{std::move(traversal)}
           : Result<std::vector<ID>, std::vector<ID>>{
               Failure{filter_to_vec(g.nodes(), [&](ID id) { return fanins[as_integral(id)] > 0; })}};
}

} // namespace ooze
