#pragma once

#include "ooze/iter.h"
#include "ooze/primitives.h"
#include "ooze/span.h"
#include "ooze/strong_id.h"

namespace ooze {

template <typename ID>
class Graph {
  using underlying_type = decltype(as_integral(ID{}));

  std::vector<ID> _fanout;
  std::vector<i32> _indices = {0};

public:
  i32 num_nodes() const { return int(_indices.size() - 1); }
  i32 num_edges() const { return int(_fanout.size()); }

  auto nodes() const { return id_range(ID(underlying_type(num_nodes()))); }
  auto fanout(ID n) const {
    return Span(_fanout.data() + _indices[as_integral(n)], _fanout.data() + _indices[as_integral(n) + 1]);
  }

  ID add_node() {
    _indices.push_back(int(_fanout.size()));
    return ID{underlying_type(num_nodes() - 1)};
  }

  void add_fanout_to_last_node(ID fanout) {
    _fanout.push_back(fanout);
    _indices.back()++;
  }

  void reserve_nodes(size_t s) { _indices.reserve(s + 1); }
  void reserve_edges(size_t s) { _fanout.reserve(s); }

  void shrink_to_fit() {
    _indices.shrink_to_fit();
    _fanout.shrink_to_fit();
  }

  friend auto as_tie(const Graph& g) { return std::tie(g._fanout, g._indices); }
  KNOT_COMPAREABLE(Graph);
};

} // namespace ooze
