#pragma once

#include "ooze/function_graph.h"
#include "ooze/inst.h"

namespace ooze {

enum class PassBy { Copy, Move, Borrow };

struct Oterm {
  Term term;
  bool borrow = false;
  KNOT_ORDERED(Oterm);
};

struct Iterm {
  Term term;
  PassBy pb = PassBy::Copy;

  KNOT_ORDERED(Iterm);
};

class ConstructingGraph {
  std::vector<bool> input_borrows;

  std::vector<std::vector<std::vector<Iterm>>> owned_fwds;
  std::vector<std::vector<Iterm>> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<Inst> insts;

  std::vector<Iterm>& fwd_of(Oterm);
  void add_edges(Span<Oterm>, Span<PassBy>);

public:
  ConstructingGraph() = default;

  explicit ConstructingGraph(std::vector<bool>);

  std::vector<Oterm> add(Inst, Span<Oterm>, Span<PassBy>, int output_count);
  std::vector<Oterm> add(const FunctionGraph&, Span<Oterm>);

  FunctionGraph finalize(Span<Oterm>, Span<PassBy>) &&;
};

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<bool> input_borrows);

} // namespace ooze
