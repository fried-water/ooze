#pragma once

#include "async_functions.h"
#include "ooze/any_function.h"
#include "ooze/function_graph.h"

#include <exception>

namespace ooze {

enum class PassBy { Copy, Move, Borrow };

struct Term {
  int node_id = 0;
  int port = 0;
  KNOT_ORDERED(Term);
};

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

struct ValueForward {
  std::vector<Term> terms;
  int copy_end = 0;
  int move_end = 0;

  KNOT_ORDERED(ValueForward);
};

struct FunctionGraph::State {
  std::vector<bool> input_borrows;
  int output_count = 0;

  std::vector<std::vector<ValueForward>> owned_fwds;
  std::vector<ValueForward> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<AsyncFn> fns;
};

class ConstructingGraph {
  std::vector<bool> input_borrows;

  std::vector<std::vector<std::vector<Iterm>>> owned_fwds;
  std::vector<std::vector<Iterm>> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<AsyncFn> fns;

  std::vector<Iterm>& fwd_of(Oterm);
  void add_edges(Span<Oterm>, Span<PassBy>);

public:
  ConstructingGraph() = default;

  explicit ConstructingGraph(std::vector<bool>);

  std::vector<Oterm> add(AsyncFn, Span<Oterm>, Span<PassBy>, int output_count);
  std::vector<Oterm> add(const FunctionGraph&, Span<Oterm>);

  FunctionGraph finalize(Span<Oterm>, Span<PassBy>) &&;
};

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<bool> input_borrows);

} // namespace ooze
