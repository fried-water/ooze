#pragma once

#include "ooze/inst.h"

namespace ooze {

struct Term {
  int node_id = 0;
  int port = 0;
  KNOT_ORDERED(Term);
};

struct ValueForward {
  std::vector<Term> terms;
  int copy_end = 0;
  int move_end = 0;

  KNOT_ORDERED(ValueForward);
};

struct FunctionGraph {
  std::vector<bool> input_borrows;
  int output_count = 0;

  std::vector<std::vector<ValueForward>> owned_fwds;
  std::vector<ValueForward> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<Inst> fns;

  KNOT_COMPAREABLE(FunctionGraph);
};

} // namespace ooze
