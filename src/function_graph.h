#pragma once

#include "inst.h"

#include <optional>
#include <utility>
#include <vector>

namespace ooze {

struct Term {
  int node_id = 0;
  int port = 0;

  friend auto operator<=>(const Term&, const Term&) = default;
};

struct ValueForward {
  std::vector<Term> terms;
  int copy_end = 0;
  int move_end = 0;
  int cleanup_idx = -1;

  friend auto operator<=>(const ValueForward&, const ValueForward&) = default;
};

struct BorrowCleanup {
  int count = 0;
  std::optional<Term> fwd;

  friend auto operator<=>(const BorrowCleanup&, const BorrowCleanup&) = default;
};

struct FunctionGraph {
  std::vector<bool> input_borrows;
  int output_count = 0;

  std::vector<std::vector<ValueForward>> owned_fwds;
  std::vector<ValueForward> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<Inst> insts;

  std::vector<BorrowCleanup> borrow_cleanups;
  std::vector<std::vector<int>> node_borrows;

  friend bool operator==(const FunctionGraph&, const FunctionGraph&) = default;
};

} // namespace ooze
