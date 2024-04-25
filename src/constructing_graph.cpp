#include "pch.h"

#include "constructing_graph.h"

namespace ooze {

namespace {

std::vector<Oterm> make_oterms(int node, const std::vector<bool>& borrows) {
  int value_idx = 0;
  int borrow_idx = 0;
  return transform_to_vec(borrows, [&](bool borrowed) {
    return Oterm{node, borrowed ? borrow_idx++ : value_idx++, borrowed};
  });
}

std::vector<Oterm> make_oterms(int node, int size) {
  std::vector<Oterm> terms;
  terms.reserve(size);
  for(int i = 0; i < size; i++) {
    terms.push_back({node, i, false});
  }
  return terms;
}

std::pair<int, int> counts(const std::vector<bool>& borrows) {
  const auto borrow_count = int(std::count(borrows.begin(), borrows.end(), true));
  return {int(borrows.size()) - borrow_count, borrow_count};
}

std::pair<int, int> counts(Span<PassBy> pbs) {
  const auto borrows = stdr::count_if(pbs, [](PassBy pb) { return pb == PassBy::Borrow; });
  return {int(pbs.size() - borrows), int(borrows)};
}

ValueForward to_fwd(std::vector<Iterm> terms) {
  const auto copy_end =
    std::stable_partition(terms.begin(), terms.end(), [&](Iterm i) { return i.pb == PassBy::Copy; });
  const auto move_end = std::stable_partition(copy_end, terms.end(), [&](Iterm i) { return i.pb == PassBy::Move; });

  return {transform_to_vec(terms, [](Iterm i) { return i.term; }),
          int(std::distance(terms.begin(), copy_end)),
          int(std::distance(terms.begin(), move_end))};
}

auto find_borrow_cleanups(std::vector<std::vector<ValueForward>> fwds) {
  std::vector<BorrowCleanup> cleanups;
  auto node_borrows = std::vector<std::vector<int>>(fwds.size() - 1);

  knot::preorder(fwds, [&](ValueForward& fwd) {
    if(fwd.move_end != std::ssize(fwd.terms)) {
      fwd.cleanup_idx = int(cleanups.size());
      cleanups.push_back({int(fwd.terms.size()) - fwd.move_end,
                          fwd.copy_end != fwd.move_end ? std::optional(fwd.terms[fwd.copy_end]) : std::nullopt});
      for(int i = fwd.move_end; i < std::ssize(fwd.terms); i++) {
        node_borrows[fwd.terms[i].node_id].push_back(fwd.cleanup_idx);
      }
    }
  });

  return std::tuple(std::move(cleanups), std::move(node_borrows), std::move(fwds));
}

} // namespace

std::vector<Iterm>& ConstructingGraph::fwd_of(Oterm o) {
  assert(o.term.node_id == 0 || !o.borrow);

  return o.borrow ? input_borrowed_fwds[o.term.port] : owned_fwds[o.term.node_id][o.term.port];
}

void ConstructingGraph::add_edges(Span<Oterm> inputs, Span<PassBy> pbs) {
  int value_idx = 0;
  int borrow_idx = 0;

  for(int i = 0; i < std::ssize(inputs); i++) {
    fwd_of(inputs[i]).push_back({int(insts.size()), pbs[i] == PassBy::Borrow ? borrow_idx++ : value_idx++, pbs[i]});
  }
}

std::vector<Oterm> ConstructingGraph::add(Inst fn, Span<Oterm> inputs, Span<PassBy> pbs, int output_count) {
  add_edges(inputs, pbs);

  input_counts.push_back(counts(pbs));
  insts.push_back(fn);
  owned_fwds.emplace_back(output_count);

  return make_oterms(int(insts.size()), output_count);
}

std::vector<Oterm> ConstructingGraph::add(const FunctionGraph& g, Span<Oterm> inputs) {
  const int offset = int(owned_fwds.size()) - 1;

  std::vector<Oterm> outputs(g.output_count);

  const auto process_fwds = [&](Oterm fanin, const ValueForward& input_fwd, std::vector<Iterm>& output_terms) {
    for(int i = 0; i < int(input_fwd.terms.size()); i++) {
      const Term fanout = input_fwd.terms[i];
      const PassBy pb =
        i < input_fwd.copy_end ? PassBy::Copy : (i < input_fwd.move_end ? PassBy::Move : PassBy::Borrow);
      if(fanout.node_id == g.insts.size()) {
        outputs[fanout.port] = fanin;
      } else {
        output_terms.push_back({{fanout.node_id + offset, fanout.port}, pb});
      }
    }
  };

  int value_idx = 0;
  int borrow_idx = 0;

  for(int i = 0; i < inputs.size(); i++) {
    const ValueForward& inner_fwd =
      g.input_borrows[i] ? g.input_borrowed_fwds[borrow_idx++] : g.owned_fwds[0][value_idx++];
    process_fwds(inputs[i], inner_fwd, fwd_of(inputs[i]));
  }

  for(int n = 0; n < g.insts.size(); n++) {
    input_counts.push_back(g.input_counts[n]);
    insts.push_back(g.insts[n]);
    owned_fwds.emplace_back(g.owned_fwds[n + 1].size());

    for(int p = 0; p < g.owned_fwds[n + 1].size(); p++) {
      process_fwds({{n + 1 + offset, p}, false}, g.owned_fwds[n + 1][p], owned_fwds.back()[p]);
    }
  }

  return outputs;
}

std::optional<int> find_tailcall(const std::vector<std::vector<ValueForward>>& fwds, int output_count) {
  std::vector<bool> takes_ref(fwds.size(), false);

  const int output_node = int(fwds.size() - 1);

  const auto is_tailcall = [&](const std::vector<ValueForward>& fwds) {
    for(int i = 0; i < fwds.size(); i++) {
      if(!stdr::equal(fwds[i].terms, std::array{Term{output_node, i}})) {
        return false;
      }
    }
    return true;
  };

  for(int i = 0; i < fwds.size(); i++) {
    for(const ValueForward& fwd : fwds[i]) {
      for(int u = fwd.move_end; u < std::ssize(fwd.terms); u++) {
        takes_ref[fwd.terms[u].node_id] = true;
      }
    }

    if(output_count == std::ssize(fwds[i]) && is_tailcall(fwds[i])) {
      return i > 0 && !takes_ref[i - 1] ? std::optional(i - 1) : std::nullopt;
    }
  }

  return std::nullopt;
}

FunctionGraph ConstructingGraph::finalize(Span<Oterm> outputs, Span<PassBy> pbs) && {
  assert(!owned_fwds.empty());

  add_edges(outputs, pbs);

  auto [borrow_cleanups, node_borrows, final_owned_fwds] =
    find_borrow_cleanups(knot::map<std::vector<std::vector<ValueForward>>>(std::move(owned_fwds), to_fwd));

  const auto tailcall = find_tailcall(final_owned_fwds, int(outputs.size()));

  return FunctionGraph{
    std::move(input_borrows),
    int(outputs.size()),
    std::move(final_owned_fwds),
    knot::map<std::vector<ValueForward>>(std::move(input_borrowed_fwds), to_fwd),
    std::move(input_counts),
    std::move(insts),
    std::move(borrow_cleanups),
    std::move(node_borrows),
    tailcall};
}

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<bool> input_borrows) {
  std::vector<Oterm> terms = make_oterms(0, input_borrows);
  return {ConstructingGraph{std::move(input_borrows)}, std::move(terms)};
}

ConstructingGraph::ConstructingGraph(std::vector<bool> input_borrows_) : input_borrows{std::move(input_borrows_)} {
  auto [value_count, borrow_count] = counts(input_borrows);
  owned_fwds.emplace_back(value_count);
  input_borrowed_fwds.resize(borrow_count);
}

} // namespace ooze
