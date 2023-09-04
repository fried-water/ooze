#include "pch.h"

#include "graph_inner.h"

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
  const auto borrows = std::count_if(pbs.begin(), pbs.end(), [](PassBy pb) { return pb == PassBy::Borrow; });
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

} // namespace

std::vector<Iterm>& ConstructingGraph::fwd_of(Oterm o) {
  assert(o.term.node_id == 0 || !o.borrow);

  return o.borrow ? input_borrowed_fwds[o.term.port] : owned_fwds[o.term.node_id][o.term.port];
}

void ConstructingGraph::add_edges(Span<Oterm> inputs, Span<PassBy> pbs) {
  int value_idx = 0;
  int borrow_idx = 0;

  for(int i = 0; i < inputs.ssize(); i++) {
    fwd_of(inputs[i]).push_back({int(fns.size()), pbs[i] == PassBy::Borrow ? borrow_idx++ : value_idx++, pbs[i]});
  }
}

std::vector<Oterm> ConstructingGraph::add(AsyncFn fn, Span<Oterm> inputs, Span<PassBy> pbs, int num_outputs) {
  add_edges(inputs, pbs);

  input_counts.push_back(counts(pbs));
  fns.push_back(std::move(fn));
  owned_fwds.push_back(std::vector<std::vector<Iterm>>(num_outputs));

  return make_oterms(int(fns.size()), num_outputs);
}

std::vector<Oterm> ConstructingGraph::add(const FunctionGraph& g_outer, Span<Oterm> inputs) {
  const FunctionGraph::State& g = *g_outer.state;

  const int offset = int(owned_fwds.size()) - 1;

  std::vector<Oterm> outputs(g.output_count);

  const auto process_fwds = [&](Oterm fanin, const ValueForward& input_fwd, std::vector<Iterm>& output_terms) {
    for(int i = 0; i < int(input_fwd.terms.size()); i++) {
      const Term fanout = input_fwd.terms[i];
      const PassBy pb =
        i < input_fwd.copy_end ? PassBy::Copy : (i < input_fwd.move_end ? PassBy::Move : PassBy::Borrow);
      if(fanout.node_id == g.fns.size()) {
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

  for(int n = 0; n < g.fns.size(); n++) {
    input_counts.push_back(g.input_counts[n]);
    fns.push_back(g.fns[n]);
    owned_fwds.push_back(std::vector<std::vector<Iterm>>(g.owned_fwds[n + 1].size()));

    for(int p = 0; p < g.owned_fwds[n + 1].size(); p++) {
      process_fwds({{n + 1 + offset, p}, false}, g.owned_fwds[n + 1][p], owned_fwds.back()[p]);
    }
  }

  return outputs;
}

FunctionGraph ConstructingGraph::finalize(Span<Oterm> outputs, Span<PassBy> pbs) && {
  assert(owned_fwds.size() > 0);

  add_edges(outputs, pbs);

  return FunctionGraph{std::make_shared<const FunctionGraph::State>(FunctionGraph::State{
    std::move(input_borrows),
    int(outputs.size()),
    knot::map<std::vector<std::vector<ValueForward>>>(std::move(owned_fwds), to_fwd),
    knot::map<std::vector<ValueForward>>(std::move(input_borrowed_fwds), to_fwd),
    std::move(input_counts),
    std::move(fns)})};
}

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<bool> input_borrows) {
  std::vector<Oterm> terms = make_oterms(0, input_borrows);
  return {ConstructingGraph{std::move(input_borrows)}, std::move(terms)};
}

ConstructingGraph::ConstructingGraph(std::vector<bool> input_borrows_) : input_borrows{std::move(input_borrows_)} {
  auto [value_count, borrow_count] = counts(input_borrows);
  owned_fwds.push_back(std::vector<std::vector<Iterm>>(value_count));
  input_borrowed_fwds.resize(borrow_count);
}

} // namespace ooze
