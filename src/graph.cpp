#include "pch.h"

#include "graph_inner.h"
#include "ooze/util.h"

#include <unordered_map>

namespace ooze {

namespace {

std::vector<Oterm> make_oterms(int node, Span<TypeProperties> types) {
  std::vector<Oterm> terms;
  terms.reserve(types.size());

  int value_idx = 0;
  int borrow_idx = 0;

  for(const TypeProperties& t : types) {
    terms.push_back({node, t.value ? value_idx++ : borrow_idx++, t.value});
  }

  return terms;
}

std::vector<Oterm> make_oterms(int node, int size) {
  std::vector<Oterm> terms;
  terms.reserve(size);
  for(int i = 0; i < size; i++) {
    terms.push_back({node, i, true});
  }
  return terms;
}

std::pair<int, int> counts(Span<TypeProperties> types) {
  const auto values = std::count_if(types.begin(), types.end(), [](auto t) { return t.value; });
  return {int(values), int(types.size() - values)};
}

TypeProperties type_of(Span<TypeProperties> types, int port, bool is_value) {
  auto it = std::find_if(types.begin(), types.end(), [&](auto t) { return t.value == is_value; });
  while(port-- > 0 && it != types.end()) {
    it = std::find_if(it + 1, types.end(), [&](auto t) { return t.value == is_value; });
  }
  assert(it != types.end());
  return *it;
}

TypeProperties type_of(const FunctionGraph::State& g, Oterm t) {
  if(t.node_id == 0) {
    return type_of(g.input_types, t.port, t.value);
  } else if(t.node_id <= g.exprs.size()) {
    assert(t.value);
    return std::visit(
      Overloaded{[&](const std::shared_ptr<const AnyFunction>& f) {
                   return TypeProperties{f->output_types()[t.port], true};
                 },
                 [&](const SExpr& i) {
                   return TypeProperties{i.types[t.port], true};
                 },
                 [&](const IfExpr& i) {
                   return TypeProperties{i.if_branch.state->output_types[t.port], true};
                 },
                 [&](const SelectExpr& s) { return s.types[t.port]; },
                 [&](const ConvergeExpr& c) {
                   return TypeProperties{c.body.state->output_types[t.port + 1], true};
                 }},
      g.exprs[t.node_id - 1]);
  } else {
    assert(t.value);
    return {g.output_types[t.port], true};
  }
}

ValueForward& fwd_of(FunctionGraph::State& g, Oterm t) {
  if(t.value) {
    return g.owned_fwds[t.node_id][t.port];
  } else {
    assert(t.node_id == 0);
    return g.input_borrowed_fwds[t.port];
  }
}

void check_types(const FunctionGraph::State& g, Span<TypeProperties> expected_types, Span<Oterm> inputs) {
  if(inputs.size() != expected_types.size()) {
    throw GraphError{};
  }

  for(int i = 0; i < inputs.ssize(); i++) {
    const Oterm oterm = inputs[i];
    const TypeProperties input_type = expected_types[i];
    const TypeProperties given_type = type_of(g, oterm);

    if(given_type.id != input_type.id) {
      throw GraphError{};
    } else if(!input_type.value || is_copyable(input_type.id)) {
      // always fine
    } else if(!given_type.value) {
      throw GraphError{};
    }
  }
}

void add_edges(FunctionGraph::State& g, Span<Oterm> inputs, Span<TypeProperties> input_types) {
  assert(inputs.size() == input_types.size());

  int value_idx = 0;
  int borrow_idx = 0;

  for(int i = 0; i < inputs.ssize(); i++) {
    const Iterm iterm = {int(g.exprs.size()), input_types[i].value ? value_idx++ : borrow_idx++, input_types[i].value};

    fwd_of(g, inputs[i]).terms.push_back(iterm);
  }
}

ValueForward fixup_fwd(TypeProperties type, ValueForward fwd) {
  const auto ref_begin = std::stable_partition(fwd.terms.begin(), fwd.terms.end(), [&](Iterm t) { return t.value; });

  fwd.move_end = static_cast<int>(std::distance(fwd.terms.begin(), ref_begin));
  fwd.copy_end = type.value ? std::max(0, fwd.move_end - 1) : fwd.move_end;

  if(ref_begin != fwd.terms.end() && is_copyable(type.id) && fwd.copy_end != fwd.move_end) {
    fwd.copy_end++;
  }

  return fwd;
}

std::vector<Oterm>
add_internal(FunctionGraph::State& g, Expr expr, Span<Oterm> inputs, Span<TypeProperties> input_types) {
  check_types(g, input_types, inputs);

  add_edges(g, inputs, input_types);

  const int output_count = num_outputs(expr);
  g.input_counts.push_back(counts(input_types));
  g.exprs.push_back(std::move(expr));
  g.owned_fwds.push_back(std::vector<ValueForward>(output_count));

  return make_oterms(int(g.exprs.size()), output_count);
}

} // namespace

TypeProperties ConstructingGraph::type(Oterm oterm) { return type_of(_state, oterm); }

std::vector<Oterm> ConstructingGraph::add(AnyFunction f, Span<Oterm> inputs) {
  const auto shared_f = std::make_shared<const AnyFunction>(std::move(f));
  return add_internal(_state, shared_f, inputs, shared_f->input_types());
}

std::vector<Oterm> ConstructingGraph::add(const FunctionGraph& g_outer, Span<Oterm> inputs) {
  const FunctionGraph::State& g = *g_outer.state;

  check_types(_state, g.input_types, inputs);

  const int offset = int(_state.owned_fwds.size()) - 1;

  std::vector<Oterm> outputs(g.output_types.size());

  const auto process_fwds = [&](Oterm fanin, const auto& input_terms, auto& output_terms) {
    for(Iterm fanout : input_terms) {
      if(fanout.node_id == g.exprs.size()) {
        outputs[fanout.port] = fanin;
      } else {
        output_terms.push_back({fanout.node_id + offset, fanout.port, fanout.value});
      }
    }
  };

  int value_idx = 0;
  int borrow_idx = 0;

  for(int i = 0; i < inputs.size(); i++) {
    const ValueForward& inner_fwd =
      g.input_types[i].value ? g.owned_fwds[0][value_idx++] : g.input_borrowed_fwds[borrow_idx++];
    process_fwds(inputs[i], inner_fwd.terms, fwd_of(_state, inputs[i]).terms);
  }

  for(int n = 0; n < g.exprs.size(); n++) {
    _state.input_counts.push_back(g.input_counts[n]);
    _state.exprs.push_back(g.exprs[n]);
    _state.owned_fwds.push_back(std::vector<ValueForward>(g.owned_fwds[n + 1].size()));

    for(int p = 0; p < g.owned_fwds[n + 1].size(); p++) {
      process_fwds({n + 1 + offset, p, true}, g.owned_fwds[n + 1][p].terms, _state.owned_fwds.back()[p].terms);
    }
  }

  return outputs;
}

std::vector<Oterm> ConstructingGraph::add_functional(
  Span<TypeProperties> fn_input_types, std::vector<TypeID> outputs, Oterm fn, Span<Oterm> fn_inputs) {

  std::vector<Oterm> inputs;
  inputs.reserve(fn_inputs.size() + 1);
  inputs.push_back(fn);
  inputs.insert(inputs.end(), fn_inputs.begin(), fn_inputs.end());

  std::vector<TypeProperties> input_types;
  input_types.reserve(fn_input_types.size() + 1);
  input_types.push_back({type_id<FunctionGraph>(), true});
  std::copy(fn_input_types.begin(), fn_input_types.end(), std::back_inserter(input_types));

  return add_internal(_state, SExpr{std::move(outputs)}, inputs, input_types);
}

std::vector<Oterm> ConstructingGraph::add_if(FunctionGraph if_branch, FunctionGraph else_branch, Span<Oterm> inputs) {
  if(if_branch.state->input_types != else_branch.state->input_types ||
     if_branch.state->output_types != else_branch.state->output_types) {
    throw GraphError{};
  }

  std::vector<TypeProperties> input_types;
  input_types.reserve(if_branch.state->input_types.size() + 1);
  input_types.push_back(TypeProperties{type_id<bool>(), true});
  input_types.insert(input_types.end(), if_branch.state->input_types.begin(), if_branch.state->input_types.end());

  return add_internal(_state, IfExpr{std::move(if_branch), std::move(else_branch)}, inputs, input_types);
}

std::vector<Oterm> ConstructingGraph::add_select(Oterm cond, Span<Oterm> if_branch, Span<Oterm> else_branch) {
  if(if_branch.size() != else_branch.size()) {
    throw GraphError{};
  }

  std::vector<TypeProperties> types = transform_to_vec(if_branch, [&](Oterm o) { return type(o); });

  for(size_t i = 0; i < if_branch.size(); i++) {
    if(types[i] != type(else_branch[i])) {
      throw GraphError{};
    }
  }

  std::vector<TypeProperties> input_types;
  input_types.reserve(if_branch.size() * 2 + 1);

  input_types.push_back({type_id<bool>(), true});
  std::transform(types.begin(),
                 types.end(),
                 std::back_inserter(input_types),
                 // TODO: allow select to return borrows
                 [&](TypeProperties t) {
                   return TypeProperties{t.id, true};
                 });
  input_types.insert(input_types.end(), input_types.begin() + 1, input_types.end());

  std::vector<Oterm> inputs;
  inputs.reserve(if_branch.size() * 2 + 1);
  inputs.push_back(cond);
  inputs.insert(inputs.end(), if_branch.begin(), if_branch.end());
  inputs.insert(inputs.end(), else_branch.begin(), else_branch.end());

  return add_internal(_state, SelectExpr{std::move(types)}, inputs, input_types);
}

std::vector<Oterm> ConstructingGraph::add_converge(FunctionGraph g, Span<Oterm> inputs) {
  std::vector<TypeProperties> input_types;
  input_types.reserve(g.state->input_types.size() + 1);
  input_types.push_back(TypeProperties{type_id<bool>(), true});
  input_types.insert(input_types.end(), g.state->input_types.begin(), g.state->input_types.end());

  return add_internal(_state, ConvergeExpr{std::move(g)}, inputs, input_types);
}

FunctionGraph ConstructingGraph::finalize(Span<Oterm> outputs) && {
  assert(_state.owned_fwds.size() > 0);

  _state.output_types = transform_to_vec(outputs, [&](Oterm t) { return type_of(_state, t).id; });

  std::vector<TypeProperties> props = transform_to_vec(_state.output_types, [](TypeID t) {
    return TypeProperties{t, true};
  });

  check_types(_state, props, outputs);

  add_edges(_state, outputs, props);

  for(int i = 0; i < int(_state.owned_fwds.size()); i++) {
    for(int p = 0; p < int(_state.owned_fwds[i].size()); p++) {
      _state.owned_fwds[i][p] = fixup_fwd(type_of(_state, Oterm{i, p, true}), std::move(_state.owned_fwds[i][p]));
    }
  }

  for(int i = 0; i < int(_state.input_borrowed_fwds.size()); i++) {
    _state.input_borrowed_fwds[i] =
      fixup_fwd(type_of(_state, Oterm{0, i, false}), std::move(_state.input_borrowed_fwds[i]));
  }

  return FunctionGraph{std::make_shared<const FunctionGraph::State>(std::move(_state))};
}

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<TypeProperties> types) {
  std::vector<Oterm> terms = make_oterms(0, types);
  return {ConstructingGraph{std::move(types)}, std::move(terms)};
}

FunctionGraph make_graph(AnyFunction f) {
  auto [cg, inputs] = make_graph(f.input_types());
  return std::move(cg).finalize(cg.add(std::move(f), inputs));
}

ConstructingGraph::ConstructingGraph(std::vector<TypeProperties> _input_types) : _state{std::move(_input_types)} {
  auto [value_count, borrow_count] = counts(_state.input_types);
  _state.owned_fwds.push_back(std::vector<ValueForward>(value_count));
  _state.input_borrowed_fwds.resize(borrow_count);
}

Span<TypeProperties> FunctionGraph::input_types() const { return state->input_types; }
Span<TypeID> FunctionGraph::output_types() const { return state->output_types; }

} // namespace ooze
