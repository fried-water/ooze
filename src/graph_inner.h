#pragma once

#include "ooze/any_function.h"
#include "ooze/graph.h"
#include "ooze/result.h"

namespace ooze {

struct BadArity {
  int expected = 0;
  int given = 0;

  KNOT_ORDERED(BadArity);
};

struct BadType {
  int index = 0;
  TypeID expected;
  TypeID given;

  KNOT_ORDERED(BadType);
};

struct AlreadyMoved {
  int index = 0;
  TypeID type;

  KNOT_ORDERED(AlreadyMoved);
};

struct CannotCopy {
  int index = 0;
  TypeID type;

  KNOT_ORDERED(CannotCopy);
};

struct MismatchedBranchTypes {
  KNOT_ORDERED(MismatchedBranchTypes);
};

using GraphError = std::variant<BadArity, BadType, AlreadyMoved, CannotCopy, MismatchedBranchTypes>;

std::string msg(const GraphError& e);

struct Oterm {
  int node_id = 0;
  int port = 0;
  bool value = false;

  KNOT_ORDERED(Oterm);
};

struct Iterm {
  int node_id = 0;
  int port = 0;
  bool value = false;

  KNOT_ORDERED(Iterm);
};

struct ValueForward {
  std::vector<Iterm> terms;
  int copy_end = 0;
  int move_end = 0;

  KNOT_ORDERED(ValueForward);
};

struct SExpr {
  std::vector<TypeID> types;
};

struct IfExpr {
  FunctionGraph if_branch;
  FunctionGraph else_branch;
};

struct SelectExpr {
  std::vector<TypeProperties> types;
};

struct WhileExpr {
  FunctionGraph body;
};

using Expr = std::variant<std::shared_ptr<const AnyFunction>, SExpr, IfExpr, SelectExpr, WhileExpr>;

class ConstructingGraph {
  struct State;
  std::unique_ptr<State> _state;

  explicit ConstructingGraph(std::vector<TypeProperties>);

public:
  ConstructingGraph() = default;
  ~ConstructingGraph();

  ConstructingGraph(ConstructingGraph&&);
  ConstructingGraph& operator=(ConstructingGraph&&);

  TypeProperties type(Oterm);

  Result<std::vector<Oterm>, GraphError> add(AnyFunction, Span<Oterm>);
  Result<std::vector<Oterm>, GraphError> add(const FunctionGraph&, Span<Oterm>);

  Result<std::vector<Oterm>, GraphError>
  add_functional(Span<TypeProperties>, std::vector<TypeID>, Oterm fn, Span<Oterm>);

  Result<std::vector<Oterm>, GraphError> add_if(FunctionGraph, FunctionGraph, Span<Oterm>);
  Result<std::vector<Oterm>, GraphError> add_select(Oterm cond, Span<Oterm> if_, Span<Oterm> else_);
  Result<std::vector<Oterm>, GraphError> add_while(FunctionGraph, Span<Oterm>);

  Result<FunctionGraph, GraphError> finalize(Span<Oterm>) &&;

  friend std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<TypeProperties>);
};

std::tuple<ConstructingGraph, std::vector<Oterm>> make_graph(std::vector<TypeProperties> input_types);

FunctionGraph make_graph(AnyFunction);

struct FunctionGraph::State {
  std::vector<TypeProperties> input_types;
  std::vector<TypeID> output_types;

  std::vector<std::vector<ValueForward>> owned_fwds;
  std::vector<ValueForward> input_borrowed_fwds;

  std::vector<std::pair<int, int>> input_counts;
  std::vector<Expr> exprs;
};

inline int num_outputs(const Expr& e) {
  return int(std::visit(
    Overloaded{[](const std::shared_ptr<const AnyFunction>& f) { return f->output_types().size(); },
               [](const SExpr& e) { return e.types.size(); },
               [](const IfExpr& e) { return e.if_branch.state->output_types.size(); },
               [](const SelectExpr& e) { return e.types.size(); },
               [](const WhileExpr& e) { return e.body.state->output_types.size() - 1; }},
    e));
}

} // namespace ooze
