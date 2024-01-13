#pragma once

#include "ooze/ast_flat.h"
#include "user_msg.h"

namespace ooze {

struct DirectProp {};
struct FloatingProp {};
struct TupleProp {
  int idx;
  int size;
};
struct BorrowProp {};
struct FnInputProp {};
struct FnOutputProp {};

using Propagation = std::variant<DirectProp, TupleProp, BorrowProp, FnInputProp, FnOutputProp, FloatingProp>;

struct ASTPropagation {
  ASTID target = {};
  bool wrap = false;
  Propagation propagation = DirectProp{};
};

std::vector<std::vector<ASTPropagation>>
calculate_propagations(const Graph<ASTID>& ident_graph, const Forest<ASTTag, ASTID>&);

TypeRef unify(const TypeCache&, TypeGraph&, TypeRef, TypeRef, bool recurse);

std::tuple<ASTID, TypeRef, int>
overload_resolution(const TypeCache&, TypeGraph&, const Graph<ASTID>& ident_graph, const std::vector<TypeRef>&, ASTID);

std::vector<ContextualError2> check_fully_resolved(
  Span<std::string_view>,
  const std::vector<std::vector<ASTPropagation>>&,
  const AST&,
  const TypeGraph&,
  const TypeNames&);

ContextualResult2<void, AST, TypeGraph>
apply_language_rules(Span<std::string_view>, const TypeCache&, const TypeNames&, AST, TypeGraph);

ContextualResult2<void, AST, TypeGraph> constraint_propagation(
  Span<std::string_view>,
  const TypeCache&,
  const NativeTypeInfo& native_types,
  const Graph<ASTID>& ident_graph,
  const std::vector<std::vector<ASTPropagation>>&,
  AST,
  TypeGraph,
  bool debug = false);

} // namespace ooze
