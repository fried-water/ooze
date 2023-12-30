#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "user_msg.h"

namespace ooze {

std::vector<const TypedExpr*> undeclared_bindings(const TypedFunction&);

std::optional<Type<TypeID>> unify_types(const Type<TypeID>&, const Type<TypeID>&);

ContextualResult<TypedPattern> type_check(const Env&, TypedPattern, Type<TypeID>);
ContextualResult<TypedFunction>
type_check(const Env&, TypedFunction, std::optional<FunctionType<TypeID>> = {}, bool debug = false);

std::vector<ContextualError> check_fully_resolved(const Env&, const TypedFunction&);

inline FunctionType<TypeID> type_of(const TypedFunction& f) { return {f.pattern.type, f.expr.type}; }

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
  Span<std::string_view>, const std::vector<std::vector<ASTPropagation>>&, const AST&, const TypeGraph&);

ContextualResult2<void, AST, TypeGraph> apply_language_rules(Span<std::string_view>, const TypeCache&, AST, TypeGraph);

ContextualResult2<void, AST, TypeGraph> constraint_propagation(
  Span<std::string_view>,
  const TypeCache&,
  const std::unordered_set<TypeID>& copy_types,
  const Graph<ASTID>& ident_graph,
  const std::vector<std::vector<ASTPropagation>>&,
  AST,
  TypeGraph,
  bool debug = false);

} // namespace ooze
