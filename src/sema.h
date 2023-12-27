#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "ooze/src_map.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedExpr> type_name_resolution(const Env&, const UnTypedExpr&);
ContextualResult<TypedPattern> type_name_resolution(const Env&, const UnTypedPattern&);
ContextualResult<TypedAssignment> type_name_resolution(const Env&, const UnTypedAssignment&);
ContextualResult<Type<TypeID>> type_name_resolution(const Env&, const Type<NamedType>&);

TypedPattern inferred_inputs(const TypedExpr&, Set<std::string> active);
ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

ContextualResult2<void, TypeGraph>
type_name_resolution(const SrcMap&, const std::unordered_map<std::string, TypeID>&, TypeGraph);

Graph<ASTID> calculate_ident_graph(const SrcMap&, const AST&);

struct CallGraphData {
  Map<ASTID, std::vector<ASTID>> fn_callers;
  Map<ASTID, ASTID> binding_of;
  std::vector<ASTID> root_fns;
};

ContextualResult2<CallGraphData, AST, TypeGraph>
sema(const SrcMap&,
     const TypeCache&,
     const std::unordered_map<std::string, TypeID>&,
     const std::unordered_set<TypeID>& copy_types,
     AST,
     TypeGraph);

} // namespace ooze
