#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedExpr> type_name_resolution(const Env&, const UnTypedExpr&);
ContextualResult<TypedPattern> type_name_resolution(const Env&, const UnTypedPattern&);
ContextualResult<TypedAssignment> type_name_resolution(const Env&, const UnTypedAssignment&);
ContextualResult<Type<TypeID>> type_name_resolution(const Env&, const Type<NamedType>&);

ContextualResult2<TypeGraph> type_name_resolution(const SrcMap&, const Env&, TypeGraph);

std::tuple<Graph<ASTID>, std::vector<ASTID>> calculate_ident_graph(const SrcMap&, const AST&);

TypedPattern inferred_inputs(const TypedExpr&, Set<std::string> active);

ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

inline auto
type_name_resolution(const SrcMap& sm, const Env& e, ContextualResult2<std::tuple<AST, Types>> parse_result) {
  return std::move(parse_result).and_then(applied([&](AST ast, Types types) {
    return type_name_resolution(sm, e, std::move(types.graph)).map([&](auto type_graph) {
      return std::tuple(std::move(ast), Types{std::move(types.graph), std::move(types.ast_types)});
    });
  }));
}

} // namespace ooze
