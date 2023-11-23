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

ContextualResult<std::vector<TypeID>>
type_name_resolution(const Env&, std::string_view src, const Graph<TypeRef, TypeTag, Slice>&);

std::tuple<Graph<ASTID>, std::vector<ASTID>> calculate_ident_graph(std::string_view src, const AST&);

TypedPattern inferred_inputs(const TypedExpr&, Set<std::string> active);

ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

inline auto type_name_resolution(
  const Env& e, std::string_view src, ContextualResult<std::tuple<AST, UnresolvedTypes>> parse_result) {
  return std::move(parse_result).and_then(applied([&](AST ast, UnresolvedTypes types) {
    return type_name_resolution(e, src, types.graph).map([&](auto type_ids) {
      auto&& [g, tags, slices] = std::move(types.graph).decompose();
      return std::tuple(std::move(ast),
                        Types{std::move(g).append_column(std::move(tags)).append_column(std::move(type_ids)),
                              std::move(types.ast_types)});
    });
  }));
}

} // namespace ooze
