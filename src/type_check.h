#pragma once

#include "ast.h"
#include "type_cache.h"
#include "user_msg.h"

#include "ooze/type.h"

namespace ooze {

Type unify(const TypeCache&, TypeGraph&, Type, Type, bool recurse);

ContextualResult<void, AST> apply_language_rules(const TypeCache&, const TypeNames&, AST, Span<ASTID>);

struct OverloadResolutionData {
  Map<ASTID, ASTID> overloads;
  std::vector<std::tuple<ASTID, Type, std::vector<ASTID>>> instantiations;
};

ContextualResult<OverloadResolutionData, AST> constraint_propagation(
  Span<std::string_view>,
  const TypeCache&,
  const NativeTypeInfo& native_types,
  const Graph<ASTID>& ident_graph,
  AST,
  Span<ASTID>,
  bool debug = false);

inline bool is_resolved(const TypeGraph& tg, Type t) {
  const auto children = tg.fanout(t);
  return tg.get<TypeTag>(t) != TypeTag::Floating &&
         std::all_of(children.begin(), children.end(), [&](Type child) { return is_resolved(tg, child); });
}

} // namespace ooze
