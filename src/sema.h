#pragma once

#include "ooze/ast_flat.h"
#include "ooze/src_map.h"
#include "user_msg.h"

namespace ooze {

ContextualResult2<void, TypeGraph> type_name_resolution(
  Span<std::string_view>, const TypeNames&, const std::vector<std::pair<TypeRef, SrcRef>>&, TypeGraph);

ContextualResult2<Graph<ASTID>> calculate_ident_graph(Span<std::string_view>, const AST&);

struct CallGraphData {
  Graph<ASTID> call_graph;
  std::vector<ASTID> leaf_fns;
  Map<ASTID, ASTID> binding_of;
};

ContextualResult2<CallGraphData, AST, TypeGraph>
sema(Span<std::string_view>, const TypeCache&, const NativeTypeInfo&, AST, TypeGraph);

} // namespace ooze
