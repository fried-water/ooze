#pragma once

#include "type_cache.h"
#include "user_msg.h"

#include "ooze/ast.h"
#include "ooze/src_map.h"
#include "ooze/type.h"

namespace ooze {

ContextualResult<void, TypeGraph>
type_name_resolution(Span<std::string_view>, const TypeNames&, const std::vector<std::pair<Type, SrcRef>>&, TypeGraph);

ContextualResult<Graph<ASTID>> calculate_ident_graph(Span<std::string_view>, const AST&);

struct CallGraphData {
  Graph<ASTID> call_graph;
  std::vector<ASTID> topographical_fn_ordering;
  Map<ASTID, ASTID> binding_of;
};

ContextualResult<CallGraphData, AST, TypeGraph>
sema(Span<std::string_view>, const TypeCache&, const NativeTypeInfo&, AST, TypeGraph);

} // namespace ooze
