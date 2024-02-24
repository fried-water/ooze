#pragma once

#include "type_cache.h"
#include "user_msg.h"

#include "ooze/ast.h"
#include "ooze/src_map.h"
#include "ooze/type.h"

namespace ooze {

ContextualResult<void, TypeGraph>
type_name_resolution(Span<std::string_view>, const TypeNames&, const std::vector<std::pair<Type, SrcRef>>&, TypeGraph);

ContextualResult<Graph<ASTID>>
calculate_ident_graph(Span<std::string_view>, const AST&, Span<ASTID> roots, Span<ASTID> global_imports = {});

struct SemaData {
  Map<ASTID, ASTID> overloads;
  std::vector<ASTID> resolved_roots;
  std::vector<ASTID> generic_roots;
};

ContextualResult<SemaData, AST>
sema(Span<std::string_view>,
     const TypeCache&,
     const NativeTypeInfo&,
     AST,
     Span<ASTID> root,
     Span<ASTID> global_imports = {});

} // namespace ooze
