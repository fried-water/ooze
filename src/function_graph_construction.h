#pragma once

#include "function_graph.h"
#include "user_msg.h"

#include "ooze/ast.h"

namespace ooze {

struct FunctionGraphData {
  std::vector<ASTID> global_values;
  std::vector<ASTID> global_borrows;
  FunctionGraph fg;
};

FunctionGraphData create_graph(const AST&,
                               const TypeGraph&,
                               const std::unordered_set<TypeID>& copy_types,
                               const Map<ASTID, ASTID>& binding_of,
                               ASTID);

} // namespace ooze
