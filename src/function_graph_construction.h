#pragma once

#include "ooze/ast.h"
#include "ooze/program.h"
#include "ooze/type.h"

namespace ooze {

struct FunctionGraphData {
  Program program;
  std::vector<ASTID> global_values;
  std::vector<ASTID> global_borrows;
  FunctionGraph graph;
};

FunctionGraphData create_graph(Program,
                               const AST&,
                               const TypeGraph&,
                               const std::unordered_set<TypeID>& copy_types,
                               const Map<ASTID, ASTID>& binding_of,
                               ASTID);

} // namespace ooze
