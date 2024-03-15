#pragma once

#include "ast.h"
#include "program.h"

#include "ooze/type.h"

namespace ooze {

struct FunctionGraphData {
  Program program;
  std::vector<ASTID> captured_values;
  std::vector<ASTID> captured_borrows;
  FunctionGraph graph;
};

FunctionGraphData create_graph(Program,
                               const AST&,
                               const std::unordered_set<TypeID>& copy_types,
                               const Map<ASTID, ASTID>& overloads,
                               const Map<ASTID, std::vector<ASTID>>& loop_results,
                               ASTID);

} // namespace ooze
