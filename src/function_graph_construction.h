#pragma once

#include "ast.h"
#include "program.h"
#include "user_msg.h"

#include "ooze/type.h"

namespace ooze {

struct FunctionGraphData {
  std::vector<ASTID> captured_values;
  std::vector<ASTID> captured_borrows;
  FunctionGraph graph;
};

ContextualResult<FunctionGraphData, Program> create_graph(
  Program, const AST&, const std::unordered_set<TypeID>& copy_types, const Map<ASTID, ASTID>& overloads, ASTID);

} // namespace ooze
