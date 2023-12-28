#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "ooze/function_graph.h"
#include "user_msg.h"

namespace ooze {

FunctionGraph create_graph(const Env&, const CheckedFunction&);

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
