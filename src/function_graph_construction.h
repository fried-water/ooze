#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "ooze/function_graph.h"
#include "user_msg.h"

namespace ooze {

FunctionGraph create_graph(const Env&, const CheckedFunction&);

FunctionGraph create_graph(
  const Env&, const AST&, const TypeGraph&, const Map<ASTID, EnvFunctionRef>&, const Graph<ASTID>& ident_graph, ASTID);

} // namespace ooze
