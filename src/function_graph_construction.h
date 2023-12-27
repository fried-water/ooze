#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"
#include "ooze/function_graph.h"
#include "user_msg.h"

namespace ooze {

FunctionGraph create_graph(const Env&, const CheckedFunction&);

FunctionGraph create_graph(const std::unordered_set<TypeID>& copy_types,
                           const AST&,
                           const TypeGraph&,
                           const Map<ASTID, AsyncFn>&,
                           const Map<ASTID, ASTID>& binding_of,
                           ASTID fn_id);

} // namespace ooze
