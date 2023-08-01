#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "ooze/graph.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<FunctionGraph> create_graph(const Env&, const CheckedFunction&);

} // namespace ooze
