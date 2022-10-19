#pragma once

#include "ooze/env.h"
#include "typed_ast.h"
#include "user_msg.h"

#include <anyf/graph.h>

namespace ooze {

ContextualResult<anyf::FunctionGraph> create_graph(const Env&, const CheckedFunction&);

} // namespace ooze
