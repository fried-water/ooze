#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "user_msg.h"

#include <anyf/graph.h>

namespace ooze {

ContextualResult<anyf::FunctionGraph> create_graph(const Env&, const CheckedFunction&);

} // namespace ooze
