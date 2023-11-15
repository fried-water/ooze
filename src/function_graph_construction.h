#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "ooze/function_graph.h"
#include "user_msg.h"

namespace ooze {

FunctionGraph create_graph(const Env&, const CheckedFunction&);

} // namespace ooze
