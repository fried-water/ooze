#pragma once

#include "ooze/env.h"
#include "typed_ast.h"

#include <anyf/graph.h>

namespace ooze {

Result<anyf::FunctionGraph> create_graph(const Env&, const TypedFunction&);

} // namespace ooze
