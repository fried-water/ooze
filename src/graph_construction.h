#pragma once

#include "ast.h"
#include "ooze/env.h"

#include <anyf/graph.h>

namespace ooze {

using anyf::FunctionGraph;

Result<Map<std::string, FunctionGraph>> create_graphs(const Env&, const AST&);

} // namespace ooze
