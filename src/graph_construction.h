#pragma once

#include "ast.h"
#include "env.h"

#include <graph.h>

namespace ooze {

using anyf::FunctionGraph;

Map<std::string, FunctionGraph> create_graphs(const Env&, const AST&);

} // namespace ooze
