#pragma once

#include "ast.h"
#include "ooze/env.h"

#include <anyf/graph.h>

namespace ooze {

using anyf::FunctionGraph;

Result<FunctionGraph> create_graph(const Env&, const AST&, const ast::Expr&, std::function<Any(const std::string&)>);

} // namespace ooze
