#pragma once

#include "ast.h"
#include "ooze/env.h"

#include <anyf/graph.h>

namespace ooze {

using anyf::FunctionGraph;

Result<Map<std::string, FunctionGraph>> create_graphs(const Env&, const AST&, std::function<Any(const std::string&)>);

Result<FunctionGraph> create_graph(const Env&,
                                   const ast::Expr&,
                                   const Map<std::string, FunctionGraph>&,
                                   std::function<Any(const std::string&)>);

} // namespace ooze
