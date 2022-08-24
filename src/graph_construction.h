#pragma once

#include "ast.h"
#include "ooze/env.h"

#include <anyf/graph.h>

namespace ooze {

using anyf::FunctionGraph;

std::pair<Env, std::vector<std::string>> create_graphs(Env, const AST&);

Result<FunctionGraph> create_graph(const Env& e, const ast::Function& f);

Result<FunctionGraph>
create_graph(const Env&, const ast::Expr&, const std::vector<std::pair<std::string, TypeProperties>>&);

std::vector<std::pair<std::string, TypeProperties>>
inputs_of(const Env&, const ast::Expr&, std::function<std::optional<TypeID>(const std::string&)> type_of);

} // namespace ooze
