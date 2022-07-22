#pragma once

#include "anyf/graph.h"
#include "ast.h"
#include "ooze/env.h"

namespace ooze {

std::string function_string(const Env&, std::string_view fn_name, const anyf::FunctionGraph&);
std::string function_string(const Env&, std::string_view fn_name, const anyf::AnyFunction&);

Result<void> type_check(const Env&, const std::vector<ast::Binding>&, const std::vector<TypeID>&);

} // namespace ooze
