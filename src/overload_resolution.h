#pragma once

#include "ast.h"
#include "ooze/env.h"
#include "typed_ast.h"

#include <anyf/any_function.h>

namespace ooze {

Result<TypedFunction> overload_resolution(const Env&, const UnTypedFunction&);

Result<TypedFunction> overload_resolution(const Env&,
                                          const std::variant<UnTypedExpr, UnTypedAssignment>&,
                                          const std::unordered_map<std::string, TypeID>&);

} // namespace ooze
