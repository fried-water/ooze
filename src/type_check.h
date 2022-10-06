#pragma once

#include "ast.h"
#include "ooze/env.h"
#include "typed_ast.h"

#include <anyf/any_function.h>

namespace ooze {

UnTypedBody convert_to_function_body(std::variant<UnTypedExpr, UnTypedAssignment>);

Result<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
Result<TypedBody> type_name_resolution(const Env&, const UnTypedBody&);

Result<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

Result<CheckedFunction>
overload_resolution(const Env&, const TypedBody&, const std::unordered_map<std::string, TypeID>&);

} // namespace ooze
