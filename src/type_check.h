#pragma once

#include "ast.h"
#include "ooze/env.h"
#include "typed_ast.h"
#include "user_msg.h"

#include <anyf/any_function.h>

namespace ooze {

UnTypedBody convert_to_function_body(std::variant<UnTypedExpr, UnTypedAssignment>);

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedBody> type_name_resolution(const Env&, const UnTypedBody&);

ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

ContextualResult<CheckedFunction>
overload_resolution(const Env&, const TypedBody&, const std::unordered_map<std::string, TypeID>&);

} // namespace ooze
