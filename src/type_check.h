#pragma once

#include "ast.h"
#include "ooze/env.h"
#include "typed_ast.h"
#include "user_msg.h"

#include <anyf/any_function.h>

namespace ooze {

UnTypedScope convert_to_scope(std::variant<UnTypedExpr, UnTypedAssignment>);

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedScope> type_name_resolution(const Env&, const UnTypedScope&);

ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

ContextualResult<CheckedFunction>
overload_resolution(const Env&, const TypedScope&, const std::unordered_map<std::string, TypeID>&);

} // namespace ooze
