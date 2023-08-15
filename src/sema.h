#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedExpr> type_name_resolution(const Env&, const UnTypedExpr&);
ContextualResult<TypedPattern> type_name_resolution(const Env&, const UnTypedPattern&);
ContextualResult<TypedAssignment> type_name_resolution(const Env&, const UnTypedAssignment&);
ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env&, const CompoundType<NamedType>&);

TypedPattern inferred_inputs(const TypedExpr&, Set<std::string> active);

ContextualResult<CheckedFunction> overload_resolution(const Env&, const TypedFunction&);

} // namespace ooze
