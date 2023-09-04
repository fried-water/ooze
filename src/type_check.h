#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "user_msg.h"

namespace ooze {

std::vector<const TypedExpr*> undeclared_bindings(const TypedFunction&);

std::optional<Type<TypeID>> unify_types(const Type<TypeID>&, const Type<TypeID>&);

ContextualResult<TypedPattern> type_check(const Env&, TypedPattern, Type<TypeID>);
ContextualResult<TypedFunction>
type_check(const Env&, TypedFunction, std::optional<FunctionType<TypeID>> = {}, bool debug = false);

std::vector<ContextualError> check_fully_resolved(const Env&, const TypedFunction&);

inline FunctionType<TypeID> type_of(const TypedFunction& f) { return {f.pattern.type, f.expr.type}; }

} // namespace ooze
