#pragma once

#include "ooze/env.h"
#include "typed_ast.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedHeader> type_name_resolution(const Env&, const UnTypedHeader&);
ContextualResult<TypedExpr> type_name_resolution(const Env&, const UnTypedExpr&);
ContextualResult<TypedAssignment> type_name_resolution(const Env&, const UnTypedAssignment&);
ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env&, const CompoundType<NamedType>&);

TypedHeader inferred_header(const TypedExpr&);

// TODO return Unresolved or CheckedFunction
ContextualResult<CheckedFunction> overload_resolution(const Env&, TypedFunction, bool debug = false);

ContextualResult<CompoundType<TypeID>> type_check(const Env&, const ast::Pattern&, CompoundType<TypeID>);

} // namespace ooze
