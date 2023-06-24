#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"
#include "user_msg.h"

namespace ooze {

ContextualResult<TypedFunction> type_name_resolution(const Env&, const UnTypedFunction&);
ContextualResult<TypedHeader> type_name_resolution(const Env&, const UnTypedHeader&);
ContextualResult<TypedExpr> type_name_resolution(const Env&, const UnTypedExpr&);
ContextualResult<TypedAssignment> type_name_resolution(const Env&, const UnTypedAssignment&);
ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env&, const CompoundType<NamedType>&);

TypedHeader inferred_header(const TypedExpr&);

ContextualResult<CompoundType<TypeID>> type_check(const Env&, const ast::Pattern&, CompoundType<TypeID>);

ContextualResult<std::variant<CheckedFunction, TypedFunction>>
overload_resolution(const Env&, TypedFunction, std::optional<FunctionType<TypeID>> type = {}, bool debug = false);

ContextualResult<CheckedFunction> overload_resolution_concrete(const Env&,
                                                               TypedFunction,
                                                               std::optional<FunctionType<TypeID>> type = {},
                                                               bool debug = false);

} // namespace ooze
