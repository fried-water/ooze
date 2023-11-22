#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
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

TypeRef unify(Graph<TypeRef, TypeTag, TypeID>&, TypeRef, TypeRef, bool recurse);

ContextualResult<Types> apply_language_rules(const Env&, const AST&, Types);

ContextualResult<Types>
type_check(std::string_view,
           const Env&,
           const Map<ASTID, ASTID>& ident_to_binding,
           const std::vector<ASTID>& undeclared_bindings,
           const AST&,
           Types,
           bool debug = false);

} // namespace ooze
