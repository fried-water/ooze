#pragma once

#include "ooze/ast.h"
#include "ooze/ast_flat.h"
#include "ooze/env.h"

namespace ooze {

std::string pretty_print(const Env&, TypeID);

std::string pretty_print(const UnTypedPattern&);
std::string pretty_print(const TypedPattern&);

std::string pretty_print(const Type<NamedType>&);
std::string pretty_print(const Env&, const Type<TypeID>&);

std::string pretty_print(const Env&, const FunctionType<TypeID>&);

std::string pretty_print(const UnTypedAST& ast);

std::string pretty_print(const UnTypedFunction&);
std::string pretty_print(const UnTypedAssignment&);
std::string pretty_print(const UnTypedExpr&);

std::string pretty_print(const Env&, const TypedFunction&);
std::string pretty_print(const Env&, const TypedAssignment&);
std::string pretty_print(const Env&, const TypedExpr&);

std::string pretty_print(const Env&, const CheckedFunction&);
std::string pretty_print(const Env&, const CheckedAssignment&);
std::string pretty_print(const Env&, const CheckedExpr&);

std::string pretty_print(const SrcMap&, const Env&, const AST&, const Types&, std::optional<ASTID> = {});

std::string pretty_print(const Env&, const TypeGraph&, TypeRef);

template <typename R, typename T>
R untype(const Env& e, const T& t) {
  return knot::map<R>(t,
                      Overloaded{[&](TypeID t) { return NamedType{pretty_print(e, t)}; },
                                 [&](const EnvFunctionRef& r) { return ast::Ident{r.name}; }});
}

} // namespace ooze
