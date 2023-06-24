#pragma once

#include "ooze/ast.h"
#include "ooze/env.h"

namespace ooze {

std::string pretty_print(const Env&, TypeID);

std::string pretty_print(const ast::Pattern&);

std::string pretty_print(const CompoundType<NamedType>&);
std::string pretty_print(const Env&, const CompoundType<TypeID>&);

std::string pretty_print(const Env&, const FunctionType<TypeID>&);

std::string pretty_print(const UnTypedAST& ast);

std::string pretty_print(const UnTypedFunction&);
std::string pretty_print(const UnTypedHeader&);
std::string pretty_print(const UnTypedAssignment&);
std::string pretty_print(const UnTypedExpr&);

std::string pretty_print(const Env&, const TypedFunction&);
std::string pretty_print(const Env&, const TypedHeader&);
std::string pretty_print(const Env&, const TypedAssignment&);
std::string pretty_print(const Env&, const TypedExpr&);

std::string pretty_print(const Env&, const CheckedFunction&);
std::string pretty_print(const Env&, const CheckedHeader&);
std::string pretty_print(const Env&, const CheckedAssignment&);
std::string pretty_print(const Env&, const CheckedExpr&);

template <typename R, typename T>
R untype(const Env& e, const T& t) {
  return knot::map<R>(t,
                      Overloaded{[&](TypeID t) { return NamedType{pretty_print(e, t)}; },
                                 [&](const EnvFunctionRef& r) { return NamedFunction{r.name}; }});
}

} // namespace ooze
