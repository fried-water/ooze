#pragma once

#include "ast.h"

namespace ooze {

struct EnvFunctionRef {
  std::string name;
  int overload_idx = 0;

  KNOT_ORDERED(EnvFunctionRef);
};

// Replace type names with type id
using TypedExpr = UnTypedExpr;
using TypedHeader = ast::FunctionHeader<anyf::TypeID>;
using TypedAssignment = ast::Assignment<anyf::TypeID, NamedFunction>;
using TypedScope = ast::Scope<anyf::TypeID, NamedFunction>;
using TypedFunction = ast::Function<anyf::TypeID, NamedFunction>;

// Replace function names with specific function overloads
using CheckedExpr = ast::Expr<EnvFunctionRef>;
using CheckedAssignment = ast::Assignment<anyf::TypeID, EnvFunctionRef>;
using CheckedScope = ast::Scope<anyf::TypeID, EnvFunctionRef>;
using CheckedFunction = ast::Function<anyf::TypeID, EnvFunctionRef>;

} // namespace ooze
