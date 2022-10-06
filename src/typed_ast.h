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
using TypedBody = ast::FunctionBody<anyf::TypeID, NamedFunction>;

// Replace function names with specific function overloads
using CheckedExpr = ast::Expr<EnvFunctionRef>;
using CheckedAssignment = ast::Assignment<anyf::TypeID, EnvFunctionRef>;
using CheckedBody = ast::FunctionBody<anyf::TypeID, EnvFunctionRef>;

struct TypedFunction {
  TypedHeader header;
  TypedBody body;

  KNOT_ORDERED(TypedFunction);
};

struct CheckedFunction {
  TypedHeader header;
  CheckedBody body;

  KNOT_ORDERED(CheckedFunction);
};

} // namespace ooze
