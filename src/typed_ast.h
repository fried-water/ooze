#pragma once

#include "ast.h"

namespace ooze {

struct EnvFunctionRef {
  std::string name;
  int overload_idx = 0;

  KNOT_ORDERED(EnvFunctionRef);
};

using TypedExpr = ast::Expr<EnvFunctionRef>;
using TypedAssignment = ast::Assignment<anyf::TypeID, EnvFunctionRef>;

struct TypedFunction {
  std::vector<ast::Parameter<anyf::TypeID>> parameters;
  std::vector<TypedAssignment> assignments;
  std::vector<TypedExpr> ret;

  KNOT_ORDERED(TypedFunction);
};

} // namespace ooze
