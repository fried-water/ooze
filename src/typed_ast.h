#pragma once

#include "ast.h"

namespace ooze {

struct EnvFunctionRef {
  std::string name;
  int overload_idx = 0;

  KNOT_ORDERED(EnvFunctionRef);
};

// Replace type names with type id
using TypedAssignment = ast::Assignment<anyf::TypeID, NamedFunction>;
using TypedScopeExpr = ast::ScopeExpr<anyf::TypeID, NamedFunction>;
using TypedCallExpr = ast::CallExpr<anyf::TypeID, NamedFunction>;
using TypedBorrowExpr = ast::BorrowExpr<anyf::TypeID, NamedFunction>;
using TypedExpr = ast::Expr<anyf::TypeID, NamedFunction>;
using TypedHeader = ast::FunctionHeader<anyf::TypeID>;
using TypedFunction = ast::Function<anyf::TypeID, NamedFunction>;

// Replace function names with specific function overloads
using CheckedAssignment = ast::Assignment<anyf::TypeID, EnvFunctionRef>;
using CheckedScopeExpr = ast::ScopeExpr<anyf::TypeID, EnvFunctionRef>;
using CheckedCallExpr = ast::CallExpr<anyf::TypeID, EnvFunctionRef>;
using CheckedBorrowExpr = ast::BorrowExpr<anyf::TypeID, EnvFunctionRef>;
using CheckedExpr = ast::Expr<anyf::TypeID, EnvFunctionRef>;
using CheckedHeader = ast::FunctionHeader<anyf::TypeID>;
using CheckedFunction = ast::Function<anyf::TypeID, EnvFunctionRef>;

} // namespace ooze
