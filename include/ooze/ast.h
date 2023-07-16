#pragma once

#include "ooze/primatives.h"
#include "ooze/type.h"

#include <anyf/type.h>

namespace ooze {

using Literal = std::variant<bool, std::string, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64>;

namespace ast {

struct WildCard {
  KNOT_ORDERED(WildCard);
};

struct Ident {
  std::string name;
  KNOT_ORDERED(Ident);
};

struct Pattern {
  std::variant<std::vector<Pattern>, WildCard, Ident> v;
  Slice ref;
  KNOT_ORDERED(Pattern);
};

template <typename T, typename F>
struct Expr;

template <typename T, typename F>
struct CallExpr {
  F function;
  Indirect<Expr<T, F>> arg;

  KNOT_ORDERED(CallExpr);
};

template <typename T, typename F>
struct BorrowExpr {
  Indirect<Expr<T, F>> expr;
  KNOT_ORDERED(BorrowExpr);
};

template <typename T, typename F>
struct Assignment {
  Pattern pattern;
  CompoundType<T> type;
  Indirect<Expr<T, F>> expr;
  Slice ref;

  KNOT_ORDERED(Assignment);
};

template <typename T, typename F>
struct ScopeExpr {
  std::vector<Assignment<T, F>> assignments;
  Indirect<Expr<T, F>> result;

  KNOT_ORDERED(ScopeExpr);
};

template <typename T, typename F>
struct Expr {
  std::variant<std::vector<Expr<T, F>>, ScopeExpr<T, F>, CallExpr<T, F>, BorrowExpr<T, F>, Ident, Literal> v;
  Slice ref;

  KNOT_ORDERED(Expr);
};

template <typename T>
struct FunctionHeader {
  Pattern pattern;
  FunctionType<T> type;
  Slice ref;

  KNOT_ORDERED(FunctionHeader);
};

template <typename T, typename F>
struct Function {
  FunctionHeader<T> header;
  Expr<T, F> expr;

  KNOT_ORDERED(Function);
};

} // namespace ast

struct NamedType {
  std::string name;
  KNOT_ORDERED(NamedType);
};

struct NamedFunction {
  std::string name;
  KNOT_ORDERED(NamedFunction);
};

using UnTypedAssignment = ast::Assignment<NamedType, NamedFunction>;
using UnTypedScopeExpr = ast::ScopeExpr<NamedType, NamedFunction>;
using UnTypedCallExpr = ast::CallExpr<NamedType, NamedFunction>;
using UnTypedBorrowExpr = ast::BorrowExpr<NamedType, NamedFunction>;
using UnTypedExpr = ast::Expr<NamedType, NamedFunction>;
using UnTypedHeader = ast::FunctionHeader<NamedType>;
using UnTypedFunction = ast::Function<NamedType, NamedFunction>;

using UnTypedAST = std::vector<std::tuple<std::string, UnTypedFunction>>;

struct EnvFunctionRef {
  std::string name;
  int overload_idx = 0;
  FunctionType<anyf::TypeID> type;

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
