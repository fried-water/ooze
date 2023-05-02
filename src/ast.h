#pragma once

#include "literal.h"
#include "ooze/type.h"

namespace ooze {
namespace ast {

struct IdentExpr {
  std::string name;
  KNOT_ORDERED(IdentExpr);
};

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
  std::vector<Expr<T, F>> parameters;

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
  std::variant<std::vector<Expr<T, F>>, ScopeExpr<T, F>, CallExpr<T, F>, BorrowExpr<T, F>, IdentExpr, Literal> v;
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

} // namespace ooze
