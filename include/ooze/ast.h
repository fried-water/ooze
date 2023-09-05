#pragma once

#include "ooze/primitives.h"
#include "ooze/type.h"

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

template <typename T>
struct Pattern {
  std::variant<std::vector<Pattern<T>>, WildCard, Ident> v;
  Type<T> type = floating_type<T>();
  Slice ref;
  KNOT_ORDERED(Pattern);
};

template <typename T, typename... Extras>
struct Expr;

template <typename T, typename... Extras>
struct CallExpr {
  Indirect<Expr<T, Extras...>> callee;
  Indirect<Expr<T, Extras...>> arg;

  KNOT_ORDERED(CallExpr);
};

template <typename T, typename... Extras>
struct SelectExpr {
  Indirect<Expr<T, Extras...>> condition;
  Indirect<Expr<T, Extras...>> if_expr;
  Indirect<Expr<T, Extras...>> else_expr;

  KNOT_ORDERED(SelectExpr);
};

template <typename T, typename... Extras>
struct BorrowExpr {
  Indirect<Expr<T, Extras...>> expr;
  KNOT_ORDERED(BorrowExpr);
};

template <typename T, typename... Extras>
struct Assignment {
  Pattern<T> pattern;
  Indirect<Expr<T, Extras...>> expr;

  KNOT_ORDERED(Assignment);
};

template <typename T, typename... Extras>
struct ScopeExpr {
  std::vector<Assignment<T, Extras...>> assignments;
  Indirect<Expr<T, Extras...>> result;

  KNOT_ORDERED(ScopeExpr);
};

template <typename T, typename... Extras>
using ExprVariant =
  std::variant<std::vector<Expr<T, Extras...>>,
               ScopeExpr<T, Extras...>,
               SelectExpr<T, Extras...>,
               CallExpr<T, Extras...>,
               BorrowExpr<T, Extras...>,
               Ident,
               Literal,
               Extras...>;

template <typename T, typename... Extras>
struct Expr {
  ExprVariant<T, Extras...> v;
  Type<T> type = floating_type<T>();
  Slice ref;
  KNOT_ORDERED(Expr);
};

template <typename T, typename... Extras>
struct Function {
  Pattern<T> pattern;
  Expr<T, Extras...> expr;

  KNOT_ORDERED(Function);
};

} // namespace ast

struct NamedType {
  std::string name;
  KNOT_ORDERED(NamedType);
};

struct EnvFunctionRef {
  std::string name;
  int overload_idx = 0;

  KNOT_ORDERED(EnvFunctionRef);
};

using UnTypedPattern = ast::Pattern<NamedType>;
using UnTypedAssignment = ast::Assignment<NamedType>;
using UnTypedScopeExpr = ast::ScopeExpr<NamedType>;
using UnTypedSelectExpr = ast::SelectExpr<NamedType>;
using UnTypedCallExpr = ast::CallExpr<NamedType>;
using UnTypedBorrowExpr = ast::BorrowExpr<NamedType>;
using UnTypedExpr = ast::Expr<NamedType>;
using UnTypedFunction = ast::Function<NamedType>;

using UnTypedAST = std::vector<std::tuple<std::string, UnTypedFunction>>;

// Replace type names with type id
using TypedPattern = ast::Pattern<TypeID>;
using TypedAssignment = ast::Assignment<TypeID>;
using TypedScopeExpr = ast::ScopeExpr<TypeID>;
using TypedSelectExpr = ast::SelectExpr<TypeID>;
using TypedCallExpr = ast::CallExpr<TypeID>;
using TypedBorrowExpr = ast::BorrowExpr<TypeID>;
using TypedExpr = ast::Expr<TypeID>;
using TypedFunction = ast::Function<TypeID>;

// Replace function names with specific function overloads
using CheckedAssignment = ast::Assignment<TypeID, EnvFunctionRef>;
using CheckedScopeExpr = ast::ScopeExpr<TypeID, EnvFunctionRef>;
using CheckedSelectExpr = ast::SelectExpr<TypeID, EnvFunctionRef>;
using CheckedCallExpr = ast::CallExpr<TypeID, EnvFunctionRef>;
using CheckedBorrowExpr = ast::BorrowExpr<TypeID, EnvFunctionRef>;
using CheckedExpr = ast::Expr<TypeID, EnvFunctionRef>;
using CheckedFunction = ast::Function<TypeID, EnvFunctionRef>;

} // namespace ooze
