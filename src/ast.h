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

template <typename F>
struct Expr;

template <typename F>
struct Call {
  F function;
  std::vector<Expr<F>> parameters;

  KNOT_ORDERED(Call);
};

template <typename F>
struct BorrowExpr {
  Indirect<Expr<F>> e;
  KNOT_ORDERED(BorrowExpr);
};

template <typename F>
struct Expr {
  std::variant<std::vector<Expr<F>>, Call<F>, BorrowExpr<F>, IdentExpr, Literal> v;
  Slice ref;
  KNOT_ORDERED(Expr);
};

template <typename T, typename F>
struct Assignment {
  Pattern pattern;
  CompoundType<T> type;
  Expr<F> expr;
  Slice ref;

  KNOT_ORDERED(Assignment);
};

template <typename T>
struct FunctionHeader {
  Pattern pattern;
  FunctionType<T> type;
  Slice ref;

  KNOT_ORDERED(FunctionHeader);
};

template <typename T, typename F>
struct Scope {
  std::vector<Assignment<T, F>> assignments;
  Expr<F> result;

  KNOT_ORDERED(Scope);
};

template <typename T, typename F>
struct Function {
  FunctionHeader<T> header;
  Scope<T, F> scope;

  KNOT_ORDERED(Function);
};

} // namespace ast

struct NamedType {
  std::string name;
  Slice ref;

  KNOT_ORDERED(NamedType);
};

struct NamedFunction {
  std::string name;
  KNOT_ORDERED(NamedFunction);
};

using UnTypedExpr = ast::Expr<NamedFunction>;
using UnTypedAssignment = ast::Assignment<NamedType, NamedFunction>;
using UnTypedHeader = ast::FunctionHeader<NamedType>;
using UnTypedScope = ast::Scope<NamedType, NamedFunction>;
using UnTypedFunction = ast::Function<NamedType, NamedFunction>;

using AST = std::vector<std::tuple<std::string, UnTypedFunction>>;

std::string pretty_print(const AST&);
std::string pretty_print(const std::tuple<std::string, UnTypedFunction>&);
std::string pretty_print(const UnTypedFunction&);
std::string pretty_print(const UnTypedAssignment&);
std::string pretty_print(const ast::Expr<NamedFunction>&);
std::string pretty_print(const ast::Call<NamedFunction>&);
std::string pretty_print(const CompoundType<NamedType>&);
std::string pretty_print(const ast::Pattern&);

} // namespace ooze
