#pragma once

#include "indirect.h"
#include "literal.h"

namespace ooze {
namespace ast {

template <typename F>
struct Call;

template <typename F>
struct Expr {
  std::variant<Indirect<Call<F>>, std::string, Literal> v;

  friend constexpr auto names(knot::Type<Expr>) { return knot::Names("Expr", {"v"}); }

  KNOT_ORDERED(Expr);
};

template <typename F>
struct Call {
  F function;
  std::vector<Expr<F>> parameters;

  friend constexpr auto names(knot::Type<Call>) { return knot::Names("Call", {"function", "parameters"}); }

  KNOT_ORDERED(Call);
};

template <typename T>
struct Parameter {
  std::string name;
  T type;
  bool borrow = false;

  KNOT_ORDERED(Parameter);
};

template <typename T>
struct Binding {
  std::string name;
  std::optional<T> type;

  KNOT_ORDERED(Binding);
};

template <typename T, typename F>
struct Assignment {
  std::vector<Binding<T>> bindings;
  Expr<F> expr;

  KNOT_ORDERED(Assignment);
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

using UnTypedExpr = ast::Expr<NamedFunction>;
using UnTypedAssignment = ast::Assignment<NamedType, NamedFunction>;

struct UnTypedFunction {
  std::string name;
  std::vector<ast::Parameter<NamedType>> parameters;
  std::vector<NamedType> result;
  std::vector<UnTypedAssignment> assignments;
  std::vector<UnTypedExpr> ret;

  KNOT_ORDERED(UnTypedFunction);
};

using AST = std::vector<UnTypedFunction>;

std::string pretty_print(const AST&);
std::string pretty_print(const UnTypedFunction&);
std::string pretty_print(const UnTypedAssignment&);
std::string pretty_print(const ast::Expr<NamedFunction>&);
std::string pretty_print(const ast::Call<NamedFunction>&);
std::string pretty_print(const ast::Parameter<NamedType>&);
std::string pretty_print(const ast::Binding<NamedType>&);

} // namespace ooze
