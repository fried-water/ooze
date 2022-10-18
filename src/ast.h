#pragma once

#include "literal.h"

namespace ooze {
namespace ast {

template <typename F>
struct Expr;

template <typename F>
struct Call {
  F function;
  std::vector<Expr<F>> parameters;

  KNOT_ORDERED(Call);
};

template <typename F>
struct Expr {
  std::variant<Call<F>, std::string, Literal> v;
  Slice ref;

  KNOT_ORDERED(Expr);
};

template <typename T>
struct Parameter {
  std::string name;
  T type;
  bool borrow = false;
  Slice ref;

  KNOT_ORDERED(Parameter);
};

template <typename T>
struct Binding {
  std::string name;
  std::optional<T> type;
  Slice ref;

  KNOT_ORDERED(Binding);
};

template <typename T, typename F>
struct Assignment {
  std::vector<Binding<T>> bindings;
  Expr<F> expr;

  KNOT_ORDERED(Assignment);
};

template <typename T>
struct FunctionHeader {
  std::vector<Parameter<T>> parameters;
  std::vector<T> result;
  Slice ref;

  KNOT_ORDERED(FunctionHeader);
};

template <typename T, typename F>
struct FunctionBody {
  std::vector<Assignment<T, F>> assignments;
  std::vector<Expr<F>> result;

  KNOT_ORDERED(FunctionBody);
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
using UnTypedBody = ast::FunctionBody<NamedType, NamedFunction>;

struct UnTypedFunction {
  std::string name;
  UnTypedHeader header;
  UnTypedBody body;

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
