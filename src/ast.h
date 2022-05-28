#pragma once

#include "indirect.h"
#include "literal.h"

namespace ooze {

namespace ast {

struct Call;

struct Expr {
  std::variant<Indirect<Call>, std::string, Literal> v;

  KNOT_COMPAREABLE(Expr);
};

struct Parameter {
  std::string name;
  std::string type;
  bool borrow = false;

  KNOT_COMPAREABLE(Parameter);
};

struct Binding {
  std::string name;
  std::optional<std::string> type;

  KNOT_COMPAREABLE(Binding);
};

struct Call {
  std::string name;
  std::vector<Expr> parameters;

  KNOT_COMPAREABLE(Call);
};

struct Assignment {
  std::vector<Binding> variables;
  Expr expr;

  KNOT_COMPAREABLE(Assignment);
};

struct Function {
  std::string name;
  std::vector<Parameter> parameters;
  std::vector<std::string> result;
  std::vector<Assignment> assignments;
  std::vector<Expr> ret;

  KNOT_COMPAREABLE(Function);
};

} // namespace ast

using AST = std::vector<ast::Function>;

bool validate(const AST&);

std::vector<std::string> typecheck(const AST&);

std::string pretty_print(const AST&);

} // namespace ooze
