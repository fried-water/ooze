#pragma once

#include "literal.h"

namespace ooze {

namespace ast {

struct Call;

struct Expr {
  std::variant<std::vector<Expr>, std::unique_ptr<Call>, std::string, Literal> v;

  KNOT_COMPAREABLE(Expr);
};

struct Type {
  std::string name;
  bool borrow = false;

  KNOT_COMPAREABLE(Type);
};

struct Binding {
  std::string name;
  std::optional<Type> type;

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
  std::vector<Binding> parameters;
  std::vector<Type> result;
  std::vector<Assignment> assignments;
  Expr ret;

  KNOT_COMPAREABLE(Function);
};

inline bool operator==(const std::unique_ptr<Call>& lhs, const std::unique_ptr<Call>& rhs) { return *lhs == *rhs; }

inline bool operator!=(const std::unique_ptr<Call>& lhs, const std::unique_ptr<Call>& rhs) { return !(lhs == rhs); }

} // namespace ast

using AST = std::vector<ast::Function>;

bool validate(const AST&);

std::vector<std::string> typecheck(const AST&);

std::string pretty_print(const AST&);

} // namespace ooze
