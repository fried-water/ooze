#include "pch.h"

#include "ast.h"

namespace ooze {

using namespace ast;

namespace {

template <typename T>
void pretty_print(std::ostream&, const std::vector<T>&, bool allow_short = false);

void pretty_print(std::ostream& os, const std::string& s) { os << s; }

void pretty_print(std::ostream& os, const Parameter& t) {
  os << t.name << ": ";

  if(t.borrow) {
    os << '&';
  }
  os << t.type;
}

void pretty_print(std::ostream& os, const Binding& b) {
  os << b.name;

  if(b.type) {
    os << ": " << *b.type;
  }
}

void pretty_print(std::ostream&, const Expr&);

void pretty_print(std::ostream& os, const Call& c) { pretty_print(os << c.name, c.parameters); }

struct ExprPrettyPrinter {
  std::ostream& os;

  void operator()(const std::vector<Expr>& exprs) const { pretty_print(os, exprs, true); }
  void operator()(const Indirect<Call>& call) const { pretty_print(os, *call); }
  void operator()(const std::string& name) const { os << name; }
  void operator()(const Literal& l) const { os << to_string(l); }
};

void pretty_print(std::ostream& os, const Expr& e) { std::visit(ExprPrettyPrinter{os}, e.v); }

void pretty_print(std::ostream& os, const Assignment& a) {
  pretty_print(os << "let ", a.bindings, true);
  pretty_print(os << " = ", a.expr);
}

void pretty_print(std::ostream& os, const Function& f) {
  pretty_print(os << "fn " << f.name, f.parameters);
  pretty_print(os << " -> ", f.result, true);
  os << " {\n";

  for(const Assignment& a : f.assignments) {
    pretty_print(os << "  ", a);
    os << "\n";
  }

  pretty_print(os << "  ", f.ret);
  os << "\n}\n";
}

template <typename T>
void pretty_print(std::ostream& os, const std::vector<T>& v, bool allow_short) {
  if(v.empty()) {
    os << "()";
  } else if(v.size() == 1 && allow_short) {
    pretty_print(os, v.front());
  } else {
    os << "(";
    pretty_print(os, v.front());

    std::for_each(v.begin() + 1, v.end(), [&](const auto& ele) { pretty_print(os << ", ", ele); });

    os << ")";
  }
}

} // namespace

std::string pretty_print(const AST& ast) {
  std::stringstream ss;

  for(const Function& f : ast) {
    pretty_print(ss, f);
    ss << "\n";
  }

  return std::move(ss).str();
}

std::string pretty_print(const ast::Expr& expr) {
  std::stringstream ss;
  pretty_print(ss, expr);
  return std::move(ss).str();
}

std::string pretty_print(const ast::Assignment& asgn) {
  std::stringstream ss;
  pretty_print(ss, asgn);
  return std::move(ss).str();
}

} // namespace ooze
