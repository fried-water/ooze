#include "pch.h"

#include "ast.h"

namespace ooze {

using namespace ast;

namespace {

template <typename T>
void pretty_print(std::ostream&, const std::vector<T>&, bool allow_short = false);

void pretty_print(std::ostream& os, const NamedType& t) { os << t.name; }

void pretty_print(std::ostream& os, const Parameter<NamedType>& t) {
  os << t.name << ": ";

  if(t.borrow) {
    os << '&';
  }
  os << t.type.name;
}

void pretty_print(std::ostream& os, const Binding<NamedType>& b) {
  os << b.name;

  if(b.type) {
    os << ": " << b.type->name;
  }
}

void pretty_print(std::ostream&, const Expr<NamedFunction>&);

void pretty_print(std::ostream& os, const Call<NamedFunction>& c) { pretty_print(os << c.function.name, c.parameters); }

struct ExprPrettyPrinter {
  std::ostream& os;

  void operator()(const std::vector<Expr<NamedFunction>>& exprs) const { pretty_print(os, exprs, true); }

  void operator()(const Call<NamedFunction>& call) const { pretty_print(os, call); }
  void operator()(const std::string& ident) const { os << ident; }
  void operator()(const Literal& l) const { os << to_string(l); }
};

void pretty_print(std::ostream& os, const Expr<NamedFunction>& e) { std::visit(ExprPrettyPrinter{os}, e.v); }

void pretty_print(std::ostream& os, const UnTypedAssignment& a) {
  pretty_print(os << "let ", a.bindings, true);
  pretty_print(os << " = ", a.expr);
}

void pretty_print(std::ostream& os, const UnTypedHeader& h) {
  pretty_print(os, h.parameters);
  pretty_print(os << " -> ", h.result, true);
}

void pretty_print(std::ostream& os, const UnTypedBody& b) {
  os << " {\n";

  for(const auto& a : b.assignments) {
    pretty_print(os << "  ", a);
    os << "\n";
  }

  pretty_print(os << "  ", b.result);
  os << "\n}";
}

void pretty_print(std::ostream& os, const UnTypedFunction& f) {
  pretty_print(os << "fn " << f.name, f.header);
  pretty_print(os, f.body);
  os << "\n";
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

template <typename T>
std::string pretty_print_gen(const T& t) {
  std::stringstream ss;
  pretty_print(ss, t);
  return std::move(ss).str();
}

} // namespace

std::string pretty_print(const AST& ast) {
  std::stringstream ss;

  for(const auto& f : ast) {
    pretty_print(ss, f);
    ss << "\n";
  }

  return std::move(ss).str();
}

std::string pretty_print(const Expr<NamedFunction>& expr) { return pretty_print_gen(expr); }
std::string pretty_print(const Call<NamedFunction>& c) { return pretty_print_gen(c); }
std::string pretty_print(const UnTypedFunction& f) { return pretty_print_gen(f); }
std::string pretty_print(const UnTypedAssignment& asgn) { return pretty_print_gen(asgn); }
std::string pretty_print(const Parameter<NamedType>& p) { return pretty_print_gen(p); }
std::string pretty_print(const Binding<NamedType>& b) { return pretty_print_gen(b); }

} // namespace ooze
