#include "pch.h"

#include "ast.h"

namespace ooze {

using namespace ast;

namespace {

struct Printer {
  template <typename T>
  void pretty_print(std::ostream& os, const std::vector<T>& v) {
    if(v.empty()) {
      os << "()";
    } else {
      os << "(";
      pretty_print(os, v.front());

      std::for_each(v.begin() + 1, v.end(), [&](const auto& ele) { pretty_print(os << ", ", ele); });

      os << ")";
    }
  }

  template <typename... Ts>
  void pretty_print(std::ostream& os, const std::variant<Ts...>& v) {
    std::visit([&](const auto& v) { return pretty_print(os, v); }, v);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const Indirect<T>& i) {
    pretty_print(os, *i);
  }

  void pretty_print(std::ostream& os, WildCard) { os << "_"; }
  void pretty_print(std::ostream& os, Floating) { os << "_"; }

  void pretty_print(std::ostream& os, const Literal& l) { os << to_string(l); }
  void pretty_print(std::ostream& os, const Ident& i) { os << i.name; }
  void pretty_print(std::ostream& os, const IdentExpr& i) { os << i.name; }
  void pretty_print(std::ostream& os, const NamedType& t) { os << t.name; }

  void pretty_print(std::ostream& os, const Borrow<NamedType>& r) { pretty_print(os << '&', r.type); }

  void pretty_print(std::ostream& os, const FunctionType<NamedType>& r) {
    pretty_print(os, *r.input);
    pretty_print(os << " -> ", *r.output);
  }

  void pretty_print(std::ostream& os, const UnTypedAssignment& a) {
    pretty_print(os << "let ", a.pattern);
    if(!std::holds_alternative<Floating>(a.type.v)) {
      pretty_print(os << ": ", a.type);
    }
    pretty_print(os << " = ", a.expr);
  }

  void pretty_print(std::ostream& os, const ScopeExpr<NamedType, NamedFunction>& b) {
    os << " {\n";

    for(const auto& a : b.assignments) {
      pretty_print(os << "  ", a);
      os << "\n";
    }

    pretty_print(os << "  ", b.result);
    os << "\n}";
  }

  void pretty_print(std::ostream& os, const Pattern& p) { pretty_print(os, p.v); }
  void pretty_print(std::ostream& os, const CompoundType<NamedType>& t) { pretty_print(os, t.v); }
  void pretty_print(std::ostream& os, const Expr<NamedType, NamedFunction>& e) { pretty_print(os, e.v); }

  void pretty_print(std::ostream& os, const BorrowExpr<NamedType, NamedFunction>& b) {
    pretty_print(os << '&', *b.expr);
  }
  void pretty_print(std::ostream& os, const CallExpr<NamedType, NamedFunction>& c) {
    pretty_print(os << c.function.name, c.parameters);
  }

  void pretty_print(std::ostream& os, const UnTypedHeader& h) {
    pretty_print(os, h.pattern);
    pretty_print(os << ": ", h.type);
  }

  void pretty_print(std::ostream& os, const UnTypedFunction& f) {
    pretty_print(os, f.header);
    pretty_print(os, f.expr);
    os << "\n";
  }

  void pretty_print(std::ostream& os, const std::tuple<std::string, UnTypedFunction>& t) {
    const auto& [name, function] = t;
    pretty_print(os << "fn " << name, function);
  }

  template <typename T>
  std::string operator()(const T& t) {
    std::stringstream ss;
    pretty_print(ss, t);
    return std::move(ss).str();
  }
};

} // namespace

std::string pretty_print(const AST& ast) {
  std::stringstream ss;

  for(const auto& f : ast) {
    Printer{}.pretty_print(ss, f);
    ss << "\n";
  }

  return std::move(ss).str();
}

std::string pretty_print(const std::tuple<std::string, UnTypedFunction>& t) { return Printer{}(t); }
std::string pretty_print(const UnTypedFunction& f) { return Printer{}(f); }
std::string pretty_print(const UnTypedAssignment& asgn) { return Printer{}(asgn); }
std::string pretty_print(const UnTypedExpr& expr) { return Printer{}(expr); }
std::string pretty_print(const CompoundType<NamedType>& t) { return Printer{}(t); }
std::string pretty_print(const ast::Pattern& p) { return Printer{}(p); }

} // namespace ooze
