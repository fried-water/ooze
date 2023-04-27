#include "pch.h"

#include "pretty_print.h"

namespace ooze {

using namespace ast;

namespace {

struct Printer {
  const Env* _e = nullptr;
  int _indentation = 0;

  template <typename T>
  void pretty_print(std::ostream& os, const std::vector<T>& v) {
    os << '(';
    if(!v.empty()) {
      pretty_print(os, v.front());
      std::for_each(v.begin() + 1, v.end(), [&](const auto& ele) { pretty_print(os << ", ", ele); });
    }
    os << ')';
  }

  template <typename... Ts>
  void pretty_print(std::ostream& os, const std::variant<Ts...>& v) {
    std::visit([&](const auto& v) { return pretty_print(os, v); }, v);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const Indirect<T>& i) {
    pretty_print(os, *i);
  }

  void pretty_print(std::ostream& os, WildCard) { os << '_'; }
  void pretty_print(std::ostream& os, Floating) { os << '_'; }

  void pretty_print(std::ostream& os, const Literal& l) { os << to_string(l); }
  void pretty_print(std::ostream& os, const Ident& i) { os << i.name; }
  void pretty_print(std::ostream& os, const IdentExpr& i) { os << i.name; }
  void pretty_print(std::ostream& os, const NamedType& t) { os << t.name; }
  void pretty_print(std::ostream& os, const NamedFunction& f) { os << f.name; }

  void pretty_print(std::ostream& os, const EnvFunctionRef& f) { os << f.name; }

  void pretty_print(std::ostream& os, TypeID t) {
    const auto it = _e->type_names.find(t);
    os << (it != _e->type_names.end() ? it->second : fmt::format("type 0x{:x}", t));
  }

  template <typename T>
  void pretty_print(std::ostream& os, const Borrow<T>& r) {
    pretty_print(os << '&', r.type);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const FunctionType<T>& r) {
    pretty_print(os, *r.input);
    pretty_print(os << " -> ", *r.output);
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const Assignment<T, F>& a) {
    pretty_print(os << "let ", a.pattern);
    if(!std::holds_alternative<Floating>(a.type.v)) {
      pretty_print(os << ": ", a.type);
    }
    pretty_print(os << " = ", a.expr);
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const ScopeExpr<T, F>& b) {
    _indentation++;
    os << "{\n";

    for(const auto& a : b.assignments) {
      for(int i = 0; i < _indentation; i++) os << "  ";
      pretty_print(os, a);
      os << ";\n";
    }

    for(int i = 0; i < _indentation; i++) os << "  ";
    pretty_print(os, b.result);
    os << "\n";

    _indentation--;
    for(int i = 0; i < _indentation; i++) os << "  ";
    os << "}";
  }

  void pretty_print(std::ostream& os, const Pattern& p) { pretty_print(os, p.v); }

  template <typename T>
  void pretty_print(std::ostream& os, const CompoundType<T>& t) {
    pretty_print(os, t.v);
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const Expr<T, F>& e) {
    pretty_print(os, e.v);
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const BorrowExpr<T, F>& b) {
    pretty_print(os << '&', *b.expr);
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const CallExpr<T, F>& c) {
    pretty_print(os, c.function);
    pretty_print(os, c.parameters);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const FunctionHeader<T>& h) {
    const auto* pattern_vec = std::get_if<std::vector<Pattern>>(&h.pattern.v);
    const auto* in_vec = std::get_if<std::vector<CompoundType<T>>>(&h.type.input->v);

    if(in_vec && pattern_vec && in_vec->size() == pattern_vec->size()) {
      os << '(';
      if(!in_vec->empty()) {
        pretty_print(os, pattern_vec->front());
        pretty_print(os << ": ", in_vec->front());

        for(int i = 1; i < in_vec->size(); i++) {
          pretty_print(os << ", ", (*pattern_vec)[i]);
          pretty_print(os << ": ", (*in_vec)[i]);
        }
      }
      pretty_print(os << ") -> ", *h.type.output);
    } else {
      pretty_print(os, h.pattern);
      pretty_print(os << ": ", h.type);
    }
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const Function<T, F>& f) {
    pretty_print(os, f.header);

    if(std::holds_alternative<ScopeExpr<T, F>>(f.expr.v)) {
      pretty_print(os << ' ', f.expr);
    } else {
      pretty_print(os << " = ", f.expr);
    }
  }

  template <typename T, typename F>
  void pretty_print(std::ostream& os, const std::tuple<std::string, Function<T, F>>& t) {
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

std::string pretty_print(const UnTypedAST& ast) {
  std::stringstream ss;

  if(!ast.empty()) {
    for(int i = 0; i < ast.size() - 1; i++) {
      Printer{}.pretty_print(ss, ast[i]);
      ss << "\n\n";
    }

    Printer{}.pretty_print(ss, ast.back());
  }

  return std::move(ss).str();
}

std::string pretty_print(const Env& e, TypeID t) { return Printer{&e}(t); }

std::string pretty_print(const ast::Pattern& p) { return Printer{}(p); }

std::string pretty_print(const CompoundType<NamedType>& t) { return Printer{}(t); }
std::string pretty_print(const Env& e, const CompoundType<TypeID>& t) { return Printer{&e}(t); }

std::string pretty_print(const UnTypedFunction& f) { return Printer{}(f); }
std::string pretty_print(const UnTypedHeader& h) { return Printer{}(h); }
std::string pretty_print(const UnTypedAssignment& asgn) { return Printer{}(asgn); }
std::string pretty_print(const UnTypedExpr& expr) { return Printer{}(expr); }

std::string pretty_print(const Env& e, const TypedFunction& f) { return Printer{&e}(f); }
std::string pretty_print(const Env& e, const TypedHeader& h) { return Printer{&e}(h); }
std::string pretty_print(const Env& e, const TypedAssignment& asgn) { return Printer{&e}(asgn); }
std::string pretty_print(const Env& e, const TypedExpr& expr) { return Printer{&e}(expr); }

std::string pretty_print(const Env& e, const CheckedFunction& f) { return Printer{&e}(f); }
std::string pretty_print(const Env& e, const CheckedAssignment& asgn) { return Printer{&e}(asgn); }
std::string pretty_print(const Env& e, const CheckedExpr& expr) { return Printer{&e}(expr); }

} // namespace ooze
