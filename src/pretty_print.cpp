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
  void pretty_print(std::ostream& os, FloatingType) { os << '_'; }

  void pretty_print(std::ostream& os, const Literal& l) {
    os << std::visit(
      Overloaded{
        [](bool b) { return std::string(b ? "true" : "false"); },
        [](const std::string& s) { return fmt::format("\"{}\"", s); },
        [](i8 i) { return fmt::format("'{}'", i); },
        [](i16 i) { return fmt::format("{}i16", i); },
        [](i32 i) { return fmt::format("{}i32", i); },
        [](i64 i) { return fmt::format("{}i64", i); },
        [](u8 u) { return fmt::format("'{}u8'", u); },
        [](u16 u) { return fmt::format("{}u16", u); },
        [](u32 u) { return fmt::format("{}u32", u); },
        [](u64 u) { return fmt::format("{}u64", u); },
        [](f32 f) { return fmt::format("{}f", f); },
        [](f64 f) { return fmt::format("{}", f); }},
      l);
  }

  void pretty_print(std::ostream& os, const Ident& i) { os << i.name; }
  void pretty_print(std::ostream& os, const NamedType& t) { os << t.name; }

  void pretty_print(std::ostream& os, const EnvFunctionRef& f) { os << f.name; }

  void pretty_print(std::ostream& os, TypeID t) {
    const auto it = _e->type_names.find(t);
    os << (it != _e->type_names.end() ? it->second : fmt::format("type 0x{:x}", t.id));
  }

  template <typename T>
  void pretty_print(std::ostream& os, const BorrowType<T>& r) {
    pretty_print(os << '&', r.type);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const FunctionType<T>& r) {
    pretty_print(os << "fn", *r.input);
    pretty_print(os << " -> ", *r.output);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const Assignment<T, Extras...>& a) {
    pretty_print(os << "let ", a.pattern);
    if(!std::holds_alternative<FloatingType>(a.pattern.type.v)) {
      pretty_print(os << ": ", a.pattern.type);
    }
    pretty_print(os << " = ", a.expr);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const ScopeExpr<T, Extras...>& b) {
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

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const SelectExpr<T, Extras...>& e) {
    pretty_print(os << "select ", e.condition);
    pretty_print(os << " ", e.if_expr);
    pretty_print(os << " else ", e.else_expr);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const Pattern<T>& p) {
    pretty_print(os, p.v);
  }

  template <typename T>
  void pretty_print(std::ostream& os, const Type<T>& t) {
    pretty_print(os, t.v);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const Expr<T, Extras...>& e) {
    pretty_print(os, e.v);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const BorrowExpr<T, Extras...>& b) {
    pretty_print(os << '&', *b.expr);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const CallExpr<T, Extras...>& c) {
    pretty_print(os, c.callee);
    pretty_print(os, c.arg);
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const Function<T, Extras...>& f) {
    const auto* pattern_vec = std::get_if<std::vector<Pattern<T>>>(&f.pattern.v);
    const auto* in_vec = std::get_if<std::vector<Type<T>>>(&f.pattern.type.v);

    if(in_vec && pattern_vec && in_vec->size() == pattern_vec->size()) {
      os << '(';
      if(!in_vec->empty()) {
        const auto print_element = [&](const Pattern<T>& p, const Type<T>& t) {
          pretty_print(os, p);
          pretty_print(os << ": ", std::holds_alternative<FloatingType>(t.v) ? p.type : t);
        };

        print_element(pattern_vec->front(), in_vec->front());

        for(int i = 1; i < in_vec->size(); i++) {
          print_element((*pattern_vec)[i], (*in_vec)[i]);
        }
      }
      os << ")";
    } else {
      pretty_print(os, f.pattern);
      if(!std::holds_alternative<FloatingType>(f.pattern.type.v)) {
        pretty_print(os << ": ", f.pattern.type);
      }
    }

    pretty_print(os << " -> ", f.expr.type);

    if(std::holds_alternative<ScopeExpr<T, Extras...>>(f.expr.v)) {
      pretty_print(os << ' ', f.expr);
    } else {
      pretty_print(os << " = ", f.expr);
    }
  }

  template <typename T, typename... Extras>
  void pretty_print(std::ostream& os, const std::tuple<std::string, Function<T, Extras...>>& t) {
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

void print_literal(std::ostream& os, const Literal& l) {
  os << std::visit(
    Overloaded{
      [](bool b) { return std::string(b ? "true" : "false"); },
      [](const std::string& s) { return fmt::format("\"{}\"", s); },
      [](i8 i) { return fmt::format("'{}'", i); },
      [](i16 i) { return fmt::format("{}i16", i); },
      [](i32 i) { return fmt::format("{}i32", i); },
      [](i64 i) { return fmt::format("{}i64", i); },
      [](u8 u) { return fmt::format("'{}u8'", u); },
      [](u16 u) { return fmt::format("{}u16", u); },
      [](u32 u) { return fmt::format("{}u32", u); },
      [](u64 u) { return fmt::format("{}u64", u); },
      [](f32 f) { return fmt::format("{}f", f); },
      [](f64 f) { return fmt::format("{}", f); }},
    l);
}

void pretty_print(std::ostream& os, const Env& e, const Graph<TypeRef, TypeTag, TypeID>& g, TypeRef t) {
  const auto children = g.fanout(t);

  switch(g.get<TypeTag>(t)) {
  case TypeTag::Leaf: {
    const auto it = e.type_names.find(g.get<TypeID>(t));
    os << (it != e.type_names.end() ? it->second : fmt::format("type 0x{:x}", g.get<TypeID>(t).id));
    return;
  }
  case TypeTag::Floating: os << "_"; return;
  case TypeTag::Borrow:
    assert(children.size() == 1);
    pretty_print(os << "&", e, g, children.front());
    return;
  case TypeTag::Fn:
    assert(children.size() == 2);
    pretty_print(os << "fn", e, g, children[0]);
    pretty_print(os << " -> ", e, g, children[1]);
    return;
  case TypeTag::Tuple:
    os << '(';
    if(!children.empty()) {
      pretty_print(os, e, g, children.front());
      std::for_each(children.begin() + 1, children.end(), [&](TypeRef ele) { pretty_print(os << ", ", e, g, ele); });
    }
    os << ')';
    return;
  }
}

void pretty_print(std::ostream& os,
                  std::string_view src,
                  const Env& e,
                  const AST& ast,
                  const Types& types,
                  ASTID id,
                  int indentation = 0) {
  const auto print_binding = [&](std::ostream& os, ASTID pattern_id) {
    pretty_print(os, src, e, ast, types, pattern_id, indentation);
    if(TypeRef t = types.ast_types[pattern_id.get()];
       t.is_valid() && types.graph.get<TypeTag>(t) != TypeTag::Floating) {
      pretty_print(os << ": ", e, types.graph, t);
    }
  };

  const auto children = ast.forest.child_ids(id);
  auto it = children.begin();

  switch(ast.forest[id]) {
  case ASTTag::PatternWildCard: os << "_"; return;
  case ASTTag::PatternIdent:
  case ASTTag::ExprIdent: os << sv(src, ast.srcs[id.get()]); return;
  case ASTTag::ExprLiteral: print_literal(os, lookup_literal(ast, id)); return;
  case ASTTag::ExprCall:
    pretty_print(os, src, e, ast, types, *it++, indentation);
    pretty_print(os, src, e, ast, types, *it, indentation);
    return;
  case ASTTag::ExprBorrow: pretty_print(os << "&", src, e, ast, types, *it, indentation); return;
  case ASTTag::ExprSelect:
    pretty_print(os << "select ", src, e, ast, types, *it++, indentation);
    pretty_print(os << " ", src, e, ast, types, *it++, indentation);
    pretty_print(os << " else ", src, e, ast, types, *it, indentation);
    return;
  case ASTTag::Assignment:
    print_binding(os << "let ", *it++);
    pretty_print(os << " = ", src, e, ast, types, *it, indentation);
    return;
  case ASTTag::ExprWith:
    os << "{\n";
    while(ast.forest[id] == ASTTag::ExprWith) {
      for(int i = 0; i < indentation + 1; i++) os << "  ";
      pretty_print(os, src, e, ast, types, *it++, indentation + 1);
      os << ";\n";
      id = *it;
      it = ast.forest.child_ids(id).begin();
    }
    for(int i = 0; i < indentation + 1; i++) os << "  ";
    pretty_print(os, src, e, ast, types, id, indentation + 1);
    os << "\n";
    for(int i = 0; i < indentation; i++) os << "  ";
    os << "}";
    return;
  case ASTTag::Fn: {
    const ASTID pattern = *it++;
    const ASTID expr = *it;

    const auto pattern_children = ast.forest.child_ids(pattern);
    auto pattern_it = pattern_children.begin();

    os << '(';
    if(pattern_it != pattern_children.end()) {
      print_binding(os, *pattern_it);
      std::for_each(++pattern_it, children.end(), [&](ASTID c) { print_binding(os, c); });
    }
    os << ')';

    if(const TypeRef t = types.ast_types[expr.get()]; t.is_valid()) {
      pretty_print(os << " -> ", e, types.graph, t);
    } else {
      os << " -> _";
    }

    if(ast.forest[expr] != ASTTag::ExprWith) {
      os << " =";
    }

    pretty_print(os << " ", src, e, ast, types, expr, indentation);

    return;
  }
  case ASTTag::RootFn:
    pretty_print(os << "fn ", src, e, ast, types, *it++, indentation);
    pretty_print(os, src, e, ast, types, *it, indentation);
    return;
  case ASTTag::PatternTuple:
  case ASTTag::ExprTuple:
    os << '(';
    if(it != children.end()) {
      pretty_print(os, src, e, ast, types, *it, indentation);
      std::for_each(++it, children.end(), [&](ASTID c) {
        pretty_print(os << ", ", src, e, ast, types, c, indentation);
      });
    }
    os << ')';
    return;
  }
}

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

std::string pretty_print(const UnTypedPattern& p) { return Printer{}(p); }
std::string pretty_print(const TypedPattern& p) { return Printer{}(p); }

std::string pretty_print(const Type<NamedType>& t) { return Printer{}(t); }
std::string pretty_print(const Env& e, const Type<TypeID>& t) { return Printer{&e}(t); }
std::string pretty_print(const Env& e, const FunctionType<TypeID>& t) { return Printer{&e}(t); }

std::string pretty_print(const UnTypedFunction& f) { return Printer{}(f); }
std::string pretty_print(const UnTypedAssignment& asgn) { return Printer{}(asgn); }
std::string pretty_print(const UnTypedExpr& expr) { return Printer{}(expr); }

std::string pretty_print(const Env& e, const TypedFunction& f) { return Printer{&e}(f); }
std::string pretty_print(const Env& e, const TypedAssignment& asgn) { return Printer{&e}(asgn); }
std::string pretty_print(const Env& e, const TypedExpr& expr) { return Printer{&e}(expr); }

std::string pretty_print(const Env& e, const CheckedFunction& f) { return Printer{&e}(f); }
std::string pretty_print(const Env& e, const CheckedAssignment& asgn) { return Printer{&e}(asgn); }
std::string pretty_print(const Env& e, const CheckedExpr& expr) { return Printer{&e}(expr); }

std::string
pretty_print(std::string_view src, const Env& e, const AST& ast, const Types& types, std::optional<ASTID> id) {
  std::ostringstream os;
  if(id) {
    pretty_print(os, src, e, ast, types, *id);
  } else {
    for(ASTID root : ast.forest.root_ids()) {
      pretty_print(os, src, e, ast, types, root);
      if(ast.forest.next_sibling(root)) {
        os << "\n\n";
      }
    }
  }
  return std::move(os).str();
}

std::string pretty_print(const Env& e, const Graph<TypeRef, TypeTag, TypeID>& g, TypeRef t) {
  std::ostringstream os;
  pretty_print(os, e, g, t);
  return std::move(os).str();
}

} // namespace ooze
