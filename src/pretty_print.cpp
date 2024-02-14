#include "pch.h"

#include "pretty_print.h"

namespace ooze {

namespace {

void print_literal(std::ostream& os, const Literal& l) {
  os << std::visit(
    Overloaded{
      [](bool b) { return std::string(b ? "true" : "false"); },
      [](const std::string& s) { return fmt::format("\"{}\"", s); },
      [](i8 i) { return fmt::format("'{}i8'", i); },
      [](i16 i) { return fmt::format("{}i16", i); },
      [](i32 i) { return fmt::format("{}", i); },
      [](i64 i) { return fmt::format("{}i64", i); },
      [](u8 u) { return fmt::format("'{}u8'", u); },
      [](u16 u) { return fmt::format("{}u16", u); },
      [](u32 u) { return fmt::format("{}u32", u); },
      [](u64 u) { return fmt::format("{}u64", u); },
      [](f32 f) { return fmt::format(f == std::floor(f) ? "{:.1f}f" : "{}f", f); },
      [](f64 f) { return fmt::format(f == std::floor(f) ? "{:.1f}" : "{}", f); }},
    l);
}

void pretty_print(std::ostream& os, const TypeGraph& g, const TypeNames& type_names, Type t) {
  const auto children = g.fanout(t);

  switch(g.get<TypeTag>(t)) {
  case TypeTag::Leaf: os << pretty_print(type_names, g.get<TypeID>(t)); return;
  case TypeTag::Floating: os << "_"; return;
  case TypeTag::Borrow:
    assert(children.size() == 1);
    pretty_print(os << "&", g, type_names, children.front());
    return;
  case TypeTag::Fn:
    assert(children.size() == 2);
    pretty_print(os << (g.get<TypeTag>(children[0]) == TypeTag::Tuple ? "fn" : "fn "), g, type_names, children[0]);
    pretty_print(os << " -> ", g, type_names, children[1]);
    return;
  case TypeTag::Tuple:
    os << '(';
    if(!children.empty()) {
      pretty_print(os, g, type_names, children.front());
      std::for_each(children.begin() + 1, children.end(), [&](Type ele) {
        pretty_print(os << ", ", g, type_names, ele);
      });
    }
    os << ')';
    return;
  }
}

void pretty_print(std::ostream& os,
                  Span<std::string_view> srcs,
                  const AST& ast,
                  const TypeNames& type_names,
                  ASTID id,
                  int indentation = 0) {
  const auto print_binding = [&](std::ostream& os, ASTID pattern_id) {
    pretty_print(os, srcs, ast, type_names, pattern_id, indentation);
    if(const Type t = ast.types[pattern_id.get()]; t.is_valid() && ast.tg.get<TypeTag>(t) != TypeTag::Floating) {
      pretty_print(os << ": ", ast.tg, type_names, t);
    }
  };

  switch(ast.forest[id]) {
  case ASTTag::PatternWildCard: os << "_"; return;
  case ASTTag::PatternIdent:
  case ASTTag::ExprIdent: os << sv(srcs, ast.srcs[id.get()]); return;
  case ASTTag::ExprLiteral: print_literal(os, lookup_literal(ast, id)); return;
  case ASTTag::ExprCall: {
    const auto [callee, arg] = ast.forest.child_ids(id).take<2>();
    pretty_print(os, srcs, ast, type_names, callee, indentation);
    pretty_print(os, srcs, ast, type_names, arg, indentation);
    return;
  }
  case ASTTag::ExprBorrow:
    pretty_print(os << "&", srcs, ast, type_names, *ast.forest.first_child(id), indentation);
    return;
  case ASTTag::ExprSelect: {
    const auto [cond, if_expr, else_expr] = ast.forest.child_ids(id).take<3>();
    pretty_print(os << "select ", srcs, ast, type_names, cond, indentation);
    pretty_print(os << " { ", srcs, ast, type_names, if_expr, indentation);
    pretty_print(os << " } else { ", srcs, ast, type_names, else_expr, indentation);
    os << " }";
    return;
  }
  case ASTTag::ExprIf: {
    const auto [cond, if_expr, else_expr] = ast.forest.child_ids(id).take<3>();
    pretty_print(os << "if ", srcs, ast, type_names, cond, indentation);
    pretty_print(os << " { ", srcs, ast, type_names, if_expr, indentation);
    pretty_print(os << " } else { ", srcs, ast, type_names, else_expr, indentation);
    os << " }";
    return;
  }
  case ASTTag::Assignment: {
    const auto [pattern, expr] = ast.forest.child_ids(id).take<2>();
    if(ast.forest.is_root(id) && ast.forest[expr] == ASTTag::Fn) {
      pretty_print(os << "fn ", srcs, ast, type_names, pattern, indentation);
      pretty_print(os, srcs, ast, type_names, expr, indentation);
    } else {
      print_binding(os << "let ", pattern);
      pretty_print(os << " = ", srcs, ast, type_names, expr, indentation);
    }
    return;
  }
  case ASTTag::ExprWith:
    os << "{\n";
    while(ast.forest[id] == ASTTag::ExprWith) {
      const auto [assignment, expr] = ast.forest.child_ids(id).take<2>();
      for(int i = 0; i < indentation + 1; i++) os << "  ";
      pretty_print(os, srcs, ast, type_names, assignment, indentation + 1);
      os << ";\n";
      id = expr;
    }
    for(int i = 0; i < indentation + 1; i++) os << "  ";
    pretty_print(os, srcs, ast, type_names, id, indentation + 1);
    os << "\n";
    for(int i = 0; i < indentation; i++) os << "  ";
    os << "}";
    return;
  case ASTTag::Fn: {
    const auto [pattern, expr] = ast.forest.child_ids(id).take<2>();
    os << '(';
    if(!ast.forest.is_leaf(pattern)) {
      const auto [pattern_first, pattern_rest] = ast.forest.child_ids(pattern).match();
      print_binding(os, pattern_first);
      for(const ASTID c : pattern_rest) {
        print_binding(os << ", ", c);
      }
    }
    os << ')';

    if(const Type t = ast.types[expr.get()]; t.is_valid()) {
      pretty_print(os << " -> ", ast.tg, type_names, t);
    } else {
      os << " -> _";
    }

    if(ast.forest[expr] != ASTTag::ExprWith) {
      os << " =";
    }

    pretty_print(os << " ", srcs, ast, type_names, expr, indentation);

    return;
  }
  case ASTTag::PatternTuple:
  case ASTTag::ExprTuple:
    os << '(';
    if(!ast.forest.is_leaf(id)) {
      const auto [first, rest] = ast.forest.child_ids(id).match();
      pretty_print(os, srcs, ast, type_names, first, indentation);
      for(const ASTID c : rest) {
        pretty_print(os << ", ", srcs, ast, type_names, c, indentation);
      }
    }
    os << ')';
    return;
  case ASTTag::EnvValue: {
    os << "<env_value>";
    return;
  }

  case ASTTag::Module: {
    os << "mod " << sv(srcs, ast.srcs[id.get()]) << " {\n";

    for(const ASTID child : ast.forest.child_ids(id)) {
      for(int i = 0; i < indentation; i++) os << "  ";
      pretty_print(os, srcs, ast, type_names, child, indentation);
      os << "\n";
    }

    os << "\n";
    for(int i = 0; i < indentation; i++) os << "  ";
    os << "}";
    return;
  }
  }
}

} // namespace

std::string pretty_print(const TypeNames& names, TypeID tid) {
  const std::string_view name = find_name(names, tid);
  return name.empty() ? fmt::format("type 0x{:x}", tid.id) : std::string(name);
}

std::string
pretty_print(Span<std::string_view> srcs, const AST& ast, const TypeNames& type_names, std::optional<ASTID> id) {
  std::ostringstream os;
  if(id) {
    pretty_print(os, srcs, ast, type_names, *id);
  } else {
    for(const ASTID root : ast.forest.root_ids()) {
      pretty_print(os, srcs, ast, type_names, root);
      if(ast.forest.next_sibling(root)) {
        os << "\n\n";
      }
    }
  }
  return std::move(os).str();
}

std::string pretty_print(const TypeGraph& g, const TypeNames& type_names, Type t) {
  std::ostringstream os;
  pretty_print(os, g, type_names, t);
  return std::move(os).str();
}

std::string pretty_print_fn_type(const TypeGraph& g, const TypeNames& type_names, Type t) {
  assert(g.get<TypeTag>(t) == TypeTag::Fn);

  std::ostringstream os;

  const auto children = g.fanout(t);
  pretty_print(os, g, type_names, children[0]);
  pretty_print(os << " -> ", g, type_names, children[1]);
  return std::move(os).str();
}

} // namespace ooze
