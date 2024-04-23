#pragma once

#include "forest.h"
#include "src_map.h"

#include "ooze/primitives.h"
#include "ooze/type.h"

#include <knot/core.h>

#include <algorithm>
#include <string_view>

namespace ooze {

using Literal = std::variant<bool, std::string, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64>;

enum class ASTTag {
  PatternWildCard,
  PatternIdent,
  PatternTuple,
  // PatternBorrow, // TODO

  ExprLiteral,
  ExprCall,
  ExprIf,
  ExprBorrow,
  ExprWith,
  ExprTuple,
  ExprIdent,
  ExprQualified,
  Fn,
  EnvValue,

  Assignment,
  ModuleRef,
  Module
};

constexpr auto names(knot::Type<ASTTag>) {
  return knot::Names(
    "AST",
    {"PatternWildCard",
     "PatternIdent",
     "PatternTuple",
     "ExprLiteral",
     "ExprCall",
     "ExprIf",
     "ExprBorrow",
     "ExprWith",
     "ExprTuple",
     "ExprIdent",
     "ExprQualified",
     "Fn",
     "EnvValue",
     "Assignment",
     "ModuleRef",
     "Module"});
}

using ASTID = StrongID<struct ASTSpace>;

struct AST {
  Forest<ASTTag, ASTID> forest;
  std::vector<SrcRef> srcs;
  std::vector<Type> types;
  TypeGraph tg;
  std::vector<std::pair<ASTID, Literal>> literals;

  friend bool operator==(const AST&, const AST&) = default;
};

inline const Literal& lookup_literal(const AST& ast, ASTID id) {
  return std::lower_bound(
           ast.literals.begin(), ast.literals.end(), id, [](const auto& p, ASTID id) { return p.first < id; })
    ->second;
}

inline bool is_expr(ASTTag tag) {
  switch(tag) {
  case ASTTag::ExprLiteral:
  case ASTTag::ExprCall:
  case ASTTag::ExprBorrow:
  case ASTTag::ExprWith:
  case ASTTag::ExprTuple:
  case ASTTag::ExprIdent:
  case ASTTag::Fn:
  case ASTTag::EnvValue: return true;
  default: return false;
  }
}

inline bool is_pattern(ASTTag tag) {
  switch(tag) {
  case ASTTag::PatternWildCard:
  case ASTTag::PatternTuple:
  case ASTTag::PatternIdent: return true;
  default: return false;
  }
}

inline std::optional<ASTID> owning_module(const Forest<ASTTag, ASTID>& f, ASTID id) {
  if(const auto parent = f.parent(id); parent) {
    return f[*parent] == ASTTag::Module ? parent : std::nullopt;
  } else {
    // Root module
    return f.ABOVE_ROOTS;
  }
}

inline bool is_global_pattern(const Forest<ASTTag, ASTID>& f, ASTID id) {
  assert(f[id] == ASTTag::PatternIdent);
  while(!f.is_root(id) && is_pattern(f[id])) {
    id = *f.parent(id);
  }
  return f[id] == ASTTag::Assignment && owning_module(f, id);
}

inline Type global_type(const AST& ast, ASTID root) {
  return ast.types[ast.forest[root] == ASTTag::Assignment ? ast.forest.first_child(root)->get() : root.get()];
}

inline ASTID append_root(AST& ast, ASTTag tag, SrcRef ref, Type type, Span<ASTID> children = {}) {
  ast.srcs.push_back(ref);
  ast.types.push_back(type);
  return ast.forest.append_root_post_order(tag, children);
}

} // namespace ooze
