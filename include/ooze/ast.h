#pragma once

#include "ooze/forest.h"
#include "ooze/graph.h"
#include "ooze/primitives.h"
#include "ooze/src_map.h"
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
  ExprSelect,
  ExprBorrow,
  ExprWith,
  ExprTuple,
  ExprIdent,
  Fn,
  EnvValue,

  Assignment
};

constexpr auto names(knot::Type<ASTTag>) {
  return knot::Names(
    "AST",
    {"PatternWildCard",
     "PatternIdent",
     "PatternTuple",
     "ExprLiteral",
     "ExprCall",
     "ExprSelect",
     "ExprBorrow",
     "ExprWith",
     "ExprTuple",
     "ExprIdent",
     "Fn",
     "EnvValue",
     "Assignment"});
}

using ASTID = StrongID<struct ASTSpace>;

struct AST {
  Forest<ASTTag, ASTID> forest;
  std::vector<SrcRef> srcs;
  std::vector<Type> types;
  std::vector<std::pair<ASTID, Literal>> literals;

  KNOT_COMPAREABLE(AST);
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
  case ASTTag::ExprSelect:
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

inline bool is_global(const Forest<ASTTag, ASTID>& f, ASTID id) {
  if(f[id] == ASTTag::PatternIdent) {
    while(!f.is_root(id) && is_pattern(f[id])) {
      id = *f.parent(id);
    }
    return f.is_root(id) && f[id] == ASTTag::Assignment;
  } else {
    return false;
  }
}

inline ASTID append_root(AST& ast, ASTTag tag, SrcRef ref, Type type, Span<ASTID> children = {}) {
  ast.srcs.push_back(ref);
  ast.types.push_back(type);
  return ast.forest.append_root_post_order(tag, children);
}

inline ASTID add_global(AST& ast, SrcRef ref, Type type) {
  const ASTID ident_id = append_root(ast, ASTTag::PatternIdent, ref, type);
  const ASTID fn_id = append_root(ast, ASTTag::EnvValue, ref, type);
  append_root(ast, ASTTag::Assignment, SrcRef{}, Type{}, std::array{ident_id, fn_id});
  return ident_id;
}

} // namespace ooze
