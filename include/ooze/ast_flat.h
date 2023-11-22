#pragma once

#include "ooze/forest.h"
#include "ooze/graph.h"
#include "ooze/primitives.h"
#include "ooze/type.h"
#include "ooze/type_flat.h"

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

  Assignment,
  Fn,
  RootFn
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
     "Assignment",
     "Fn",
     "RootFn"});
}

using ASTID = StrongID<struct ASTSpace>;

struct AST {
  Forest<ASTTag, ASTID> forest;
  std::vector<Slice> srcs;
  std::vector<std::pair<ASTID, Literal>> literals;

  KNOT_COMPAREABLE(AST);
};

template <typename T>
struct TypesGeneric {
  Graph<TypeRef, TypeTag, T> graph;
  std::vector<TypeRef> ast_types;

  KNOT_COMPAREABLE(TypesGeneric);
};

using UnresolvedTypes = TypesGeneric<Slice>;
using Types = TypesGeneric<TypeID>;

inline const Literal& lookup_literal(const AST& ast, ASTID id) {
  return std::lower_bound(
           ast.literals.begin(), ast.literals.end(), id, [](const auto& p, ASTID id) { return p.first < id; })
    ->second;
}

} // namespace ooze
