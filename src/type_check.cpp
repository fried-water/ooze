#include "pch.h"

#include "ooze/tree.h"
#include "pretty_print.h"
#include "type_check.h"

#include <deque>

namespace ooze {

using namespace ast;

namespace {

struct MismatchedType {
  TypeRef conflicting_type;
};

struct UnableToDeduce {};
struct ReturnBorrow {};
struct InvalidBorrow {
  std::string type;
};

struct UnusedBinding {};
struct OverusedBinding {
  int count = 0;
};

// Ordered by priority
using TypeCheckErrorVariant =
  std::variant<MismatchedType, UnableToDeduce, InvalidBorrow, ReturnBorrow, UnusedBinding, OverusedBinding>;

struct TypeCheckError {
  TypeCheckErrorVariant type;
  ASTID id;
};

bool has_floating(const TypeGraph& tg, TypeRef t) {
  return tg.get<TypeTag>(t) == TypeTag::Floating ||
         any_of(tg.fanout(t), [&](TypeRef child) { return has_floating(tg, child); });
}

TypeRef propagated_type(TypeGraph& g, Propagation p, bool wrap, TypeRef t, TypeRef floating) {
  const auto opt_fanout = [&](TypeRef t, int i) { return i < g.num_fanout(t) ? g.fanout(t)[i] : TypeRef{}; };

  return std::visit(
    Overloaded{[&](DirectProp) { return t; },
               [&](FloatingProp) { return floating; },
               [&](TupleProp p) {
                 if(wrap) {
                   const TypeRef type = g.add_node(TypeTag::Tuple, {});
                   for(int i = 0; i < p.size; i++) {
                     g.add_fanout_to_last_node(i == p.idx ? t : floating);
                   }
                   return type;
                 } else {
                   return opt_fanout(t, p.idx);
                 }
               },
               [&](BorrowProp) { return wrap ? g.add_node(std::array{t}, TypeTag::Borrow, {}) : opt_fanout(t, 0); },
               [&](FnInputProp) {
                 return wrap ? g.add_node(std::array{t, floating}, TypeTag::Fn, {}) : opt_fanout(t, 0);
               },
               [&](FnOutputProp) {
                 return wrap ? g.add_node(std::array{floating, t}, TypeTag::Fn, {}) : opt_fanout(t, 1);
               }},
    p);
}

std::vector<std::vector<TypeCheckError>>
cluster_adjacent(const std::vector<std::vector<ASTPropagation>>& propagations, std::vector<TypeCheckError> errors) {
  // Group clusterable errors by id
  Map<ASTID, std::vector<TypeCheckErrorVariant>> errors_by_id;
  for(auto& [error, id] : errors) {
    errors_by_id[id].push_back(std::move(error));
  }

  std::vector<std::vector<TypeCheckError>> groups;

  Set<ASTID> visited;
  std::vector<ASTID> to_visit;

  for(const auto& [id, _] : errors_by_id) {
    std::vector<TypeCheckError> group;
    to_visit.push_back(id);
    while(!to_visit.empty()) {
      ASTID id = to_visit.back();
      to_visit.pop_back();

      if(visited.insert(id).second) {
        if(const auto it = errors_by_id.find(id); it != errors_by_id.end()) {
          group = transform_to_vec(
            std::move(it->second),
            [&](auto error) {
              return TypeCheckError{std::move(error), id};
            },
            std::move(group));
        }

        for(const ASTPropagation& p : propagations[id.get()]) {
          if(errors_by_id.find(p.target) != errors_by_id.end()) {
            to_visit.push_back(p.target);
          }
        }
      }
    }

    if(!group.empty()) {
      groups.push_back(std::move(group));
    }
  }

  return groups;
}

ContextualError2 generate_error(Span<std::string_view> srcs,
                                const AST& ast,
                                const TypeGraph& tg,
                                const TypeNames& type_names,
                                const TypeCheckError& error) {
  const TypeRef type = ast.types[error.id.get()];
  const SrcRef ref = ast.srcs[error.id.get()];
  return std::visit(
    Overloaded{
      [&](const MismatchedType& m) {
        return ContextualError2{ref,
                                fmt::format("expected {}, given {}",
                                            pretty_print(srcs, tg, type_names, type),
                                            pretty_print(srcs, tg, type_names, m.conflicting_type))};
      },
      [&](const UnableToDeduce&) {
        return ContextualError2{
          ref, fmt::format("unable to fully deduce type, deduced: {}", pretty_print(srcs, tg, type_names, type))};
      },
      [&](const ReturnBorrow&) {
        return ContextualError2{ref, "cannot return a borrowed value"};
      },
      [&](const InvalidBorrow& b) {
        return ContextualError2{ref, fmt::format("cannot borrow a {}", b.type)};
      },
      [&](const UnusedBinding&) {
        return ContextualError2{
          ref, fmt::format("unused binding '{}'", sv(srcs, ref)), {"prefix with an _ to silence this error"}};
      },
      [&](const OverusedBinding& b) {
        return ContextualError2{ref, fmt::format("binding '{}' used {} times", sv(srcs, ref), b.count)};
      }},
    error.type);
}

std::vector<ContextualError2>
generate_errors(Span<std::string_view> srcs,
                const AST& ast,
                const TypeGraph& tg,
                const TypeNames& type_names,
                std::vector<std::vector<TypeCheckError>> error_clusters) {
  // Find most *relevant* error per cluster
  return sorted(
    transform_to_vec(error_clusters,
                     [&](const std::vector<TypeCheckError>& group) {
                       const auto projection = [&](const TypeCheckError& error) {
                         const auto type_complexity = [&](auto self, TypeRef t) -> i32 {
                           i32 count = 0;
                           preorder(tg, t, [&](TypeRef) {
                             count++;
                             return true;
                           });
                           return count;
                         };

                         const SrcRef ref = ast.srcs[error.id.get()];
                         const int complexity =
                           type_complexity(type_complexity, ast.types[error.id.get()]) +
                           std::visit(Overloaded{[&](const MismatchedType& m) {
                                                   return type_complexity(type_complexity, m.conflicting_type);
                                                 },
                                                 [](const auto&) { return 0; }},
                                      error.type);
                         return std::tuple(
                           error.type.index(),
                           ast.forest[error.id] == ASTTag::PatternWildCard,
                           complexity,
                           ref.file,
                           ref.slice.end,
                           size(ref.slice),
                           ref.slice.begin);
                       };

                       return generate_error(
                         srcs,
                         ast,
                         tg,
                         type_names,
                         *std::min_element(group.begin(), group.end(), [&](const auto& lhs, const auto& rhs) {
                           return projection(lhs) < projection(rhs);
                         }));
                     }),
    [](const auto& err) { return std::tie(err.ref.slice.end, err); });
}

std::tuple<TypeGraph, std::vector<TypeRef>, std::vector<TypeCheckError>> constraint_propagation(
  Span<std::string_view> srcs,
  const TypeCache& tc,
  const TypeNames& type_names,
  const std::vector<std::vector<ASTPropagation>>& propagations,
  const Forest<ASTTag, ASTID>& forest,
  const Graph<ASTID>& ident_graph,
  TypeGraph tg,
  std::vector<TypeRef> types,
  bool debug = false) {
  std::deque<std::pair<ASTID, TypeRef>> to_visit;

  for(ASTID id : forest.ids()) {
    if(debug) {
      fmt::print("Propagations {}: p {} ig {}\n",
                 id.get(),
                 knot::debug(propagations[id.get()]),
                 knot::debug(ident_graph.fanout(id)));
    }
    if(tg.get<TypeTag>(types[id.get()]) != TypeTag::Floating) {
      to_visit.emplace_back(id, types[id.get()]);
    }
  }

  std::fill(types.begin(), types.end(), tc.floating);
  std::vector<TypeRef> conflicting_types(forest.size(), TypeRef::Invalid());

  while(!to_visit.empty()) {
    const auto [id, type] = std::move(to_visit.front());
    to_visit.pop_front();

    const TypeRef original_type = types[id.get()];
    const bool was_conflicting = conflicting_types[id.get()].is_valid();

    if(debug) {
      fmt::print("Processing {} {}\n   propagated {}\n   existing   {}\n",
                 id.get(),
                 knot::debug(forest[id]),
                 pretty_print(srcs, tg, type_names, type),
                 pretty_print(srcs, tg, type_names, original_type));
    }

    // Apply propagated type
    if(const TypeRef unified = unify(tc, tg, original_type, type, true); unified.is_valid()) {
      types[id.get()] = unified;
    } else {
      conflicting_types[id.get()] = type;
    }

    // Attempt overload resolution
    if(forest[id] == ASTTag::ExprIdent && ident_graph.num_fanout(id) > 1) {
      if(auto [_, overload_type, matches] = overload_resolution(tc, tg, ident_graph, types, id); matches == 1) {
        types[id.get()] = overload_type;
      }
    }

    if(debug) {
      fmt::print("   final      {}\n", pretty_print(srcs, tg, type_names, types[id.get()]));
    }

    if(!compare_dags(tg, original_type, types[id.get()])) {
      for(const auto [dst, wrap, propagation] : propagations[id.get()]) {
        to_visit.emplace_back(dst, propagated_type(tg, propagation, wrap, types[id.get()], tc.floating));
      }
    }

    // First time a conflict happens propagate it, produces better error messages
    if(!was_conflicting && conflicting_types[id.get()].is_valid()) {
      for(const auto [dst, wrap, propagation] : propagations[id.get()]) {
        const TypeRef propagated = propagated_type(tg, propagation, wrap, conflicting_types[id.get()], tc.floating);
        if(propagated.is_valid()) {
          to_visit.emplace_back(dst, propagated);
        }
      }
    }
  }

  if(debug) {
    fmt::print("Final types\n");
    for(ASTID id : forest.ids()) {
      fmt::print("    {:<4} {:<25} = {}",
                 id.get(),
                 knot::debug(forest[id]),
                 pretty_print(srcs, tg, type_names, types[id.get()]));

      if(conflicting_types[id.get()].is_valid()) {
        fmt::print(" (conflicting {})", pretty_print(srcs, tg, type_names, conflicting_types[id.get()]));
      }

      fmt::print("\n");
    }
  }

  return {std::move(tg), std::move(types), transform_filter_to_vec(forest.ids(), [&](ASTID id) {
            const TypeRef t = conflicting_types[id.get()];
            return t.is_valid() ? std::optional(TypeCheckError{MismatchedType{t}, id}) : std::nullopt;
          })};
}

void find_returned_borrows(const AST& ast, const TypeGraph& tg, std::vector<TypeCheckError>& errors, ASTID id) {
  const auto has_borrow = [&](auto self, TypeRef t) -> bool {
    const TypeTag tag = tg.get<TypeTag>(t);
    return tag == TypeTag::Borrow ||
           (tag == TypeTag::Tuple && any_of(tg.fanout(t), [&](TypeRef c) { return self(self, c); }));
  };

  const TypeRef type = ast.types[id.get()];
  if(tg.get<TypeTag>(type) == TypeTag::Borrow) {
    errors.push_back({ReturnBorrow{}, id});
  } else {
    switch(ast.forest[id]) {
    case ASTTag::ExprCall:
    case ASTTag::ExprIdent:
      if(has_borrow(has_borrow, type)) {
        errors.push_back({ReturnBorrow{}, id});
      }
      break;
    case ASTTag::ExprTuple:
      for(ASTID id : ast.forest.child_ids(id)) {
        find_returned_borrows(ast, tg, errors, id);
      }
      break;
    case ASTTag::ExprSelect:
    case ASTTag::ExprWith: {
      // Skip first child for both With (assignment) and Select (condition)
      auto [_, rest] = ast.forest.child_ids(id).match();
      for(ASTID id : rest) {
        find_returned_borrows(ast, tg, errors, id);
      }

      break;
    }
    default: break;
    }
  }
}

std::vector<TypeCheckError> find_returned_borrows(const AST& ast, const TypeGraph& tg) {
  std::vector<TypeCheckError> errors;

  for(ASTID root_id : ast.forest.root_ids()) {
    const ASTID expr_id = is_expr(ast.forest[root_id]) ? root_id : ast.forest.child_ids(root_id).get<1>();

    find_returned_borrows(ast, tg, errors, expr_id);

    if(ast.forest[expr_id] == ASTTag::Fn) {
      find_returned_borrows(ast, tg, errors, ast.forest.child_ids(expr_id).get<1>());
    }
  }

  return errors;
}

std::vector<TypeCheckError> find_binding_usage_errors(
  Span<std::string_view> srcs,
  const std::unordered_set<TypeID>& copy_types,
  const AST& ast,
  const TypeGraph& tg,
  const Graph<ASTID>& ident_graph) {
  const auto has_non_copy_type = [&](auto self, TypeRef t) -> bool {
    switch(tg.get<TypeTag>(t)) {
    case TypeTag::Leaf: return copy_types.find(tg.get<TypeID>(t)) == copy_types.end();
    case TypeTag::Tuple: return any_of(tg.fanout(t), [=](TypeRef t) { return self(self, t); });
    case TypeTag::Floating: return false;
    case TypeTag::Borrow: return false;
    case TypeTag::Fn: return false;
    }
  };

  const auto not_borrowed = [&](ASTID id) {
    const auto parent = ast.forest.parent(id);
    return !parent || ast.forest[*parent] != ASTTag::ExprBorrow;
  };

  return transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) -> std::optional<TypeCheckError> {
    const TypeRef t = ast.types[id.get()];
    if(ast.forest[id] != ASTTag::PatternIdent) {
      return std::nullopt;
    } else if(sv(srcs, ast.srcs[id.get()])[0] != '_' && ident_graph.num_fanout(id) == 0) {
      return is_global(ast.forest, id) ? std::nullopt : std::optional(TypeCheckError{UnusedBinding{}, id});
    } else if(count_if(ident_graph.fanout(id), not_borrowed) > 1 &&
              has_non_copy_type(has_non_copy_type, ast.types[id.get()])) {
      return TypeCheckError{OverusedBinding{ident_graph.num_fanout(id)}, id};
    } else {
      return std::nullopt;
    }
  });
}

TypeRef language_rule(const TypeCache& tc, const AST& ast, TypeGraph& g, ASTID id) {

  switch(ast.forest[id]) {
  case ASTTag::ExprLiteral: {
    const TypeID tid = std::visit(
      [](const auto& ele) { return type_id(knot::decay(knot::Type<decltype(ele)>{})); }, lookup_literal(ast, id));
    const auto it = tc.native.find(tid);
    return it != tc.native.end() ? it->second.first : g.add_node(TypeTag::Leaf, tid);
  }

  case ASTTag::ExprBorrow: return tc.borrow_floating;

  case ASTTag::PatternTuple:
  case ASTTag::ExprTuple: {
    const TypeRef tuple = g.add_node(TypeTag::Tuple, TypeID{});
    for(ASTID _ : ast.forest.child_ids(id)) {
      g.add_fanout_to_last_node(tc.floating);
    }
    return tuple;
  }

  case ASTTag::Fn: return tc.fn_floating;

  case ASTTag::Assignment: return tc.unit;

  case ASTTag::ExprCall:
  case ASTTag::ExprSelect:
  case ASTTag::PatternWildCard:
  case ASTTag::PatternIdent:
  case ASTTag::ExprWith:
  case ASTTag::EnvValue:
  case ASTTag::ExprIdent: return tc.floating;
  }

  return TypeRef::Invalid();
}

} // namespace

TypeRef unify(const TypeCache& tc, TypeGraph& g, TypeRef x, TypeRef y, bool recurse) {
  const TypeTag xt = g.get<TypeTag>(x);
  const TypeTag yt = g.get<TypeTag>(y);

  if(xt == TypeTag::Floating) {
    return y;
  } else if(yt == TypeTag::Floating) {
    return x;
  } else if(xt != yt) {
    return TypeRef::Invalid();
  } else if(xt == TypeTag::Leaf) {
    return g.get<TypeID>(x) == g.get<TypeID>(y) ? std::min(x, y) : TypeRef::Invalid();
  } else if(g.fanout(x).empty() && g.fanout(y).empty()) {
    return std::min(x, y);
  } else {
    const auto x_count = g.num_fanout(x);
    const auto y_count = g.num_fanout(y);

    i32 x_it = 0;
    i32 y_it = 0;

    std::vector<TypeRef> results;
    const TypeRef floating = recurse ? TypeRef::Invalid() : tc.floating;

    for(; x_it != x_count && y_it != y_count; ++y_it, ++x_it) {
      const TypeRef c = recurse ? unify(tc, g, g.fanout(x)[x_it], g.fanout(y)[y_it], true) : floating;
      if(c != TypeRef::Invalid()) {
        results.push_back(c);
      } else {
        return TypeRef::Invalid();
      }
    }

    if(x_it == x_count && y_it == y_count) {
      return g.add_node(results, xt, {});
    } else {
      return TypeRef::Invalid();
    }
  }
}

ContextualResult2<void, AST, TypeGraph> apply_language_rules(
  Span<std::string_view> srcs, const TypeCache& tc, const TypeNames& type_names, AST ast, TypeGraph tg) {
  std::vector<ContextualError2> errors;

  const auto apply_type = [&](ASTID id, TypeRef t) {
    const TypeRef unified = ast.types[id.get()].is_valid() ? unify(tc, tg, ast.types[id.get()], t, true) : t;
    if(unified == TypeRef::Invalid()) {
      errors.push_back({ast.srcs[id.get()],
                        fmt::format("expected {}, given {}",
                                    pretty_print(srcs, tg, type_names, t),
                                    pretty_print(srcs, tg, type_names, ast.types[id.get()]))});
    } else {
      ast.types[id.get()] = unified;
    }
  };

  for(const ASTID id : ast.forest.ids()) {
    apply_type(id, language_rule(tc, ast, tg, id));

    const auto parent = ast.forest.parent(id);
    if(parent && ast.forest[*parent] == ASTTag::ExprCall && ast.forest.first_child(*parent) == id) {
      apply_type(id, tc.fn_floating);
    } else if(parent && ast.forest[*parent] == ASTTag::ExprSelect && ast.forest.first_child(*parent) == id) {
      apply_type(id, tc.boolean);
    }
  }

  return void_or_errors(std::move(errors), std::move(ast), std::move(tg));
}

std::tuple<ASTID, TypeRef, int> overload_resolution(
  const TypeCache& tc, TypeGraph& tg, const Graph<ASTID>& ident_graph, const std::vector<TypeRef>& types, ASTID ident) {
  int num_matches = 0;
  ASTID overload_fn;
  TypeRef overload_type;

  for(ASTID overload : ident_graph.fanout(ident)) {
    if(auto type = unify(tc, tg, types[ident.get()], types[overload.get()], true); type.is_valid()) {
      overload_type = type;
      overload_fn = overload;
      num_matches++;
      if(num_matches > 1) {
        break;
      }
    }
  }

  return std::tuple(overload_fn, overload_type, num_matches);
}

std::vector<std::vector<ASTPropagation>>
calculate_propagations(const Graph<ASTID>& ident_graph, const Forest<ASTTag, ASTID>& forest) {
  std::vector<std::vector<ASTPropagation>> propagations(forest.size());

  const auto add_pair = [&](ASTID bottom, ASTID top, Propagation prop) {
    propagations[top.get()].push_back({bottom, true, prop});
    propagations[bottom.get()].push_back({top, false, prop});
  };

  for(ASTID id : forest.ids()) {
    switch(forest[id]) {
    case ASTTag::PatternTuple:
    case ASTTag::ExprTuple: {
      int i = 0;
      const int num_children = forest.num_children(id);
      for(ASTID child : forest.child_ids(id)) {
        add_pair(id, child, TupleProp{i, num_children});
        i++;
      }
      break;
    }
    case ASTTag::ExprBorrow: add_pair(id, *forest.first_child(id), BorrowProp{}); break;
    case ASTTag::ExprSelect: {
      const auto [cond, if_expr, else_expr] = forest.child_ids(id).take<3>();
      add_pair(if_expr, else_expr, DirectProp{});
      add_pair(if_expr, id, DirectProp{});
      add_pair(else_expr, id, DirectProp{});
      break;
    }
    case ASTTag::ExprCall: {
      const auto [callee, arg] = forest.child_ids(id).take<2>();
      add_pair(callee, arg, FnInputProp{});
      add_pair(callee, id, FnOutputProp{});
      break;
    }
    case ASTTag::ExprWith: {
      const auto [_, expr] = forest.child_ids(id).take<2>();
      add_pair(id, expr, DirectProp{});
      break;
    }
    case ASTTag::Assignment: {
      const auto [pattern, expr] = forest.child_ids(id).take<2>();
      add_pair(pattern, expr, DirectProp{});
      break;
    }
    case ASTTag::Fn: {
      const auto [pattern, expr] = forest.child_ids(id).take<2>();
      add_pair(id, pattern, FnInputProp{});
      add_pair(id, expr, FnOutputProp{});
      break;
    }
    case ASTTag::ExprIdent: {
      for(ASTID pattern : ident_graph.fanout(id)) {
        if(is_global(forest, pattern)) {
          // Only propogate one way
          propagations[pattern.get()].push_back(
            {id, true, ident_graph.num_fanout(id) == 1 ? Propagation{DirectProp{}} : Propagation{FloatingProp{}}});
        } else {
          add_pair(id, pattern, DirectProp{});
        }
      }
    }
    case ASTTag::EnvValue:
    case ASTTag::ExprLiteral:
    case ASTTag::PatternIdent:
    case ASTTag::PatternWildCard: break;
    }
  }

  return propagations;
}

std::vector<ContextualError2> check_fully_resolved(
  Span<std::string_view> srcs,
  const std::vector<std::vector<ASTPropagation>>& propagations,
  const AST& ast,
  const TypeGraph& tg,
  const TypeNames& type_names) {
  return generate_errors(
    srcs, ast, tg, type_names, cluster_adjacent(propagations, transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) {
                                                  return has_floating(tg, ast.types[id.get()])
                                                           ? std::optional(TypeCheckError{UnableToDeduce{}, id})
                                                           : std::nullopt;
                                                })));
}

ContextualResult2<void, AST, TypeGraph> constraint_propagation(
  Span<std::string_view> srcs,
  const TypeCache& tc,
  const NativeTypeInfo& native_types,
  const Graph<ASTID>& ig,
  const std::vector<std::vector<ASTPropagation>>& propagations,
  AST ast,
  TypeGraph tg,
  bool debug) {
  std::vector<TypeCheckError> cp_errors;
  std::tie(tg, ast.types, cp_errors) = constraint_propagation(
    srcs, tc, native_types.names, propagations, ast.forest, ig, std::move(tg), std::move(ast.types), debug);

  const auto find_invalid_borrows = [](const AST& ast, const TypeGraph& tg) {
    return transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) -> std::optional<TypeCheckError> {
      const TypeRef type = ast.types[id.get()];
      if(tg.get<TypeTag>(type) == TypeTag::Borrow) {
        switch(tg.get<TypeTag>(tg.fanout(type)[0])) {
        case TypeTag::Tuple: return std::optional(TypeCheckError{InvalidBorrow{"tuple"}, id});
        case TypeTag::Fn: return std::optional(TypeCheckError{InvalidBorrow{"function"}, id});
        case TypeTag::Borrow: return std::optional(TypeCheckError{InvalidBorrow{"borrow"}, id});
        case TypeTag::Leaf:
        case TypeTag::Floating: return std::nullopt;
        }
      }
      return std::nullopt;
    });
  };

  std::vector<ContextualError2> errors = generate_errors(
    srcs,
    ast,
    tg,
    native_types.names,
    cluster_adjacent(propagations,
                     flatten(std::move(cp_errors),
                             find_invalid_borrows(ast, tg),
                             find_returned_borrows(ast, tg),
                             find_binding_usage_errors(srcs, native_types.copyable, ast, tg, ig))));

  return void_or_errors(std::move(errors), std::move(ast), std::move(tg));
}

} // namespace ooze
