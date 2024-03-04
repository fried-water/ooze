#include "pch.h"

#include "pretty_print.h"
#include "type_check.h"

#include <deque>

namespace ooze {

namespace {

struct NoOverload {};

struct AmbiguousOverload {
  std::vector<ASTID> matches;
};

struct MismatchedType {
  Type conflicting_type;
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
  std::variant<NoOverload,
               MismatchedType,
               AmbiguousOverload,
               InvalidBorrow,
               ReturnBorrow,
               UnusedBinding,
               OverusedBinding,
               UnableToDeduce>;

struct TypeCheckError {
  TypeCheckErrorVariant type;
  ASTID id;
};

struct DirectProp {};
struct FloatingProp {};
struct TupleProp {
  int idx;
  int size;
};
struct BorrowProp {};
struct FnInputProp {};
struct FnOutputProp {};

using Propagation = std::variant<DirectProp, TupleProp, BorrowProp, FnInputProp, FnOutputProp, FloatingProp>;

struct ASTPropagation {
  ASTID target = {};
  bool wrap = false;
  Propagation propagation = DirectProp{};
};

bool can_unify(const TypeGraph& g, Type x, Type y) {
  const TypeTag xt = g.get<TypeTag>(x);
  const TypeTag yt = g.get<TypeTag>(y);

  if(xt == TypeTag::Floating) {
    return true;
  } else if(yt == TypeTag::Floating) {
    return true;
  } else if(xt != yt) {
    return false;
  } else if(xt == TypeTag::Leaf) {
    return g.get<TypeID>(x) == g.get<TypeID>(y);
  } else if(g.num_fanout(x) == g.num_fanout(y)) {
    for(i32 i = 0; i < g.num_fanout(x); i++) {
      if(!can_unify(g, g.fanout(x)[i], g.fanout(y)[i])) {
        return false;
      }
    }
    return true;
  } else {
    return false;
  }
}

Type propagated_type(TypeGraph& g, Propagation p, bool wrap, Type t, Type floating) {
  const auto opt_fanout = [&](Type t, int i) { return i < g.num_fanout(t) ? g.fanout(t)[i] : Type{}; };

  return std::visit(
    Overloaded{[&](DirectProp) { return t; },
               [&](FloatingProp) { return floating; },
               [&](TupleProp p) {
                 if(wrap) {
                   const Type type = g.add_node(TypeTag::Tuple, {});
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

std::pair<OverloadResolutionData, std::vector<TypeCheckError>>
overload_resolution(const Graph<ASTID>& ident_graph, const AST& ast, Span<ASTID> roots) {
  Map<ASTID, ASTID> overloads;
  std::vector<std::tuple<ASTID, Type, std::vector<ASTID>>> instantiations;

  std::vector<TypeCheckError> errors;

  for(const ASTID root : roots) {
    assert(owning_module(ast.forest, root));
    for(const ASTID id : ast.forest.post_order_ids(root)) {
      if(!is_expr(ast.forest[id]) || ident_graph.num_fanout(id) == 0) continue;

      const ASTID root = ast.forest.root(id);
      if(ast.forest[root] == ASTTag::Assignment && !is_resolved(ast.tg, global_type(ast, root))) continue;

      std::vector<ASTID> matches = filter_to_vec(ident_graph.fanout(id), [&](ASTID overload) mutable {
        return can_unify(ast.tg, ast.types[id.get()], ast.types[overload.get()]);
      });

      if(matches.size() == 1 && is_resolved(ast.tg, ast.types[id.get()])) {
        const ASTID overload = matches[0];

        if(is_resolved(ast.tg, ast.types[overload.get()])) {
          overloads.emplace(id, overload);
        } else if(is_global_pattern(ast.forest, ident_graph.fanout(id)[0])) {
          const auto it = find_if(instantiations, applied([&](ASTID fn, Type fn_type, const auto&) {
                                    return fn == overload && compare_dags(ast.tg, fn_type, ast.types[id.get()]);
                                  }));

          if(it == instantiations.end()) {
            instantiations.emplace_back(overload, ast.types[id.get()], std::vector{id});
          } else {
            std::get<2>(*it).push_back(id);
          }
        }
      } else if(matches.size() != 1 && is_global_pattern(ast.forest, ident_graph.fanout(id)[0])) {
        errors.push_back(matches.size() == 0 ? TypeCheckError{NoOverload{}, id}
                                             : TypeCheckError{AmbiguousOverload{std::move(matches)}, id});
      }
    }
  }

  return std::pair(OverloadResolutionData{std::move(overloads), std::move(instantiations)}, std::move(errors));
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

ContextualError generate_error(Span<std::string_view> srcs,
                               const AST& ast,
                               const TypeNames& type_names,
                               const Graph<ASTID>& ident_graph,
                               const TypeCheckError& error) {
  const Type type = ast.types[error.id.get()];
  const SrcRef ref = ast.srcs[error.id.get()];
  return std::visit(
    Overloaded{
      [&](const NoOverload&) {
        return ContextualError{
          ref,
          "no matching overload found",
          transform_to_vec(
            ident_graph.fanout(error.id),
            [&](ASTID id) { return fmt::format("  {}", pretty_print(ast.tg, type_names, ast.types[id.get()])); },
            make_vector(fmt::format("deduced {} [{} candidate(s)]",
                                    pretty_print(ast.tg, type_names, type),
                                    ident_graph.fanout(error.id).size())))};
      },
      [&](const AmbiguousOverload& a) {
        return ContextualError{
          ref,
          "ambiguous overload",
          transform_to_vec(
            a.matches,
            [&](ASTID id) { return fmt::format("  {}", pretty_print(ast.tg, type_names, ast.types[id.get()])); },
            make_vector(
              fmt::format("deduced {} [{} candidate(s)]", pretty_print(ast.tg, type_names, type), a.matches.size())))};
      },
      [&](const MismatchedType& m) {
        return ContextualError{ref,
                               fmt::format("expected {}, given {}",
                                           pretty_print(ast.tg, type_names, type),
                                           pretty_print(ast.tg, type_names, m.conflicting_type))};
      },
      [&](const UnableToDeduce&) {
        return ContextualError{
          ref, fmt::format("unable to fully deduce type, deduced: {}", pretty_print(ast.tg, type_names, type))};
      },
      [&](const ReturnBorrow&) {
        return ContextualError{ref, "cannot return a borrowed value"};
      },
      [&](const InvalidBorrow& b) {
        return ContextualError{ref, fmt::format("cannot borrow a {}", b.type)};
      },
      [&](const UnusedBinding&) {
        return ContextualError{
          ref, fmt::format("unused binding '{}'", sv(srcs, ref)), {"prefix with an _ to silence this error"}};
      },
      [&](const OverusedBinding& b) {
        return ContextualError{ref, fmt::format("binding '{}' used more than once", sv(srcs, ref), b.count)};
      }},
    error.type);
}

std::vector<ContextualError>
generate_errors(Span<std::string_view> srcs,
                const AST& ast,
                const TypeNames& type_names,
                const Graph<ASTID>& ident_graph,
                std::vector<std::vector<TypeCheckError>> error_clusters) {
  // Find most *relevant* error per cluster
  return sorted(
    transform_to_vec(
      error_clusters,
      [&](const std::vector<TypeCheckError>& group) {
        const auto projection = [&](const TypeCheckError& error) {
          const auto type_complexity = [&](auto self, Type t) -> i32 {
            return knot::accumulate(ast.tg.fanout(t), 1, [&](i32 acc, Type t) { return acc + self(self, t); });
          };

          const SrcRef ref = ast.srcs[error.id.get()];
          const int complexity =
            type_complexity(type_complexity, ast.types[error.id.get()]) +
            std::visit(
              Overloaded{[&](const MismatchedType& m) { return type_complexity(type_complexity, m.conflicting_type); },
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
          type_names,
          ident_graph,
          *std::min_element(group.begin(), group.end(), [&](const auto& lhs, const auto& rhs) {
            return projection(lhs) < projection(rhs);
          }));
      }),
    [](const auto& err) { return std::tie(err.ref.slice.end, err); });
}

std::tuple<TypeGraph, std::vector<Type>, std::vector<TypeCheckError>> constraint_propagation(
  const TypeCache& tc,
  const TypeNames& type_names,
  const std::vector<std::vector<ASTPropagation>>& propagations,
  const Forest<ASTTag, ASTID>& forest,
  const Graph<ASTID>& ident_graph,
  TypeGraph tg,
  std::vector<Type> types,
  Span<ASTID> roots,
  bool debug = false) {
  std::deque<std::pair<ASTID, Type>> to_visit;

  for(const ASTID root : roots) {
    for(const ASTID id : forest.post_order_ids(root)) {
      if(debug) {
        fmt::print("Propagations {}: p {} ig {}\n",
                   id.get(),
                   knot::debug(propagations[id.get()]),
                   knot::debug(ident_graph.fanout(id)));
      }
      to_visit.emplace_back(id, types[id.get()]);
      types[id.get()] = tc.floating;
    }
  }

  std::vector<Type> conflicting_types(forest.size(), Type::Invalid());

  while(!to_visit.empty()) {
    const auto [id, type] = std::move(to_visit.front());
    to_visit.pop_front();

    const Type original_type = types[id.get()];
    const bool was_conflicting = conflicting_types[id.get()].is_valid();

    if(debug) {
      fmt::print("Processing {} {}\n   propagated {}\n   existing   {}\n",
                 id.get(),
                 knot::debug(forest[id]),
                 pretty_print(tg, type_names, type),
                 pretty_print(tg, type_names, original_type));
    }

    // Apply propagated type
    if(const Type unified = unify(tc, tg, original_type, type, true); unified.is_valid()) {
      types[id.get()] = unified;
    } else {
      conflicting_types[id.get()] = type;
    }

    // Attempt overload resolution
    if(forest[id] == ASTTag::ExprIdent) {
      int num_matches = 0;
      Type overload_type = Type::Invalid();

      for(const ASTID overload : ident_graph.fanout(id)) {
        if(auto type = unify(tc, tg, types[id.get()], types[overload.get()], true); type.is_valid()) {
          if(overload_type.is_valid()) {
            overload_type = Type::Invalid();
            break;
          }
          overload_type = type;
        }
      }

      if(overload_type.is_valid()) {
        types[id.get()] = overload_type;
      }
    }

    if(debug) {
      fmt::print("   final      {}\n", pretty_print(tg, type_names, types[id.get()]));
    }

    if(!compare_dags(tg, original_type, types[id.get()])) {
      for(const auto [dst, wrap, propagation] : propagations[id.get()]) {
        to_visit.emplace_back(dst, propagated_type(tg, propagation, wrap, types[id.get()], tc.floating));
      }
    }

    // First time a conflict happens propagate it, produces better error messages
    if(!was_conflicting && conflicting_types[id.get()].is_valid()) {
      for(const auto [dst, wrap, propagation] : propagations[id.get()]) {
        const Type propagated = propagated_type(tg, propagation, wrap, conflicting_types[id.get()], tc.floating);
        if(propagated.is_valid()) {
          to_visit.emplace_back(dst, propagated);
        }
      }
    }
  }

  if(debug) {
    fmt::print("Final types\n");
    for(const ASTID root : roots) {
      for(const ASTID id : forest.post_order_ids(root)) {
        fmt::print(
          "    {:<4} {:<25} = {}", id.get(), knot::debug(forest[id]), pretty_print(tg, type_names, types[id.get()]));

        if(conflicting_types[id.get()].is_valid()) {
          fmt::print(" (conflicting {})", pretty_print(tg, type_names, conflicting_types[id.get()]));
        }

        fmt::print("\n");
      }
    }
  }

  std::vector<TypeCheckError> mismatched_errors;
  for(const ASTID root : roots) {
    mismatched_errors = transform_filter_to_vec(
      forest.post_order_ids(root),
      [&](ASTID id) {
        const Type t = conflicting_types[id.get()];
        return t.is_valid() ? std::optional(TypeCheckError{MismatchedType{t}, id}) : std::nullopt;
      },
      std::move(mismatched_errors));
  }

  return {std::move(tg), std::move(types), std::move(mismatched_errors)};
}

void find_returned_borrows(const AST& ast, std::vector<TypeCheckError>& errors, ASTID id) {
  const auto has_borrow = [&](auto self, Type t) -> bool {
    const TypeTag tag = ast.tg.get<TypeTag>(t);
    return tag == TypeTag::Borrow ||
           (tag == TypeTag::Tuple && any_of(ast.tg.fanout(t), [&](Type c) { return self(self, c); }));
  };

  const Type type = ast.types[id.get()];
  if(ast.tg.get<TypeTag>(type) == TypeTag::Borrow) {
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
      for(const ASTID id : ast.forest.child_ids(id)) {
        find_returned_borrows(ast, errors, id);
      }
      break;
    case ASTTag::ExprSelect:
    case ASTTag::ExprIf:
    case ASTTag::ExprWith: {
      // Skip first child for both With (assignment) and Select/If (condition)
      auto [_, rest] = ast.forest.child_ids(id).match();
      for(const ASTID id : rest) {
        find_returned_borrows(ast, errors, id);
      }

      break;
    }
    case ASTTag::Fn: find_returned_borrows(ast, errors, ast.forest.child_ids(id).get<1>()); break;
    default: break;
    }
  }
}

std::vector<TypeCheckError> find_unresolved_errors(const AST& ast, Span<ASTID> roots) {
  std::vector<TypeCheckError> errors;

  for(const ASTID root : roots) {
    if(ast.forest[root] == ASTTag::Assignment) {
      const auto [pattern, expr] = ast.forest.child_ids(root).take<2>();
      if(is_resolved(ast.tg, ast.types[pattern.get()])) {
        errors = transform_filter_to_vec(
          ast.forest.post_order_ids(expr),
          [&](ASTID id) {
            return !is_resolved(ast.tg, ast.types[id.get()])
                     ? std::optional(TypeCheckError{UnableToDeduce{}, id})
                     : std::nullopt;
          },
          std::move(errors));
      }
    } else if(is_expr(ast.forest[root])) {
      errors = transform_filter_to_vec(
        ast.forest.post_order_ids(root),
        [&](ASTID id) {
          return !is_resolved(ast.tg, ast.types[id.get()])
                   ? std::optional(TypeCheckError{UnableToDeduce{}, id})
                   : std::nullopt;
        },
        std::move(errors));
    }
  }

  return errors;
}

template <typename Roots>
std::vector<TypeCheckError> find_returned_borrows(const AST& ast, Roots roots) {
  std::vector<TypeCheckError> errors;

  for(const ASTID root_id : roots) {
    const ASTTag tag = ast.forest[root_id];
    if(tag == ASTTag::Module) {
      errors = to_vec(find_returned_borrows(ast, ast.forest.child_ids(root_id)), std::move(errors));
    } else if(tag == ASTTag::Assignment) {
      find_returned_borrows(ast, errors, ast.forest.child_ids(root_id).get<1>());
    } else {
      find_returned_borrows(ast, errors, root_id);
    }

    for(const ASTID id : ast.forest.post_order_ids(root_id)) {
      if(ast.forest[id] == ASTTag::ExprWhile) {
        find_returned_borrows(ast, errors, ast.forest.child_ids(id).get<1>());
      }
    }
  }

  return errors;
}

bool has_conditional_overuse(const Forest<ASTTag, ASTID>& forest, Span<ASTID> usages) {
  std::vector<std::tuple<ASTID, int, int>> usage;

  for(const ASTID expr : usages) {
    ASTID child = expr;
    for(const ASTID id : forest.ancestor_ids(expr)) {
      auto it = std::find_if(usage.begin(), usage.end(), [&](auto tup) { return std::get<0>(tup) == id; });
      if(it == usage.end()) {
        usage.emplace_back(id, 0, 0);
        it = usage.end() - 1;
      }

      auto& [_, left, right] = *it;

      const int existing_max = std::max(left, right);

      if(forest[id] == ASTTag::ExprIf) {
        const auto [cond, if_, else_] = forest.child_ids(id).take<3>();
        left += child != else_ ? 1 : 0;
        right += child != if_ ? 1 : 0;
      } else {
        left++;
        right++;
      }

      child = id;

      if(std::max(left, right) <= existing_max) {
        break;
      }
    }
  }

  return std::any_of(
    usage.begin(), usage.end(), applied([](ASTID, int left, int right) { return std::max(left, right) > 1; }));
}

std::vector<TypeCheckError> find_binding_usage_errors(
  Span<std::string_view> srcs,
  const std::unordered_set<TypeID>& copy_types,
  const AST& ast,
  const Graph<ASTID>& ident_graph,
  Span<ASTID> roots) {
  const auto has_non_copy_type = [&](auto self, Type t) -> bool {
    switch(ast.tg.get<TypeTag>(t)) {
    case TypeTag::Leaf: return copy_types.find(ast.tg.get<TypeID>(t)) == copy_types.end();
    case TypeTag::Tuple: return any_of(ast.tg.fanout(t), [=](Type t) { return self(self, t); });
    case TypeTag::Floating:
    case TypeTag::Borrow:
    case TypeTag::Fn: return false;
    }
  };

  const auto not_borrowed = [&](ASTID id) {
    const auto parent = ast.forest.parent(id);
    return !parent || ast.forest[*parent] != ASTTag::ExprBorrow;
  };

  std::vector<TypeCheckError> errors;

  for(const ASTID root : roots) {
    errors = transform_filter_to_vec(
      ast.forest.post_order_ids(root),
      [&](ASTID id) -> std::optional<TypeCheckError> {
        if(ast.forest[id] == ASTTag::PatternIdent) {
          if(sv(srcs, ast.srcs[id.get()])[0] != '_' && ident_graph.num_fanout(id) == 0) {
            return is_global_pattern(ast.forest, id) ? std::nullopt
                                                     : std::optional(TypeCheckError{UnusedBinding{}, id});
          } else if(count_if(ident_graph.fanout(id), not_borrowed) > 1 &&
                    has_non_copy_type(has_non_copy_type, ast.types[id.get()]) &&
                    has_conditional_overuse(ast.forest, ident_graph.fanout(id))) {
            return TypeCheckError{OverusedBinding{ident_graph.num_fanout(id)}, id};
          }
        }

        return std::nullopt;
      },
      std::move(errors));
  }

  return errors;
}

Type language_rule(const TypeCache& tc, AST& ast, ASTID id) {
  switch(ast.forest[id]) {
  case ASTTag::ExprLiteral: {
    const TypeID tid = std::visit(
      [](const auto& ele) { return type_id(knot::decay(knot::Type<decltype(ele)>{})); }, lookup_literal(ast, id));
    return ast.tg.add_node(TypeTag::Leaf, tid);
  }

  case ASTTag::ExprBorrow: return tc.borrow_floating;

  case ASTTag::PatternTuple:
  case ASTTag::ExprTuple: {
    const Type tuple = ast.tg.add_node(TypeTag::Tuple, TypeID{});
    for(const ASTID _ : ast.forest.child_ids(id)) {
      ast.tg.add_fanout_to_last_node(tc.floating);
    }
    return tuple;
  }

  case ASTTag::Fn: return tc.fn_floating;

  case ASTTag::Assignment:
  case ASTTag::ModuleRef:
  case ASTTag::Module: return tc.unit;

  case ASTTag::ExprCall:
  case ASTTag::ExprSelect:
  case ASTTag::ExprIf:
  case ASTTag::ExprWhile:
  case ASTTag::PatternWildCard:
  case ASTTag::PatternIdent:
  case ASTTag::ExprWith:
  case ASTTag::EnvValue:
  case ASTTag::ExprQualified:
  case ASTTag::ExprIdent: return tc.floating;
  }

  return Type::Invalid();
}

std::vector<std::vector<ASTPropagation>>
calculate_propagations(const Graph<ASTID>& ident_graph, const Forest<ASTTag, ASTID>& forest, Span<ASTID> roots) {
  std::vector<std::vector<ASTPropagation>> propagations(forest.size());

  const auto add_pair = [&](ASTID bottom, ASTID top, Propagation prop) {
    propagations[top.get()].push_back({bottom, true, prop});
    propagations[bottom.get()].push_back({top, false, prop});
  };

  for(const ASTID root : roots) {
    for(const ASTID id : forest.post_order_ids(root)) {
      switch(forest[id]) {
      case ASTTag::PatternTuple:
      case ASTTag::ExprTuple: {
        int i = 0;
        const int num_children = int(forest.num_children(id));
        for(const ASTID child : forest.child_ids(id)) {
          add_pair(id, child, TupleProp{i, num_children});
          i++;
        }
        break;
      }
      case ASTTag::ExprBorrow: add_pair(id, *forest.first_child(id), BorrowProp{}); break;
      case ASTTag::ExprSelect:
      case ASTTag::ExprIf: {
        const auto [_, if_expr, else_expr] = forest.child_ids(id).take<3>();
        add_pair(if_expr, else_expr, DirectProp{});
        add_pair(if_expr, id, DirectProp{});
        add_pair(else_expr, id, DirectProp{});
        break;
      }
      case ASTTag::ExprWhile: {
        const auto [_, body] = forest.child_ids(id).take<2>();
        add_pair(body, id, DirectProp{});
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
        for(const ASTID pattern : ident_graph.fanout(id)) {
          if(is_global_pattern(forest, pattern)) {
            // Only propogate one way
            propagations[pattern.get()].push_back(
              {id, true, ident_graph.num_fanout(id) == 1 ? Propagation{DirectProp{}} : Propagation{FloatingProp{}}});
          } else {
            add_pair(id, pattern, DirectProp{});
          }
        }

        if(const auto parent = forest.parent(id); parent && forest[*parent] == ASTTag::ExprQualified) {
          add_pair(id, *parent, DirectProp{});
        }
        break;
      }
      case ASTTag::ExprQualified:
      case ASTTag::EnvValue:
      case ASTTag::ExprLiteral:
      case ASTTag::PatternIdent:
      case ASTTag::PatternWildCard:
      case ASTTag::ModuleRef:
      case ASTTag::Module: break;
      }
    }
  }

  return propagations;
}

} // namespace

Type unify(const TypeCache& tc, TypeGraph& g, Type x, Type y, bool recurse) {
  const TypeTag xt = g.get<TypeTag>(x);
  const TypeTag yt = g.get<TypeTag>(y);

  if(xt == TypeTag::Floating) {
    return y;
  } else if(yt == TypeTag::Floating) {
    return x;
  } else if(xt != yt) {
    return Type::Invalid();
  } else if(xt == TypeTag::Leaf) {
    return g.get<TypeID>(x) == g.get<TypeID>(y) ? std::min(x, y) : Type::Invalid();
  } else if(g.fanout(x).empty() && g.fanout(y).empty()) {
    return std::min(x, y);
  } else if(g.num_fanout(x) == g.num_fanout(y)) {
    std::vector<Type> results;
    const Type floating = recurse ? Type::Invalid() : tc.floating;

    for(int i = 0; i != g.num_fanout(x); ++i) {
      const Type c = recurse ? unify(tc, g, g.fanout(x)[i], g.fanout(y)[i], true) : floating;
      if(c != Type::Invalid()) {
        results.push_back(c);
      } else {
        return Type::Invalid();
      }
    }

    return g.add_node(results, xt, {});
  } else {
    return Type::Invalid();
  }
}

ContextualResult<void, AST>
apply_language_rules(const TypeCache& tc, const TypeNames& type_names, AST ast, Span<ASTID> roots) {
  std::vector<ContextualError> errors;

  const auto apply_type = [&](ASTID id, Type t) {
    const Type unified = ast.types[id.get()].is_valid() ? unify(tc, ast.tg, ast.types[id.get()], t, true) : t;
    if(unified == Type::Invalid()) {
      errors.push_back({ast.srcs[id.get()],
                        fmt::format("expected {}, given {}",
                                    pretty_print(ast.tg, type_names, t),
                                    pretty_print(ast.tg, type_names, ast.types[id.get()]))});
    } else {
      ast.types[id.get()] = unified;
    }
  };

  for(const ASTID root_id : roots) {
    for(const ASTID id : ast.forest.post_order_ids(root_id)) {
      const Type t = language_rule(tc, ast, id);
      apply_type(id, t);

      if(const auto parent = ast.forest.parent(id); parent && ast.forest.first_child(*parent) == id) {
        if(const ASTTag tag = ast.forest[*parent]; tag == ASTTag::ExprCall) {
          apply_type(id, tc.fn_floating);
        } else if(tag == ASTTag::ExprSelect || tag == ASTTag::ExprIf || tag == ASTTag::ExprWhile) {
          apply_type(id, tc.boolean);
        }
      }
    }
  }

  return void_or_errors(std::move(errors), std::move(ast));
}

ContextualResult<OverloadResolutionData, AST> constraint_propagation(
  Span<std::string_view> srcs,
  const TypeCache& tc,
  const NativeTypeInfo& native_types,
  const Graph<ASTID>& ident_graph,
  AST ast,
  Span<ASTID> roots,
  bool debug) {

  const auto propagations = calculate_propagations(ident_graph, ast.forest, roots);

  if(debug) {
    fmt::print("Initial type graph size: {} ({} edges)\n", ast.tg.num_nodes(), ast.tg.num_edges());
  }

  std::vector<TypeCheckError> cp_errors;
  std::tie(ast.tg, ast.types, cp_errors) = constraint_propagation(
    tc,
    native_types.names,
    propagations,
    ast.forest,
    ident_graph,
    std::move(ast.tg),
    std::move(ast.types),
    roots,
    debug);

  auto [ord, overload_errors] = overload_resolution(ident_graph, ast, roots);

  const auto find_invalid_borrows = [](const AST& ast, Span<ASTID> roots) {
    std::vector<TypeCheckError> errors;
    for(const ASTID root : roots) {
      errors = transform_filter_to_vec(
        ast.forest.post_order_ids(root),
        [&](ASTID id) -> std::optional<TypeCheckError> {
          const Type type = ast.types[id.get()];
          if(ast.tg.get<TypeTag>(type) == TypeTag::Borrow) {
            switch(ast.tg.get<TypeTag>(ast.tg.fanout(type)[0])) {
            case TypeTag::Tuple: return std::optional(TypeCheckError{InvalidBorrow{"tuple"}, id});
            case TypeTag::Fn: return std::optional(TypeCheckError{InvalidBorrow{"function"}, id});
            case TypeTag::Borrow: return std::optional(TypeCheckError{InvalidBorrow{"borrow"}, id});
            case TypeTag::Leaf:
            case TypeTag::Floating: return std::nullopt;
            }
          }
          return std::nullopt;
        },
        std::move(errors));
    }

    return errors;
  };

  std::vector<ContextualError> errors = generate_errors(
    srcs,
    ast,
    native_types.names,
    ident_graph,
    cluster_adjacent(propagations,
                     flatten(std::move(cp_errors),
                             find_invalid_borrows(ast, roots),
                             find_returned_borrows(ast, roots),
                             find_unresolved_errors(ast, roots),
                             find_binding_usage_errors(srcs, native_types.copyable, ast, ident_graph, roots),
                             std::move(overload_errors))));

  if(debug) {
    fmt::print("Final type graph size: {} ({} edges)\n", ast.tg.num_nodes(), ast.tg.num_edges());
  }

  return value_or_errors(std::move(ord), std::move(errors), std::move(ast));
}

} // namespace ooze
