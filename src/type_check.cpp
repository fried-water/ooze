#include "pch.h"

#include "ooze/tree.h"
#include "pretty_print.h"
#include "type_check.h"

#include <deque>

namespace ooze {

using namespace ast;

namespace {

using Variable = std::variant<const TypedExpr*, const TypedPattern*>;

struct MismatchedType {
  Type<TypeID> conflicting_type;
};

struct MismatchedType2 {
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

using TypeCheckErrorVariant2 =
  std::variant<MismatchedType2, UnableToDeduce, InvalidBorrow, ReturnBorrow, UnusedBinding, OverusedBinding>;

struct TypeCheckError {
  TypeCheckErrorVariant type;
  Variable variable;
};

struct TypeCheckError2 {
  TypeCheckErrorVariant2 type;
  ASTID id;
};

struct DirectProp {};
struct TupleProp {
  int idx;
  int size;
};
struct BorrowProp {};
struct FnInputProp {};
struct FnOutputProp {};

using Propagation = std::variant<DirectProp, TupleProp, BorrowProp, FnInputProp, FnOutputProp>;

struct VariablePropagation {
  Variable target = {};
  bool wrap = false;
  Propagation propagation = DirectProp{};
};

struct ASTPropagation {
  ASTID target = {};
  bool wrap = false;
  Propagation propagation = DirectProp{};
};

std::string pretty_print(const Env& e, Variable v) {
  return std::visit(Overloaded{[&](const TypedExpr* expr) { return pretty_print(untype<UnTypedExpr>(e, *expr)); },
                               [](const TypedPattern* p) { return ooze::pretty_print(*p); }},
                    v);
}

struct Unifier {
  auto unify(const FloatingType&, const FloatingType&) { return std::optional(floating_type<TypeID>()); }

  template <typename T>
  std::optional<Type<TypeID>> unify(const FloatingType&, const T& other) {
    return std::optional(Type<TypeID>{other});
  }

  template <typename T>
  std::optional<Type<TypeID>> unify(const T& other, const FloatingType&) {
    return std::optional(Type<TypeID>{other});
  }
  auto unify(const std::vector<Type<TypeID>>& a, const std::vector<Type<TypeID>>& b) {
    if(a.size() != b.size()) return std::optional<Type<TypeID>>();

    std::vector<Type<TypeID>> result;
    result.reserve(a.size());
    for(size_t i = 0; i < a.size(); ++i) {
      if(auto unified_type = unify(a[i], b[i]); unified_type) {
        result.push_back(std::move(*unified_type));
      } else {
        return std::optional<Type<TypeID>>();
      }
    }

    return std::optional(tuple_type(std::move(result)));
  }
  auto unify(const BorrowType<TypeID>& a, const BorrowType<TypeID>& b) {
    auto unified = unify(*a.type, *b.type);
    return unified ? std::optional(borrow_type(std::move(*unified))) : std::nullopt;
  }
  auto unify(const TypeID& a, const TypeID& b) { return a == b ? std::optional(leaf_type(a)) : std::nullopt; }
  auto unify(const FunctionType<TypeID>& f1, const FunctionType<TypeID>& f2) {
    if(auto input = unify(*f1.input, *f2.input); input) {
      if(auto output = unify(*f1.output, *f2.output); output) {
        return std::optional(function_type(std::move(*input), std::move(*output)));
      }
    }
    return std::optional<Type<TypeID>>();
  }

  template <typename A, typename B>
  std::optional<Type<TypeID>> unify(const A&, const B&) {
    return std::optional<Type<TypeID>>{};
  }

  std::optional<Type<TypeID>> unify(const Type<TypeID>& a, const Type<TypeID>& b) {
    return std::visit([&](const auto& x, const auto& y) { return unify(x, y); }, a.v, b.v);
  }
};

std::optional<Type<TypeID>> overload_resolution(const Env& e, const std::string& name, const Type<TypeID>& type) {
  if(const auto it = e.functions.find(name); it == e.functions.end()) {
    return std::nullopt;
  } else {
    std::optional<Type<TypeID>> result;

    for(size_t i = 0; i < it->second.size(); i++) {
      if(auto opt = unify_types({it->second[i].type}, type); opt) {
        if(result) {
          return std::nullopt;
        } else {
          result = std::move(*opt);
        }
      }
    }

    return result;
  }
}

Type<TypeID> propagated_type(Propagation p, bool wrap, Type<TypeID> t) {
  return std::visit(
    Overloaded{[&](DirectProp) { return std::move(t); },
               [&](TupleProp p) {
                 if(wrap) {
                   std::vector<Type<TypeID>> v(p.size, floating_type<TypeID>());
                   v[p.idx] = std::move(t);
                   return tuple_type(std::move(v));
                 } else {
                   auto* v = std::get_if<std::vector<Type<TypeID>>>(&t.v);
                   assert(v);
                   assert(p.idx < v->size());
                   return std::move((*v)[p.idx]);
                 }
               },
               [&](BorrowProp) {
                 if(wrap) {
                   return borrow_type(std::move(t));
                 } else {
                   auto* b = std::get_if<BorrowType<TypeID>>(&t.v);
                   assert(b);
                   return std::move(*b->type);
                 }
               },
               [&](FnInputProp) {
                 if(wrap) {
                   return function_type(std::move(t), floating_type<TypeID>());
                 } else {
                   auto* f = std::get_if<FunctionType<TypeID>>(&t.v);
                   return f ? std::move(*f->input) : floating_type<TypeID>();
                 }
               },
               [&](FnOutputProp) {
                 if(wrap) {
                   return function_type(floating_type<TypeID>(), std::move(t));
                 } else {
                   auto* f = std::get_if<FunctionType<TypeID>>(&t.v);
                   return f ? std::move(*f->output) : floating_type<TypeID>();
                 }
               }},
    p);
}

TypeRef propagated_type(TypeGraph& g, Propagation p, bool wrap, TypeRef t, TypeRef floating) {
  return std::visit(
    Overloaded{[&](DirectProp) { return t; },
               [&](TupleProp p) {
                 if(wrap) {
                   const TypeRef type = g.add_node(TypeTag::Tuple, {}, {});
                   for(int i = 0; i < p.size; i++) {
                     g.add_fanout_to_last_node(i == p.idx ? t : floating);
                   }
                   return type;
                 } else {
                   return g.fanout(t)[p.idx];
                 }
               },
               [&](BorrowProp) { return wrap ? g.add_node(std::array{t}, TypeTag::Borrow, {}, {}) : g.fanout(t)[0]; },
               [&](FnInputProp) {
                 return wrap ? g.add_node(std::array{t, floating}, TypeTag::Fn, {}, {}) : g.fanout(t)[0];
               },
               [&](FnOutputProp) {
                 return wrap ? g.add_node(std::array{floating, t}, TypeTag::Fn, {}, {}) : g.fanout(t)[1];
               }},
    p);
}

template <typename B, typename T>
void add_pair(Map<Variable, std::vector<VariablePropagation>>& propagations,
              const B* bottom,
              const T* top,
              Propagation prop = DirectProp{}) {
  propagations[top].push_back({bottom, true, prop});
  propagations[bottom].push_back({top, false, prop});
}

template <typename B, typename T>
void add_tuple(Map<Variable, std::vector<VariablePropagation>>& propagations,
               const std::vector<B>& tuple,
               const T* parent) {
  int i = 0;
  for(const auto& child : tuple) {
    add_pair(propagations, parent, &child, TupleProp{i, int(tuple.size())});
    i++;
  }
}

struct PropagationVisitor {
  std::vector<const TypedExpr*> undeclared_bindings;
  std::vector<Map<std::string, const TypedPattern*>> binding_names = {{}};
  Map<Variable, std::vector<VariablePropagation>> propagations;
  Map<const TypedPattern*, std::pair<int, int>> binding_usages;
  bool borrowed = false;

  void visit(const TypedPattern& pattern) {
    knot::preorder(pattern, [&](const TypedPattern& pattern) {
      knot::visit(
        pattern.v,
        Overloaded{[&](const Ident& ident) {
                     binding_names.back()[ident.name] = &pattern;
                     binding_usages.emplace(&pattern, std::pair(0, 0));
                   },
                   [&](const std::vector<TypedPattern>& tuple) { add_tuple(propagations, tuple, &pattern); }});
    });
  }

  void visit(const TypedExpr& expr) {
    knot::preorder(expr, [&](const TypedExpr& expr) {
      knot::visit(
        expr.v,
        Overloaded{[&](const TypedCallExpr& call) {
                     add_pair(propagations, call.callee.get(), call.arg.get(), FnInputProp{});
                     add_pair(propagations, call.callee.get(), &expr, FnOutputProp{});
                   },
                   [&](const std::vector<TypedExpr>& tuple) { add_tuple(propagations, tuple, &expr); },
                   [&](const TypedBorrowExpr& b) { add_pair(propagations, &expr, b.expr.get(), BorrowProp{}); },
                   [&](const Ident& ident) {
                     const auto sit = std::find_if(binding_names.rbegin(), binding_names.rend(), [&](const auto& map) {
                       return map.find(ident.name) != map.end();
                     });
                     if(sit != binding_names.rend()) {
                       const TypedPattern* binding = sit->at(ident.name);
                       add_pair(propagations, &expr, binding);
                       if(borrowed) {
                         binding_usages[binding].second++;
                       } else {
                         binding_usages[binding].first++;
                       }
                     } else {
                       undeclared_bindings.push_back(&expr);
                     }
                   },
                   [&](const TypedSelectExpr& select) {
                     add_pair(propagations, select.if_expr.get(), select.else_expr.get(), DirectProp{});
                     add_pair(propagations, select.if_expr.get(), &expr, DirectProp{});
                     add_pair(propagations, select.else_expr.get(), &expr, DirectProp{});
                   },
                   [&](const TypedScopeExpr& scope) {
                     binding_names.emplace_back();

                     add_pair(propagations, &expr, scope.result.get());

                     for(const TypedAssignment& a : scope.assignments) {
                       visit(*a.expr);
                       visit(a.pattern);
                       add_pair(propagations, &a.pattern, a.expr.get());
                     }

                     visit(*scope.result);

                     binding_names.pop_back();
                   }});
      borrowed = std::holds_alternative<TypedBorrowExpr>(expr.v);
      return !std::holds_alternative<TypedScopeExpr>(expr.v);
    });
  }
};

auto calculate_propagations(const TypedFunction& f) {
  PropagationVisitor v;

  v.visit(f.pattern);
  v.visit(f.expr);

  return std::tuple(std::move(v.propagations), std::move(v.binding_usages), std::move(v.undeclared_bindings));
}

auto calculate_propagations(const Graph<ASTID>& ident_graph, const Forest<ASTTag, ASTID>& ast) {
  std::vector<std::vector<ASTPropagation>> propagations(ast.size());

  const auto add_pair = [&](ASTID bottom, ASTID top, Propagation prop) {
    propagations[top.get()].push_back({bottom, true, prop});
    propagations[bottom.get()].push_back({top, false, prop});
  };

  for(ASTID id : ast.ids()) {
    const ASTTag tag = ast[id];
    switch(tag) {
    case ASTTag::PatternTuple:
    case ASTTag::ExprTuple: {
      int i = 0;
      const int num_children = ast.num_children(id);
      for(ASTID child : ast.child_ids(id)) {
        add_pair(id, child, TupleProp{i, num_children});
        i++;
      }
      break;
    }
    case ASTTag::ExprBorrow: add_pair(id, *ast.first_child(id), BorrowProp{}); break;
    case ASTTag::ExprSelect: {
      const ASTID cond = *ast.first_child(id);
      const ASTID if_expr = *ast.next_sibling(cond);
      const ASTID else_expr = *ast.next_sibling(if_expr);
      add_pair(if_expr, else_expr, DirectProp{});
      add_pair(if_expr, id, DirectProp{});
      add_pair(else_expr, id, DirectProp{});
      break;
    }
    case ASTTag::ExprCall: {
      const ASTID callee = *ast.first_child(id);
      const ASTID arg = *ast.next_sibling(callee);
      add_pair(callee, arg, FnInputProp{});
      add_pair(callee, id, FnOutputProp{});
      break;
    }
    case ASTTag::ExprWith: {
      const ASTID assign = *ast.first_child(id);
      add_pair(id, *ast.next_sibling(assign), DirectProp{});
      break;
    }
    case ASTTag::Assignment: {
      const ASTID pattern = *ast.first_child(id);
      const ASTID expr = *ast.next_sibling(pattern);
      add_pair(pattern, expr, DirectProp{});
      break;
    }
    case ASTTag::Fn: {
      const ASTID pattern = *ast.first_child(id);
      const ASTID expr = *ast.next_sibling(pattern);
      add_pair(id, pattern, FnInputProp{});
      add_pair(id, expr, FnOutputProp{});
      break;
    }
    case ASTTag::ExprIdent: {
      assert(ident_graph.num_fanout(id) <= 1);
      if(ident_graph.num_fanout(id) == 1) {
        add_pair(id, ident_graph.fanout(id)[0], DirectProp{});
      }
    }
    case ASTTag::ExprLiteral:
    case ASTTag::PatternIdent:
    case ASTTag::PatternWildCard:
    case ASTTag::RootFn: break;
    }
  }

  return propagations;
}

std::deque<std::pair<Variable, Type<TypeID>>>
create_initial_candidates(const Env& e, const TypedFunction& f, std::optional<FunctionType<TypeID>> type_hint) {
  std::deque<std::pair<Variable, Type<TypeID>>> candidates;

  knot::preorder(f,
                 Overloaded{
                   [&](const TypedExpr& expr) { candidates.emplace_back(&expr, expr.type); },
                   [&](const TypedPattern& pattern) { candidates.emplace_back(&pattern, pattern.type); },
                 });

  if(type_hint) {
    candidates.emplace_back(&f.pattern, std::move(*type_hint->input));
    candidates.emplace_back(&f.expr, std::move(*type_hint->output));
  }

  return candidates;
}

std::vector<std::vector<TypeCheckError>>
cluster_adjacent(const Env& e,
                 const Map<Variable, std::vector<VariablePropagation>>& propagations,
                 std::vector<TypeCheckError> errors) {
  // Group clusterable errors by variable
  Map<Variable, std::vector<TypeCheckErrorVariant>> errors_by_variable;
  for(auto& [type, var] : errors) {
    errors_by_variable[var].push_back(std::move(type));
  }

  std::vector<std::vector<TypeCheckError>> groups;

  Set<Variable> visited;

  for(const auto& [var, _] : errors_by_variable) {
    std::vector<TypeCheckError> group;
    std::vector<Variable> to_visit{var};
    while(!to_visit.empty()) {
      Variable v = to_visit.back();
      to_visit.pop_back();

      if(visited.insert(v).second) {
        if(const auto it = errors_by_variable.find(v); it != errors_by_variable.end()) {
          group = transform_to_vec(
            std::move(it->second),
            [&](auto e) {
              return TypeCheckError{std::move(e), v};
            },
            std::move(group));
        }

        if(const auto pit = propagations.find(v); pit != propagations.end()) {
          for(const VariablePropagation& p : pit->second) {
            if(errors_by_variable.find(p.target) != errors_by_variable.end()) {
              to_visit.push_back(p.target);
            }
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

ContextualError generate_error(const Env& e, const TypeCheckError& error, const Type<TypeID>& type) {
  return std::visit(
    Overloaded{
      [&](const MismatchedType& m, const auto* var) {
        return ContextualError{
          var->ref, fmt::format("expected {}, given {}", pretty_print(e, type), pretty_print(e, m.conflicting_type))};
      },
      [&](const UnableToDeduce&, const auto* var) {
        return ContextualError{
          var->ref, fmt::format("unable to fully deduce type, deduced: {}", pretty_print(e, type))};
      },
      [](const ReturnBorrow&, const TypedExpr* expr) {
        return ContextualError{expr->ref, "cannot return a borrowed value"};
      },
      [](const InvalidBorrow& b, const auto* var) {
        return ContextualError{var->ref, fmt::format("cannot borrow a {}", b.type)};
      },
      [](const UnusedBinding&, const TypedPattern* p) {
        return ContextualError{p->ref,
                               fmt::format("unused binding '{}'", std::get<Ident>(p->v).name),
                               {"prefix with an _ to silence this error"}};
      },
      [](const OverusedBinding& b, const TypedPattern* p) {
        return ContextualError{p->ref, fmt::format("binding '{}' used {} times", std::get<Ident>(p->v).name, b.count)};
      },
      [](const auto&, const auto* var) {
        assert(false);
        return ContextualError{};
      }},
    error.type,
    error.variable);
}

std::vector<ContextualError> generate_errors(const Env& e,
                                             const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                             std::vector<TypeCheckError> errors) {
  const std::vector<std::vector<TypeCheckError>> error_clusters = cluster_adjacent(e, propagations, std::move(errors));

  // Find most *relevant* error per cluster
  std::vector<ContextualError> final_errors =
    transform_to_vec(error_clusters, [&](const std::vector<TypeCheckError>& group) {
      const auto projection = [&](const TypeCheckError& error) {
        const auto type_complexity = [](const Type<TypeID>& t) {
          return knot::preorder_accumulate(t, 0, [](int acc, const Type<TypeID>&) { return acc + 1; });
        };

        const auto [type, ref] = std::visit([](const auto* p) { return std::tie(p->type, p->ref); }, error.variable);
        const int complexity =
          type_complexity(type) +
          std::visit(Overloaded{[&](const MismatchedType& m) { return type_complexity(m.conflicting_type); },
                                [](const auto&) { return 0; }},
                     error.type);
        return std::tuple(error.type.index(), complexity, ref.end, ref.begin);
      };

      const TypeCheckError& most_relevant_error =
        *std::min_element(group.begin(), group.end(), [&](const auto& lhs, const auto& rhs) {
          return projection(lhs) < projection(rhs);
        });

      return generate_error(
        e, most_relevant_error, std::visit([](const auto* p) { return p->type; }, most_relevant_error.variable));
    });

  return sorted(std::move(final_errors), [](const auto& e) { return std::tie(e.ref.end, e); });
}

std::vector<std::vector<TypeCheckError2>>
cluster_adjacent(const std::vector<std::vector<ASTPropagation>>& propagations, std::vector<TypeCheckError2> errors) {
  // Group clusterable errors by id
  Map<ASTID, std::vector<TypeCheckErrorVariant2>> errors_by_id;
  for(auto& [error, id] : errors) {
    errors_by_id[id].push_back(std::move(error));
  }

  std::vector<std::vector<TypeCheckError2>> groups;

  Set<ASTID> visited;
  std::vector<ASTID> to_visit;

  for(const auto& [id, _] : errors_by_id) {
    std::vector<TypeCheckError2> group;
    to_visit.push_back(id);
    while(!to_visit.empty()) {
      ASTID id = to_visit.back();
      to_visit.pop_back();

      if(visited.insert(id).second) {
        if(const auto it = errors_by_id.find(id); it != errors_by_id.end()) {
          group = transform_to_vec(
            std::move(it->second),
            [&](auto error) {
              return TypeCheckError2{std::move(error), id};
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

ContextualError2
generate_error(const SrcMap& sm, const Env& e, const AST& ast, const TypeGraph& tg, const TypeCheckError2& error) {
  const TypeRef type = ast.types[error.id.get()];
  const SrcRef ref = ast.srcs[error.id.get()];
  return std::visit(
    Overloaded{
      [&](const MismatchedType2& m) {
        return ContextualError2{
          ref,
          fmt::format("expected {}, given {}", pretty_print(e, tg, type), pretty_print(e, tg, m.conflicting_type))};
      },
      [&](const UnableToDeduce&) {
        return ContextualError2{
          ref, fmt::format("unable to fully deduce type, deduced: {}", pretty_print(e, tg, type))};
      },
      [&](const ReturnBorrow&) {
        return ContextualError2{ref, "cannot return a borrowed value"};
      },
      [&](const InvalidBorrow& b) {
        return ContextualError2{ref, fmt::format("cannot borrow a {}", b.type)};
      },
      [&](const UnusedBinding&) {
        return ContextualError2{
          ref, fmt::format("unused binding '{}'", sv(sm, ref)), {"prefix with an _ to silence this error"}};
      },
      [&](const OverusedBinding& b) {
        return ContextualError2{ref, fmt::format("binding '{}' used {} times", sv(sm, ref), b.count)};
      }},
    error.type);
}

std::vector<ContextualError2>
generate_errors(const SrcMap& sm,
                const Env& e,
                const AST& ast,
                const TypeGraph& tg,
                std::vector<std::vector<TypeCheckError2>> error_clusters) {
  // Find most *relevant* error per cluster
  return sorted(
    transform_to_vec(
      error_clusters,
      [&](const std::vector<TypeCheckError2>& group) {
        const auto projection = [&](const TypeCheckError2& error) {
          const auto type_complexity = [&](auto self, TypeRef t) -> i32 {
            return knot::accumulate(tg.fanout(t), 1, [&](TypeRef fanout) { return self(self, fanout); });
          };

          const SrcRef ref = ast.srcs[error.id.get()];
          const int complexity =
            type_complexity(type_complexity, ast.types[error.id.get()]) +
            std::visit(
              Overloaded{[&](const MismatchedType2& m) { return type_complexity(type_complexity, m.conflicting_type); },
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
          sm, e, ast, tg, *std::min_element(group.begin(), group.end(), [&](const auto& lhs, const auto& rhs) {
            return projection(lhs) < projection(rhs);
          }));
      }),
    [](const auto& e) { return std::tie(e.ref.slice.end, e); });
}

std::pair<Map<Variable, Type<TypeID>>, std::vector<TypeCheckError>> constraint_propagation(
  const Env& e,
  const Map<Variable, std::vector<VariablePropagation>>& propagations,
  const std::vector<const TypedExpr*> undeclared_bindings,
  std::deque<std::pair<Variable, Type<TypeID>>> to_visit,
  bool debug = false) {
  Map<Variable, Type<TypeID>> types;
  Map<Variable, Type<TypeID>> conflicting_types;

  if(debug) {
    fmt::print("Initial queue {}\n", to_visit.size());
    for(const auto& [var, type] : to_visit) {
      fmt::print("  {:>7} {:>10}: {}\n",
                 var.index() == 0 ? "expr" : "pattern",
                 pretty_print(e, var),
                 pretty_print(untype<Type<NamedType>>(e, type)));
    }
  }

  while(!to_visit.empty()) {
    auto [target, type] = std::move(to_visit.front());
    to_visit.pop_front();

    auto it = types.find(target);
    if(it == types.end()) {
      it = types.emplace(target, floating_type<TypeID>()).first;
    }

    const Type<TypeID> original_type = it->second;

    if(debug) {
      fmt::print("Processing {} {}\n   propagated {}\n   existing   {}\n",
                 target.index() == 0 ? "expr" : "pattern",
                 pretty_print(e, target),
                 pretty_print(untype<Type<NamedType>>(e, type)),
                 pretty_print(untype<Type<NamedType>>(e, original_type)));
    }

    // Apply propagated type
    if(it != types.end()) {
      if(auto unified = unify_types(type, it->second); unified) {
        it->second = std::move(*unified);
      } else {
        conflicting_types.insert_or_assign(target, type);
        continue;
      }
    } else {
      it->second = std::move(type);
    }

    // Attempt overload resolution
    knot::visit(target, [&](const TypedExpr* t) {
      knot::visit(t->v, [&](const Ident& i) {
        if(std::find(undeclared_bindings.begin(), undeclared_bindings.end(), t) != undeclared_bindings.end()) {
          if(auto overload_type = overload_resolution(e, i.name, it->second); overload_type) {
            it->second = std::move(*overload_type);
          }
        }
      });
    });

    if(debug) {
      fmt::print("   final      {}\n", pretty_print(untype<Type<NamedType>>(e, it->second)));
    }

    if(const auto pit = propagations.find(target); pit != propagations.end() && original_type != it->second) {
      for(const auto [dst_target, wrap, propagation] : pit->second) {
        Type<TypeID> dst_type = propagated_type(propagation, wrap, it->second);

        if(dst_type != floating_type<TypeID>()) {
          if(debug) {
            fmt::print("    p {} : {} {}\n",
                       pretty_print(untype<Type<NamedType>>(e, dst_type)),
                       dst_target.index() == 0 ? "expr" : "pattern",
                       pretty_print(e, dst_target));
          }

          to_visit.emplace_back(dst_target, std::move(dst_type));
        }
      }
    }
  }

  if(debug) {
    fmt::print("Final types\n");
    for(const auto& [target, type] : types) {
      fmt::print("    {:<10} {}\n", pretty_print(e, target), pretty_print(untype<Type<NamedType>>(e, type)));
    }

    fmt::print("Conflicting types {}\n", conflicting_types.size());
    for(const auto& [target, type] : conflicting_types) {
      fmt::print("    {:<10} -> {}\n", pretty_print(e, target), pretty_print(untype<Type<NamedType>>(e, type)));
    }
  }

  return {std::move(types), transform_to_vec(std::move(conflicting_types), [](auto&& p) {
            return TypeCheckError{MismatchedType{std::move(p.second)}, p.first};
          })};
}

std::tuple<TypeGraph, std::vector<TypeRef>, std::vector<TypeCheckError2>> constraint_propagation(
  const Env& e,
  const std::vector<std::vector<ASTPropagation>>& propagations,
  const std::vector<ASTID> undeclared_bindings,
  const Forest<ASTTag, ASTID>& forest,
  TypeGraph tg,
  std::vector<TypeRef> types,
  bool debug = false) {
  std::deque<std::pair<ASTID, TypeRef>> to_visit;

  for(ASTID id : forest.ids()) {
    if(tg.get<TypeTag>(types[id.get()]) != TypeTag::Floating) {
      to_visit.emplace_back(id, types[id.get()]);
    }
  }

  const TypeRef floating = tg.add_node(TypeTag::Floating, {}, TypeID{});

  std::fill(types.begin(), types.end(), floating);
  std::vector<TypeRef> conflicting_types(forest.size(), TypeRef::Invalid());

  while(!to_visit.empty()) {
    const auto [id, type] = std::move(to_visit.front());
    to_visit.pop_front();

    const TypeRef original_type = types[id.get()];

    if(debug) {
      fmt::print("Processing {} {}\n   propagated {}\n   existing   {}\n",
                 id.get(),
                 knot::debug(forest[id]),
                 pretty_print(e, tg, type),
                 pretty_print(e, tg, original_type));
    }

    // Apply propagated type
    if(const TypeRef unified = unify(tg, original_type, type, true); unified.is_valid()) {
      types[id.get()] = unified;
    } else {
      conflicting_types[id.get()] = type;
    }

    // TODO: Attempt overload resolution

    if(debug) {
      fmt::print("   final      {}\n", pretty_print(e, tg, types[id.get()]));
    }

    if(!compare_dags(tg, original_type, types[id.get()])) {
      for(const auto [dst, wrap, propagation] : propagations[id.get()]) {
        const TypeRef ptype = propagated_type(tg, propagation, wrap, types[id.get()], floating);

        if(debug) {
          fmt::print(" propagating {} -> {} {}\n", pretty_print(e, tg, ptype), dst.get(), knot::debug(forest[dst]));
        }

        to_visit.emplace_back(dst, ptype);
      }
    }
  }

  if(debug) {
    fmt::print("Final types\n");
    for(ASTID id : forest.ids()) {
      fmt::print("    {:<4} {:<25} = {}", id.get(), knot::debug(forest[id]), pretty_print(e, tg, types[id.get()]));

      if(conflicting_types[id.get()].is_valid()) {
        fmt::print(" (conflicting {})", pretty_print(e, tg, conflicting_types[id.get()]));
      }

      fmt::print("\n");
    }
  }

  return {std::move(tg), std::move(types), transform_filter_to_vec(forest.ids(), [&](ASTID id) {
            const TypeRef t = conflicting_types[id.get()];
            return t.is_valid() ? std::optional(TypeCheckError2{MismatchedType2{t}, id}) : std::nullopt;
          })};
}

std::vector<TypeCheckError> find_returned_borrows(const TypedFunction& f) {
  std::vector<TypeCheckError> errors;

  knot::preorder(
    f.expr,
    Overloaded{[&](const TypedExpr& e) {
                 const bool has_borrow =
                   knot::preorder_accumulate(e.type, false, [](bool, const BorrowType<TypeID>&) { return true; });

                 if(has_borrow) {
                   errors.push_back({ReturnBorrow{}, &e});
                 }

                 return true;
               },
               // Dont search through functions and assignments
               [&](const TypedAssignment&) { return false; },
               [&](const TypedCallExpr&) { return false; }});

  return errors;
}

std::vector<TypeCheckError> find_invalid_borrows(const TypedFunction& f) {
  std::vector<TypeCheckError> errors;

  const auto check_borrowed_type = [&](Variable var, const Type<TypeID>& t) {
    knot::visit(
      t.v,
      Overloaded{
        [&](const std::vector<Type<TypeID>>&) {
          errors.push_back({InvalidBorrow{"tuple"}, var});
        },
        [&](const FunctionType<TypeID>&) {
          errors.push_back({InvalidBorrow{"function"}, var});
        },
        [&](const BorrowType<TypeID>&) {
          errors.push_back({InvalidBorrow{"borrow"}, var});
        },
      });
  };

  // TODO allow errors to be associated with types, not just patterns and exprs
  co_visit(f.pattern, f.pattern.type, [&](const TypedPattern& p, const Type<TypeID>& t, const auto&, const auto&) {
    knot::visit(t.v, [&](const BorrowType<TypeID>& b) { check_borrowed_type(&p, *b.type); });
  });

  knot::preorder(f.expr, [&](const TypedExpr& e) {
    knot::visit(e.v, [&](const TypedBorrowExpr& b) { check_borrowed_type(&e, b.expr->type); });
  });

  return errors;
}

std::vector<TypeCheckError> find_binding_usage_errors(
  const Env& env, const TypedFunction& f, const Map<const TypedPattern*, std::pair<int, int>>& binding_usage) {
  std::vector<TypeCheckError> errors;

  for(const auto [pattern, usage] : binding_usage) {
    if(usage == std::pair(0, 0) && std::get<Ident>(pattern->v).name[0] != '_') {
      errors.push_back({UnusedBinding{}, pattern});
    } else if(usage.first > 1) {
      const std::optional<TypeID> type = std::visit(
        Overloaded{[](TypeID t) { return std::optional(t); }, [](const auto&) { return std::optional<TypeID>(); }},
        pattern->type.v);

      if(type && env.copy_types.find(*type) == env.copy_types.end()) {
        errors.push_back({OverusedBinding{usage.first}, pattern});
      }
    }
  }

  return errors;
}

void find_returned_borrows(const AST& ast, const TypeGraph& tg, std::vector<TypeCheckError2>& errors, ASTID id) {
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

std::vector<TypeCheckError2> find_returned_borrows(const AST& ast, const TypeGraph& tg) {
  std::vector<TypeCheckError2> errors;

  for(ASTID id : ast.forest.ids()) {
    if(ast.forest[id] == ASTTag::Fn) {
      find_returned_borrows(ast, tg, errors, ast.forest.child_ids(id).get<1>());
    }
  }

  return errors;
}

std::vector<TypeCheckError2> find_binding_usage_errors(
  const SrcMap& sm, const Env& e, const AST& ast, const TypeGraph& tg, const Graph<ASTID>& ident_graph) {
  const auto has_non_copy_type = [&](auto self, TypeRef t) -> bool {
    switch(tg.get<TypeTag>(t)) {
    case TypeTag::Leaf: return e.copy_types.find(tg.get<TypeID>(t)) == e.copy_types.end();
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

  return transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) -> std::optional<TypeCheckError2> {
    const TypeRef t = ast.types[id.get()];
    if(ast.forest[id] != ASTTag::PatternIdent) {
      return std::nullopt;
    } else if(sv(sm, ast.srcs[id.get()])[0] != '_' && ident_graph.num_fanout(id) == 0) {
      return TypeCheckError2{UnusedBinding{}, id};
    } else if(count_if(ident_graph.fanout(id), not_borrowed) > 1 &&
              has_non_copy_type(has_non_copy_type, ast.types[id.get()])) {
      return TypeCheckError2{OverusedBinding{ident_graph.num_fanout(id)}, id};
    } else {
      return std::nullopt;
    }
  });
}

template <typename T>
ContextualResult<T> apply_language_rules(const Env& env, T t) {
  const auto floating_tuple_type = [](size_t size) {
    return tuple_type(std::vector<Type<TypeID>>(size, floating_type<TypeID>()));
  };

  const auto language_type = [&](const auto& t) {
    return std::visit(
      Overloaded{[&](const std::vector<TypedExpr>& v) { return floating_tuple_type(v.size()); },
                 [&](const std::vector<TypedPattern>& v) { return floating_tuple_type(v.size()); },
                 [&](const TypedBorrowExpr&) { return borrow_type(floating_type<TypeID>()); },
                 [&](const Literal& l) {
                   return leaf_type(
                     std::visit([](const auto& ele) { return type_id(knot::decay(knot::Type<decltype(ele)>{})); }, l));
                 },
                 [&](const auto&) { return floating_type<TypeID>(); }},
      t.v);
  };

  std::vector<ContextualError> errors;
  const auto check = [&](auto& ele, const Type<TypeID>& lang_type) {
    if(auto unified = unify_types(lang_type, ele.type); unified) {
      ele.type = std::move(*unified);
    } else {
      errors.push_back(
        {ele.ref, fmt::format("expected {}, given {}", pretty_print(env, lang_type), pretty_print(env, ele.type))});
    }
  };

  knot::preorder(
    t,
    Overloaded{
      [&](TypedExpr& expr) {
        check(expr, language_type(expr));
        knot::visit(
          expr.v,
          Overloaded{
            [&](TypedCallExpr& call) {
              check(*call.callee, function_type<TypeID>(floating_type<TypeID>(), floating_type<TypeID>()));
            },
            [&](TypedSelectExpr& select) { check(*select.condition, leaf_type(type_id(knot::Type<bool>{}))); }});
      },
      [&](TypedPattern& pattern) { check(pattern, language_type(pattern)); }

    });

  return value_or_errors(std::move(t), std::move(errors));
}

TypeRef
language_rule(const AST& ast, TypeGraph& g, ASTID id, TypeRef floating, TypeRef borrow, TypeRef fn, TypeRef unit) {

  switch(ast.forest[id]) {
  case ASTTag::ExprLiteral: {
    return g.add_node(TypeTag::Leaf,
                      {},
                      std::visit([](const auto& ele) { return type_id(knot::decay(knot::Type<decltype(ele)>{})); },
                                 lookup_literal(ast, id)));
  }

  case ASTTag::ExprBorrow: return borrow;

  case ASTTag::PatternTuple:
  case ASTTag::ExprTuple: {
    const TypeRef tuple = g.add_node(TypeTag::Tuple, {}, TypeID{});
    for(ASTID _ : ast.forest.child_ids(id)) {
      g.add_fanout_to_last_node(floating);
    }
    return tuple;
  }

  case ASTTag::Fn: return fn;

  case ASTTag::RootFn:
  case ASTTag::Assignment: return unit;

  case ASTTag::ExprCall:
  case ASTTag::ExprSelect:
  case ASTTag::PatternWildCard:
  case ASTTag::PatternIdent:
  case ASTTag::ExprWith:
  case ASTTag::ExprIdent: return floating;
  }

  return TypeRef::Invalid();
}

} // namespace

std::vector<const TypedExpr*> undeclared_bindings(const TypedFunction& f) {
  return std::get<2>(calculate_propagations(f));
}

std::optional<Type<TypeID>> unify_types(const Type<TypeID>& a, const Type<TypeID>& b) { return Unifier{}.unify(a, b); }

TypeRef unify(TypeGraph& g, TypeRef x, TypeRef y, bool recurse) {
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
    const TypeRef floating = recurse ? TypeRef::Invalid() : g.add_node(TypeTag::Floating, {}, TypeID::Invalid());

    for(; x_it != x_count && y_it != y_count; ++y_it, ++x_it) {
      const TypeRef c = recurse ? unify(g, g.fanout(x)[x_it], g.fanout(y)[y_it], true) : floating;
      if(c != TypeRef::Invalid()) {
        results.push_back(c);
      } else {
        return TypeRef::Invalid();
      }
    }

    if(x_it == x_count && y_it == y_count) {
      const TypeRef id = g.add_node(xt, {}, g.get<TypeID>(x));
      for(TypeRef t : results) g.add_fanout_to_last_node(t);
      return id;
    } else {
      return TypeRef::Invalid();
    }
  }
}

ContextualResult2<std::pair<AST, TypeGraph>> apply_language_rules(const Env& e, AST ast, TypeGraph tg) {
  const TypeRef floating = tg.add_node(TypeTag::Floating, {}, TypeID{});
  const TypeRef boolean = tg.add_node(TypeTag::Leaf, {}, type_id(knot::Type<bool>{}));
  const TypeRef unit = tg.add_node(TypeTag::Tuple, {}, TypeID{});

  const TypeRef borrow = tg.add_node(TypeTag::Borrow, {}, TypeID{});
  tg.add_fanout_to_last_node(floating);

  const TypeRef fn = tg.add_node(TypeTag::Fn, {}, TypeID{});
  tg.add_fanout_to_last_node(floating);
  tg.add_fanout_to_last_node(floating);

  std::vector<ContextualError2> errors;

  const auto apply_type = [&](ASTID id, TypeRef t) {
    const TypeRef unified = ast.types[id.get()].is_valid() ? unify(tg, ast.types[id.get()], t, true) : t;
    if(unified == TypeRef::Invalid()) {
      errors.push_back(
        {ast.srcs[id.get()],
         fmt::format("expected {}, given {}", pretty_print(e, tg, t), pretty_print(e, tg, ast.types[id.get()]))});
    } else {
      ast.types[id.get()] = unified;
    }
  };

  for(const ASTID id : ast.forest.ids()) {
    apply_type(id, language_rule(ast, tg, id, floating, borrow, fn, unit));

    const auto parent = ast.forest.parent(id);
    if(parent && ast.forest[*parent] == ASTTag::ExprCall && ast.forest.first_child(*parent) == id) {
      apply_type(id, fn);
    } else if(parent && ast.forest[*parent] == ASTTag::ExprSelect && ast.forest.first_child(*parent) == id) {
      apply_type(id, boolean);
    }
  }

  return value_or_errors(std::pair(std::move(ast), std::move(tg)), std::move(errors));
}

ContextualResult<TypedFunction>
type_check(const Env& e, TypedFunction f, std::optional<FunctionType<TypeID>> type_hint, bool debug) {
  return apply_language_rules(e, std::move(f)).and_then([&](TypedFunction f) {
    const auto [propagations, binding_usage, undeclared_bindings] = calculate_propagations(f);

    auto [types, cp_errors] = constraint_propagation(
      e, propagations, undeclared_bindings, create_initial_candidates(e, f, std::move(type_hint)), debug);

    // Annotate types
    knot::preorder(f,
                   Overloaded{
                     [t = &types](TypedPattern& p) { p.type = t->at(&p); },
                     [t = &types](TypedExpr& e) { e.type = t->at(&e); },
                   });

    std::vector<ContextualError> errors = generate_errors(
      e,
      propagations,
      flatten(std::move(cp_errors),
              find_returned_borrows(f),
              find_invalid_borrows(f),
              find_binding_usage_errors(e, f, binding_usage)));

    return value_or_errors(std::move(f), std::move(errors));
  });
}

ContextualResult<TypedPattern> type_check(const Env& e, TypedPattern pattern, Type<TypeID> type) {
  return apply_language_rules(e, std::move(pattern)).and_then([&](TypedPattern pattern) {
    Map<Variable, std::vector<VariablePropagation>> propagations;
    knot::preorder(pattern, [&](const TypedPattern& pattern) {
      knot::visit(pattern.v, [&](const std::vector<TypedPattern>& tuple) { add_tuple(propagations, tuple, &pattern); });
    });

    std::deque<std::pair<Variable, Type<TypeID>>> candidates;
    knot::preorder(pattern, [&](const TypedPattern& pattern) { candidates.emplace_back(&pattern, pattern.type); });

    candidates.emplace_back(&pattern, std::move(type));

    auto [types, cp_errors] = constraint_propagation(e, propagations, {}, std::move(candidates));

    knot::preorder(pattern, [t = &types](TypedPattern& p) { p.type = t->at(&p); });

    std::vector<ContextualError> errors = generate_errors(e, propagations, std::move(cp_errors));

    return value_or_errors(std::move(pattern), std::move(errors));
  });
}

std::vector<ContextualError> check_fully_resolved(const Env& e, const TypedFunction& f) {
  std::vector<TypeCheckError> errors;

  const auto check_unresolved = [&](const auto& v) {
    if(knot::preorder_accumulate(v.type, false, [](bool, FloatingType) { return true; })) {
      errors.push_back({UnableToDeduce{}, &v});
    }
  };

  knot::preorder(f,
                 Overloaded{
                   [&](const TypedPattern& p) { check_unresolved(p); },
                   [&](const TypedExpr& e) { check_unresolved(e); },
                 });

  const auto [propagations, binding_usage, undeclared_bindings] = calculate_propagations(f);
  return generate_errors(e, propagations, std::move(errors));
}

ContextualResult2<std::pair<AST, TypeGraph>>
type_check(const SrcMap& sm,
           const Env& e,
           const Graph<ASTID>& ident_graph,
           const std::vector<ASTID>& undeclared_bindings,
           AST ast,
           TypeGraph tg,
           bool debug) {
  return apply_language_rules(e, std::move(ast), std::move(tg)).and_then(applied([&](AST ast, TypeGraph tg) {
    const std::vector<std::vector<ASTPropagation>> propagations = calculate_propagations(ident_graph, ast.forest);

    std::vector<TypeCheckError2> cp_errors;
    std::tie(tg, ast.types, cp_errors) = constraint_propagation(
      e, propagations, undeclared_bindings, ast.forest, std::move(tg), std::move(ast.types), debug);

    const auto find_invalid_borrows = [](const AST& ast, const TypeGraph& tg) {
      return transform_filter_to_vec(ast.forest.ids(), [&](ASTID id) -> std::optional<TypeCheckError2> {
        const TypeRef type = ast.types[id.get()];
        if(tg.get<TypeTag>(type) == TypeTag::Borrow) {
          switch(tg.get<TypeTag>(tg.fanout(type)[0])) {
          case TypeTag::Tuple: return std::optional(TypeCheckError2{InvalidBorrow{"tuple"}, id});
          case TypeTag::Fn: return std::optional(TypeCheckError2{InvalidBorrow{"function"}, id});
          case TypeTag::Borrow: return std::optional(TypeCheckError2{InvalidBorrow{"borrow"}, id});
          case TypeTag::Leaf:
          case TypeTag::Floating: return std::nullopt;
          }
        }
        return std::nullopt;
      });
    };

    std::vector<ContextualError2> errors = generate_errors(
      sm,
      e,
      ast,
      tg,
      cluster_adjacent(propagations,
                       flatten(std::move(cp_errors),
                               find_invalid_borrows(ast, tg),
                               find_returned_borrows(ast, tg),
                               find_binding_usage_errors(sm, e, ast, tg, ident_graph))));

    return value_or_errors(std::pair(std::move(ast), std::move(tg)), std::move(errors));
  }));
}

} // namespace ooze
