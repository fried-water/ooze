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
  CompoundType<TypeID> conflicting_type;
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
  Variable variable;
};

template <typename T>
using TypeCheckResult = Result<T, std::vector<TypeCheckError>>;

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

std::string pretty_print(const Env& e, Variable v) {
  return std::visit(Overloaded{[&](const TypedExpr* expr) { return pretty_print(untype<UnTypedExpr>(e, *expr)); },
                               [](const TypedPattern* p) { return ooze::pretty_print(*p); }},
                    v);
}

struct Unifier {
  auto unify(const Floating&, const Floating&) { return std::optional(floating_type<TypeID>()); }

  template <typename T>
  std::optional<CompoundType<TypeID>> unify(const Floating&, const T& other) {
    return std::optional(CompoundType<TypeID>{other});
  }

  template <typename T>
  std::optional<CompoundType<TypeID>> unify(const T& other, const Floating&) {
    return std::optional(CompoundType<TypeID>{other});
  }
  auto unify(const std::vector<CompoundType<TypeID>>& a, const std::vector<CompoundType<TypeID>>& b) {
    if(a.size() != b.size()) return std::optional<CompoundType<TypeID>>();

    std::vector<CompoundType<TypeID>> result;
    result.reserve(a.size());
    for(size_t i = 0; i < a.size(); ++i) {
      if(auto unified_type = unify(a[i], b[i]); unified_type) {
        result.push_back(std::move(*unified_type));
      } else {
        return std::optional<CompoundType<TypeID>>();
      }
    }

    return std::optional(tuple_type(std::move(result)));
  }
  auto unify(const Borrow<TypeID>& a, const Borrow<TypeID>& b) {
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
    return std::optional<CompoundType<TypeID>>();
  }

  template <typename A, typename B>
  std::optional<CompoundType<TypeID>> unify(const A&, const B&) {
    return std::optional<CompoundType<TypeID>>{};
  }

  std::optional<CompoundType<TypeID>> unify(const CompoundType<TypeID>& a, const CompoundType<TypeID>& b) {
    return std::visit([&](const auto& x, const auto& y) { return unify(x, y); }, a.v, b.v);
  }
};

std::optional<CompoundType<TypeID>>
overload_resolution(const Env& e, const std::string& name, const CompoundType<TypeID>& type) {
  if(const auto it = e.functions.find(name); it == e.functions.end()) {
    return std::nullopt;
  } else {
    std::optional<CompoundType<TypeID>> result;

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

CompoundType<TypeID> propagated_type(Propagation p, bool wrap, CompoundType<TypeID> t) {
  return std::visit(
    Overloaded{[&](DirectProp) { return std::move(t); },
               [&](TupleProp p) {
                 if(wrap) {
                   std::vector<CompoundType<TypeID>> v(p.size, floating_type<TypeID>());
                   v[p.idx] = std::move(t);
                   return tuple_type(std::move(v));
                 } else {
                   auto* v = std::get_if<std::vector<CompoundType<TypeID>>>(&t.v);
                   assert(v);
                   assert(p.idx < v->size());
                   return std::move((*v)[p.idx]);
                 }
               },
               [&](BorrowProp) {
                 if(wrap) {
                   return borrow_type(std::move(t));
                 } else {
                   auto* b = std::get_if<Borrow<TypeID>>(&t.v);
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

std::deque<std::pair<Variable, CompoundType<TypeID>>>
create_initial_candidates(const Env& e, const TypedFunction& f, std::optional<FunctionType<TypeID>> type_hint) {
  std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates;

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

ContextualError generate_error(const Env& e, const TypeCheckError& error, const CompoundType<TypeID>& type) {
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
        const auto type_complexity = [](const CompoundType<TypeID>& t) {
          return knot::preorder_accumulate(t, 0, [](int acc, const CompoundType<TypeID>&) { return acc + 1; });
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

std::pair<Map<Variable, CompoundType<TypeID>>, std::vector<TypeCheckError>> constraint_propagation(
  const Env& e,
  const Map<Variable, std::vector<VariablePropagation>>& propagations,
  const std::vector<const TypedExpr*> undeclared_bindings,
  std::deque<std::pair<Variable, CompoundType<TypeID>>> to_visit,
  bool debug = false) {
  Map<Variable, CompoundType<TypeID>> types;
  Map<Variable, CompoundType<TypeID>> conflicting_types;

  if(debug) {
    fmt::print("Initial queue {}\n", to_visit.size());
    for(const auto& [var, type] : to_visit) {
      fmt::print("  {:>7} {:>10}: {}\n",
                 var.index() == 0 ? "expr" : "pattern",
                 pretty_print(e, var),
                 pretty_print(untype<CompoundType<NamedType>>(e, type)));
    }
  }

  while(!to_visit.empty()) {
    auto [target, type] = std::move(to_visit.front());
    to_visit.pop_front();

    auto it = types.find(target);
    if(it == types.end()) {
      it = types.emplace(target, floating_type<TypeID>()).first;
    }

    const CompoundType<TypeID> original_type = it->second;

    if(debug) {
      fmt::print("Processing {} {}\n   propagated {}\n   existing   {}\n",
                 target.index() == 0 ? "expr" : "pattern",
                 pretty_print(e, target),
                 pretty_print(untype<CompoundType<NamedType>>(e, type)),
                 pretty_print(untype<CompoundType<NamedType>>(e, original_type)));
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
      fmt::print("   final      {}\n", pretty_print(untype<CompoundType<NamedType>>(e, it->second)));
    }

    if(const auto pit = propagations.find(target); pit != propagations.end() && original_type != it->second) {
      for(const auto [dst_target, wrap, propagation] : pit->second) {
        CompoundType<TypeID> dst_type = propagated_type(propagation, wrap, it->second);

        if(dst_type != floating_type<TypeID>()) {
          if(debug) {
            fmt::print("    p {} : {} {}\n",
                       pretty_print(untype<CompoundType<NamedType>>(e, dst_type)),
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
      fmt::print("    {:<10} {}\n", pretty_print(e, target), pretty_print(untype<CompoundType<NamedType>>(e, type)));
    }

    fmt::print("Conflicting types {}\n", conflicting_types.size());
    for(const auto& [target, type] : conflicting_types) {
      fmt::print("    {:<10} -> {}\n", pretty_print(e, target), pretty_print(untype<CompoundType<NamedType>>(e, type)));
    }
  }

  return {std::move(types), transform_to_vec(std::move(conflicting_types), [](auto&& p) {
            return TypeCheckError{MismatchedType{std::move(p.second)}, p.first};
          })};
}

std::vector<TypeCheckError> find_returned_borrows(const TypedFunction& f) {
  std::vector<TypeCheckError> errors;

  knot::preorder(
    f.expr,
    Overloaded{[&](const TypedExpr& e) {
                 const bool has_borrow =
                   knot::preorder_accumulate(e.type, false, [](bool, const Borrow<TypeID>&) { return true; });

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

  const auto check_borrowed_type = [&](Variable var, const CompoundType<TypeID>& t) {
    knot::visit(
      t.v,
      Overloaded{
        [&](const std::vector<CompoundType<TypeID>>&) {
          errors.push_back({InvalidBorrow{"tuple"}, var});
        },
        [&](const FunctionType<TypeID>&) {
          errors.push_back({InvalidBorrow{"function"}, var});
        },
        [&](const Borrow<TypeID>&) {
          errors.push_back({InvalidBorrow{"borrow"}, var});
        },
      });
  };

  // TODO allow errors to be associated with types, not just patterns and exprs
  co_visit(
    f.pattern, f.pattern.type, [&](const TypedPattern& p, const CompoundType<TypeID>& t, const auto&, const auto&) {
      knot::visit(t.v, [&](const Borrow<TypeID>& b) { check_borrowed_type(&p, *b.type); });
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

template <typename T>
ContextualResult<T> apply_language_rules(const Env& env, T t) {
  const auto floating_tuple_type = [](size_t size) {
    return tuple_type(std::vector<CompoundType<TypeID>>(size, floating_type<TypeID>()));
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
  const auto check = [&](auto& ele, const CompoundType<TypeID>& lang_type) {
    if(auto unified = unify_types(lang_type, ele.type); unified) {
      ele.type = std::move(*unified);
    } else {
      errors.push_back(
        {ele.ref, fmt::format("expected {}, given {}", pretty_print(env, lang_type), pretty_print(env, ele.type))});
    }
  };

  knot::preorder(
    t,
    Overloaded{[&](TypedExpr& expr) {
                 check(expr, language_type(expr));
                 knot::visit(
                   expr.v,
                   Overloaded{[&](TypedCallExpr& call) {
                                check(*call.callee,
                                      function_type<TypeID>(floating_type<TypeID>(), floating_type<TypeID>()));
                              },
                              [&](TypedSelectExpr& select) { check(*select.condition, leaf_type(type_id<bool>())); }});
               },
               [&](TypedPattern& pattern) { check(pattern, language_type(pattern)); }

    });

  return value_or_errors(std::move(t), std::move(errors));
}

} // namespace

std::vector<const TypedExpr*> undeclared_bindings(const TypedFunction& f) {
  return std::get<2>(calculate_propagations(f));
}

std::optional<CompoundType<TypeID>> unify_types(const CompoundType<TypeID>& a, const CompoundType<TypeID>& b) {
  return Unifier{}.unify(a, b);
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

ContextualResult<TypedPattern> type_check(const Env& e, TypedPattern pattern, CompoundType<TypeID> type) {
  return apply_language_rules(e, std::move(pattern)).and_then([&](TypedPattern pattern) {
    Map<Variable, std::vector<VariablePropagation>> propagations;
    knot::preorder(pattern, [&](const TypedPattern& pattern) {
      knot::visit(pattern.v, [&](const std::vector<TypedPattern>& tuple) { add_tuple(propagations, tuple, &pattern); });
    });

    std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates;
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
    if(knot::preorder_accumulate(v.type, false, [](bool, Floating) { return true; })) {
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

} // namespace ooze
