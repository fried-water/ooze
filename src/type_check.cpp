#include "pch.h"

#include "ooze/tree.h"
#include "pretty_print.h"
#include "type_check.h"

#include <deque>

namespace ooze {

using namespace ast;

namespace {

using Variable = std::variant<const TypedExpr*, const Pattern*>;

struct MismatchedType {
  CompoundType<TypeID> conflicting_type;
};
struct UnableToDeduce {};

struct MissingOverload {
  std::vector<FunctionType<TypeID>> candidates;
};

struct AmbiguousOverload {
  std::vector<FunctionType<TypeID>> candidates;
};

struct UndeclaredFunction {};
struct UndeclaredBinding {};

struct ReturnBorrow {};
struct InvalidBorrow {
  std::string type;
};
struct UnusedBinding {};

// Ordered by priority
using TypeCheckErrorVariant = std::variant<UndeclaredFunction,
                                           UndeclaredBinding,
                                           MissingOverload,
                                           AmbiguousOverload,
                                           MismatchedType,
                                           UnableToDeduce,
                                           InvalidBorrow,
                                           ReturnBorrow,
                                           UnusedBinding>;

struct TypeCheckError {
  TypeCheckErrorVariant type;
  Variable variable;
};

template <typename T>
using TypeCheckResult = Result<T, std::vector<TypeCheckError>>;

enum class PropDirection { Input, Output, Across };

struct VariablePropagation {
  Variable target = {};
  PropDirection dir;
  // If propagating input this is the src index otherwise its the destination idx
  int idx = 0;
};

std::string pretty_print(const Env& e, Variable v) {
  return std::visit(Overloaded{[&](const TypedExpr* expr) { return pretty_print(untype<UnTypedExpr>(e, *expr)); },
                               [](const Pattern* p) { return ooze::pretty_print(*p); }},
                    v);
}

CompoundType<TypeID> floating_tuple_type(size_t size) {
  return tuple_type(std::vector<CompoundType<TypeID>>(size, floating_type<TypeID>()));
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

std::optional<CompoundType<TypeID>> unify_types(const CompoundType<TypeID>& a, const CompoundType<TypeID>& b) {
  return Unifier{}.unify(a, b);
}

std::optional<std::vector<std::pair<int, CompoundType<TypeID>>>>
overload_resolution(const Env& e, const std::string& name, const CompoundType<TypeID>& type) {
  if(const auto it = e.functions.find(name); it == e.functions.end()) {
    return std::nullopt;
  } else {
    std::vector<std::pair<int, CompoundType<TypeID>>> results;

    for(size_t i = 0; i < it->second.size(); i++) {
      if(auto opt = std::visit([&](const auto& t) { return Unifier{}.unify(it->second[i].type, t); }, type.v); opt) {
        results.emplace_back(i, std::move(*opt));
      }
    }

    return results;
  }
}

CompoundType<TypeID> output_type(const CompoundType<TypeID>& t) {
  return std::visit(Overloaded{[](const FunctionType<TypeID>& t) { return *t.output; },
                               [](const auto& t) { return CompoundType<TypeID>{t}; }},
                    t.v);
}

CompoundType<TypeID> derive_function_type(const TypedCallExpr& call,
                                          const Map<Variable, CompoundType<TypeID>>& types,
                                          const CompoundType<TypeID>& result_type) {
  return function_type(tuple_type(transform_to_vec(call.parameters,
                                                   [&](const TypedExpr& expr) {
                                                     const auto it = types.find(&expr);
                                                     return it != types.end() ? output_type(it->second)
                                                                              : floating_type<TypeID>();
                                                   })),
                       result_type);
}

CompoundType<TypeID> find_src(Variable target, PropDirection dir, int idx, CompoundType<TypeID> t) {
  return std::visit(
    [&](const auto* ptr) {
      return std::visit(Overloaded{[&](const std::vector<TypedExpr>&) {
                                     auto* v = std::get_if<std::vector<CompoundType<TypeID>>>(&t.v);
                                     assert(v);
                                     assert(dir != PropDirection::Input || idx < v->size());
                                     return dir == PropDirection::Input ? std::move((*v)[idx]) : std::move(t);
                                   },
                                   [&](const std::vector<Pattern>&) {
                                     auto* v = std::get_if<std::vector<CompoundType<TypeID>>>(&t.v);
                                     assert(v);
                                     assert(dir != PropDirection::Input || idx < v->size());
                                     return dir == PropDirection::Input ? std::move((*v)[idx]) : std::move(t);
                                   },
                                   [&](const TypedCallExpr&) {
                                     auto* f = std::get_if<FunctionType<TypeID>>(&t.v);
                                     assert(f);
                                     if(dir == PropDirection::Input) {
                                       auto* f_input = std::get_if<std::vector<CompoundType<TypeID>>>(&f->input->v);
                                       assert(f_input && idx < f_input->size());
                                       return std::move((*f_input)[idx]);
                                     } else {
                                       return std::move(*f->output);
                                     }
                                   },
                                   [&](const TypedBorrowExpr&) {
                                     if(dir == PropDirection::Input) {
                                       auto* ref = std::get_if<Borrow<TypeID>>(&t.v);
                                       assert(ref);
                                       return std::move(*ref->type);
                                     } else {
                                       return std::move(t);
                                     }
                                   },
                                   [&](const auto&) { return std::move(t); }},
                        ptr->v);
    },
    target);
}

CompoundType<TypeID> find_dst(Variable dst_target, PropDirection dir, int idx, CompoundType<TypeID> src_type) {
  const auto partial_tuple = [](size_t size, int idx, CompoundType<TypeID> t) {
    std::vector<CompoundType<TypeID>> v(size, floating_type<TypeID>());
    v[idx] = std::move(t);
    return tuple_type(std::move(v));
  };

  return std::visit(
    [&](const auto* ptr) {
      return std::visit(
        Overloaded{[&](const std::vector<TypedExpr>& tuple) {
                     return dir != PropDirection::Output ? std::move(src_type)
                                                         : partial_tuple(tuple.size(), idx, std::move(src_type));
                   },
                   [&](const std::vector<Pattern>& tuple) {
                     return dir != PropDirection::Output ? std::move(src_type)
                                                         : partial_tuple(tuple.size(), idx, std::move(src_type));
                   },
                   [&](const TypedCallExpr& call) {
                     return dir != PropDirection::Output
                              ? function_type(floating_tuple_type(call.parameters.size()), std::move(src_type))
                              : function_type(partial_tuple(call.parameters.size(), idx, std::move(src_type)),
                                              floating_type<TypeID>());
                   },
                   [&](const TypedBorrowExpr&) {
                     return dir == PropDirection::Output ? borrow_type(std::move(src_type)) : std::move(src_type);
                   },
                   [&](const auto&) { return std::move(src_type); }},
        ptr->v);
    },
    dst_target);
}

template <typename B, typename T>
void add_pair(Map<Variable, std::vector<VariablePropagation>>& propagations,
              const B* bottom,
              const T* top,
              int idx = 0,
              bool across = false) {
  propagations[top].push_back({bottom, across ? PropDirection::Across : PropDirection::Output, idx});
  propagations[bottom].push_back({top, across ? PropDirection::Across : PropDirection::Input, idx});
}

template <typename B, typename T>
void add_tuple(Map<Variable, std::vector<VariablePropagation>>& propagations,
               const std::vector<B>& tuple,
               const T* parent) {
  int i = 0;
  for(const auto& child : tuple) {
    add_pair(propagations, parent, &child, i++);
  }
}

struct PropagationVisitor {
  std::vector<TypeCheckError> undeclared_identifiers;
  std::vector<Map<std::string, const Pattern*>> binding_names = {{}};
  Map<Variable, std::vector<VariablePropagation>> propagations;

  void visit(const Pattern& pattern) {
    knot::preorder(pattern, [&](const Pattern& pattern) {
      knot::visit(pattern.v,
                  Overloaded{[&](const Ident& ident) { binding_names.back()[ident.name] = &pattern; },
                             [&](const std::vector<Pattern>& tuple) { add_tuple(propagations, tuple, &pattern); }});
    });
  }

  void visit(const TypedExpr& expr) {
    knot::preorder(expr, [&](const TypedExpr& expr) {
      knot::visit(expr.v,
                  Overloaded{[&](const TypedCallExpr& call) { add_tuple(propagations, call.parameters, &expr); },
                             [&](const std::vector<TypedExpr>& tuple) { add_tuple(propagations, tuple, &expr); },
                             [&](const TypedBorrowExpr& b) { add_pair(propagations, &expr, b.expr.get()); },
                             [&](const IdentExpr& ident) {
                               const auto sit =
                                 std::find_if(binding_names.rbegin(), binding_names.rend(), [&](const auto& map) {
                                   return map.find(ident.name) != map.end();
                                 });
                               if(sit != binding_names.rend()) {
                                 add_pair(propagations, &expr, sit->at(ident.name));
                               } else {
                                 undeclared_identifiers.push_back({UndeclaredBinding{}, &expr});
                               }
                             },
                             [&](const TypedScopeExpr& scope) {
                               binding_names.emplace_back();

                               add_pair(propagations, &expr, scope.result.get());

                               for(const TypedAssignment& a : scope.assignments) {
                                 visit(*a.expr);
                                 visit(a.pattern);
                                 add_pair(propagations, &a.pattern, a.expr.get(), 0, true);
                               }

                               visit(*scope.result);

                               binding_names.pop_back();
                             }});

      return !std::holds_alternative<TypedScopeExpr>(expr.v);
    });
  }
};

std::pair<Map<Variable, std::vector<VariablePropagation>>, std::vector<TypeCheckError>>
calculate_propagations(const TypedFunction& f) {
  PropagationVisitor v;

  v.visit(f.header.pattern);
  v.visit(f.expr);

  return {std::move(v.propagations), std::move(v.undeclared_identifiers)};
}

template <typename T>
std::deque<std::pair<Variable, CompoundType<TypeID>>>
append_pattern_candidates(const T& root, std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates) {
  knot::preorder(root, [&](const Pattern& pattern) {
    candidates.emplace_back(
      &pattern,
      std::visit(Overloaded{[&](const std::vector<Pattern>& v) { return floating_tuple_type(v.size()); },
                            [&](const auto&) { return floating_type<TypeID>(); }},
                 pattern.v));
  });

  return candidates;
}

std::deque<std::pair<Variable, CompoundType<TypeID>>>
create_initial_candidates(const Env& e, const TypedFunction& f, std::optional<FunctionType<TypeID>> type_hint) {
  std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates;

  // Push types according to language rules
  knot::preorder(f.expr, [&](const TypedExpr& expr) {
    candidates.emplace_back(
      &expr,
      std::visit(
        Overloaded{[&](const std::vector<TypedExpr>& v) { return floating_tuple_type(v.size()); },
                   [&](const TypedScopeExpr& v) { return floating_type<TypeID>(); },
                   [&](const Literal& l) {
                     return leaf_type(std::visit(
                       [](const auto& ele) { return anyf::type_id(knot::decay(knot::Type<decltype(ele)>{})); }, l));
                   },
                   [&](const TypedCallExpr& c) { return derive_function_type(c, {}, floating_type<TypeID>()); },
                   [&](const TypedBorrowExpr&) { return borrow_type(floating_type<TypeID>()); },
                   [&](const IdentExpr&) { return floating_type<TypeID>(); }},
        expr.v));
  });

  candidates = append_pattern_candidates(f, std::move(candidates));

  knot::preorder(f.expr, [&](const TypedAssignment& assign) { candidates.emplace_back(&assign.pattern, assign.type); });

  candidates.emplace_back(&f.header.pattern, *f.header.type.input);
  candidates.emplace_back(&f.expr, find_dst(&f.expr, PropDirection::Input, 0, *f.header.type.output));

  if(type_hint) {
    candidates.emplace_back(&f.header.pattern, std::move(*type_hint->input));
    candidates.emplace_back(&f.expr, find_dst(&f.expr, PropDirection::Input, 0, std::move(*type_hint->output)));
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
        const std::string_view fn_name =
          std::visit(Overloaded{[](const TypedCallExpr& call) { return std::string_view(call.function.name); },
                                [](const auto&) { return std::string_view(""); }},
                     var->v);
        return ContextualError{
          var->ref,
          fmt::format(
            "expected {}{}, given {}{}", fn_name, pretty_print(e, type), fn_name, pretty_print(e, m.conflicting_type))};
      },
      [&](const UnableToDeduce&, const auto* var) {
        return ContextualError{var->ref,
                               fmt::format("unable to fully deduce type, deduced: {}", pretty_print(e, type))};
      },
      [&](const MissingOverload& o, const TypedExpr* expr) {
        const std::string& name = std::get<TypedCallExpr>(expr->v).function.name;
        return ContextualError{
          expr->ref,
          "no matching overload found",
          transform_to_vec(
            o.candidates,
            [&](const FunctionType<TypeID>& t) { return fmt::format("  {}{}", name, pretty_print(e, t)); },
            make_vector(
              fmt::format("deduced {}{} [{} candidate(s)]", name, pretty_print(e, type), o.candidates.size())))};
      },
      [&](const AmbiguousOverload& o, const TypedExpr* expr) {
        const std::string& name = std::get<TypedCallExpr>(expr->v).function.name;
        return ContextualError{
          expr->ref,
          "function call is ambiguous",
          transform_to_vec(
            o.candidates,
            [&](const FunctionType<TypeID>& t) { return fmt::format("  {}{}", name, pretty_print(e, t)); },
            make_vector(
              fmt::format("deduced {}{} [{} candidate(s)]", name, pretty_print(e, type), o.candidates.size())))};
      },
      [](const UndeclaredFunction&, const TypedExpr* expr) {
        const std::string& name = std::get<TypedCallExpr>(expr->v).function.name;
        return ContextualError{expr->ref, fmt::format("use of undeclared function '{}'", name)};
      },
      [](const UndeclaredBinding&, const TypedExpr* expr) {
        return ContextualError{expr->ref,
                               fmt::format("use of undeclared binding '{}'", std::get<IdentExpr>(expr->v).name)};
      },
      [](const ReturnBorrow&, const TypedExpr* expr) {
        return ContextualError{expr->ref, "cannot return a borrowed value"};
      },
      [](const InvalidBorrow& b, const auto* var) {
        return ContextualError{var->ref, fmt::format("cannot borrow a {}", b.type)};
      },
      [](const UnusedBinding&, const Pattern* p) {
        return ContextualError{p->ref,
                               fmt::format("unused binding '{}'", std::get<Ident>(p->v).name),
                               {"prefix with an _ to silence this error"}};
      },
      [](const auto&, const auto* var) {
        assert(false);
        return ContextualError{};
      }},
    error.type,
    error.variable);
}

std::vector<ContextualError> generate_errors(const Env& e,
                                             std::vector<TypeCheckError> errors,
                                             const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                             const Map<Variable, CompoundType<TypeID>>& types) {
  const std::vector<std::vector<TypeCheckError>> error_clusters = cluster_adjacent(e, propagations, std::move(errors));

  // Find most *relevant* error per cluster
  std::vector<ContextualError> final_errors =
    transform_to_vec(error_clusters, [&](const std::vector<TypeCheckError>& group) {
      const auto projection = [&](const TypeCheckError& error) {
        const Slice ref = std::visit([](const auto* p) { return p->ref; }, error.variable);
        return std::tuple(error.type.index(), ref.end, ref.begin);
        ;
      };

      const TypeCheckError& most_relevant_error =
        *std::min_element(group.begin(), group.end(), [&](const auto& lhs, const auto& rhs) {
          return projection(lhs) < projection(rhs);
        });

      return generate_error(e, most_relevant_error, types.at(most_relevant_error.variable));
    });

  return sorted(std::move(final_errors), [](const auto& e) { return std::tie(e.ref.end, e); });
}

std::pair<Map<Variable, CompoundType<TypeID>>, std::vector<TypeCheckError>>
constraint_propagation(const Env& e,
                       const Map<Variable, std::vector<VariablePropagation>>& propagations,
                       std::deque<std::pair<Variable, CompoundType<TypeID>>> to_visit,
                       bool debug = false) {
  Map<Variable, CompoundType<TypeID>> types;
  Map<Variable, CompoundType<TypeID>> conflicting_types;

  if(debug) {
    fmt::print("Initial queue size {}\n", to_visit.size());
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
    knot::visit(target, [&](const auto* t) {
      knot::visit(t->v, Overloaded{[&](const TypedCallExpr& call) {
                    if(auto overloads = overload_resolution(e, call.function.name, it->second);
                       overloads && overloads->size() == 1) {
                      it->second = std::move(overloads->front().second);
                    }
                  }});
    });

    if(debug) {
      fmt::print("   final      {}\n", pretty_print(untype<CompoundType<NamedType>>(e, it->second)));
    }

    if(const auto pit = propagations.find(target); pit != propagations.end() && original_type != it->second) {
      for(const auto [dst_target, propagate_dir, idx] : pit->second) {
        CompoundType<TypeID> src_type = find_src(target, propagate_dir, idx, it->second);

        if(src_type != floating_type<TypeID>()) {
          CompoundType<TypeID> dst_type = find_dst(dst_target, propagate_dir, idx, src_type);

          if(debug) {
            fmt::print("    p src_type {} dst_type {} -> {} {}\n",
                       pretty_print(untype<CompoundType<NamedType>>(e, src_type)),
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

TypeCheckResult<CheckedFunction> find_overloads(const Env& e,
                                                const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                                const TypedFunction& f,
                                                const Map<Variable, CompoundType<TypeID>>& types) {
  Map<const NamedFunction*, EnvFunctionRef> all_overloads;
  std::vector<TypeCheckError> errors;

  knot::preorder(f.expr, [&](const TypedExpr& expr) {
    if(const auto* call = std::get_if<TypedCallExpr>(&expr.v); call) {
      const auto overloads = overload_resolution(e, call->function.name, types.at(&expr));

      if(overloads && overloads->size() == 1) {
        all_overloads.emplace(&call->function,
                              EnvFunctionRef{call->function.name,
                                             overloads->front().first,
                                             std::get<FunctionType<TypeID>>(types.at(&expr).v)});
      } else if(overloads) {
        errors.push_back(
          overloads->empty()
            ? TypeCheckError{MissingOverload{transform_to_vec(e.functions.at(call->function.name),
                                                              [](const EnvFunction& f) { return f.type; })},
                             &expr}
            : TypeCheckError{AmbiguousOverload{transform_to_vec(
                               *overloads, [](const auto& p) { return std::get<FunctionType<TypeID>>(p.second.v); })},
                             &expr});
      } else {
        return errors.push_back(TypeCheckError{UndeclaredFunction{}, &expr});
      }
    }
  });

  return errors.empty() ? TypeCheckResult<CheckedFunction>{knot::map<CheckedFunction>(
                            f, [&](const NamedFunction& f) { return all_overloads.at(&f); })}
                        : Failure{std::move(errors)};
}

std::vector<TypeCheckError> find_returned_borrows(const TypedFunction& f,
                                                  const Map<Variable, CompoundType<TypeID>>& types) {
  std::vector<TypeCheckError> errors;

  knot::preorder(f.expr,
                 Overloaded{[&](const TypedExpr& e) {
                              const bool has_borrow = knot::preorder_accumulate(
                                output_type(types.at(&e)), false, [](bool, const Borrow<TypeID>&) { return true; });

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

std::vector<TypeCheckError> find_invalid_borrows(const TypedFunction& f,
                                                 const Map<Variable, CompoundType<TypeID>>& types) {
  std::vector<TypeCheckError> errors;

  const auto check_borrowed_type = [&](Variable var, const CompoundType<TypeID>& t) {
    knot::visit(t.v,
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
  co_visit(f.header.pattern,
           *f.header.type.input,
           [&](const Pattern& p, const CompoundType<TypeID>& t, const auto&, const auto&) {
             knot::visit(t.v, [&](const Borrow<TypeID>& b) { check_borrowed_type(&p, *b.type); });
           });

  knot::preorder(f.expr, [&](const TypedExpr& e) {
    knot::visit(e.v, [&](const TypedBorrowExpr& b) { check_borrowed_type(&e, types.at(b.expr.get())); });
  });

  return errors;
}

std::vector<TypeCheckError> find_unused_bindings(const TypedFunction& f,
                                                 const Map<Variable, std::vector<VariablePropagation>>& propagations) {
  std::vector<TypeCheckError> errors;

  knot::preorder(f, [&](const Pattern& p) {
    knot::visit(p.v, [&](const Ident& i) {
      const auto& props = propagations.at(&p);
      const auto uses = std::count_if(props.begin(), props.end(), [](const auto& p) {
        return p.dir == PropDirection::Output && std::holds_alternative<const TypedExpr*>(p.target);
      });

      if(uses == 0 && i.name[0] != '_') {
        errors.push_back({UnusedBinding{}, &p});
      }
    });
  });

  return errors;
}

std::vector<TypeCheckError> find_unresolved_errors(const Map<Variable, CompoundType<TypeID>>& types) {
  return transform_filter_to_vec(types, [](const auto& p) {
    return knot::preorder_accumulate(p.second, false, [](bool, Floating) { return true; })
             ? std::optional(TypeCheckError{UnableToDeduce{}, p.first})
             : std::nullopt;
  });
}

struct TypeNameResolution {
  const Env& e;
  std::vector<ContextualError>* errors;

  CompoundType<TypeID> operator()(const CompoundType<NamedType>& type) {
    return std::visit(
      Overloaded{[&](const NamedType& named) {
                   if(const auto it = e.type_ids.find(named.name); it != e.type_ids.end()) {
                     return CompoundType<TypeID>{it->second, type.ref};
                   } else {
                     errors->push_back({type.ref, "undefined type"});
                     return CompoundType<TypeID>{TypeID{}, type.ref};
                   }
                 },
                 [&](const std::vector<CompoundType<NamedType>>& v) {
                   return CompoundType<TypeID>{knot::map<std::vector<CompoundType<TypeID>>>(v, *this), type.ref};
                 },
                 [&](const FunctionType<NamedType>& f) {
                   return CompoundType<TypeID>{knot::map<FunctionType<TypeID>>(f, *this), type.ref};
                 },
                 [&](const Floating&) { return floating_type<TypeID>(type.ref); },
                 [&](const Borrow<NamedType>& b) {
                   return CompoundType<TypeID>{knot::map<Borrow<TypeID>>(b, *this), type.ref};
                 }},
      type.v);
  }
};

template <typename Typed, typename Untyped>
ContextualResult<Typed> type_name_resolution(const Env& e, const Untyped& u) {
  std::vector<ContextualError> errors;
  auto typed = knot::map<Typed>(u, TypeNameResolution{e, &errors});
  return value_or_errors(std::move(typed), std::move(errors));
}

struct InferHeaderCtx {
  std::vector<Set<std::string>> active;
  std::vector<std::pair<std::string, Slice>> args;
};

InferHeaderCtx inferred_header(InferHeaderCtx ctx, const TypedExpr& expr) {
  return std::visit(
    Overloaded{
      [&](const std::vector<TypedExpr>& tuple) { return knot::accumulate(tuple, std::move(ctx), inferred_header); },
      [&](const TypedScopeExpr& scope) {
        ctx.active.emplace_back();

        for(const TypedAssignment& assign : scope.assignments) {
          ctx = inferred_header(std::move(ctx), *assign.expr);
          knot::preorder(assign.pattern, [&](const Ident& i) { ctx.active.back().insert(i.name); });
        }

        ctx = inferred_header(std::move(ctx), *scope.result);

        ctx.active.pop_back();

        return std::move(ctx);
      },
      [&](const TypedBorrowExpr& borrow) { return inferred_header(std::move(ctx), *borrow.expr); },
      [&](const TypedCallExpr& call) { return knot::accumulate(call.parameters, std::move(ctx), inferred_header); },
      [&](const Literal&) { return std::move(ctx); },
      [&](const IdentExpr& ident) {
        if(std::all_of(
             ctx.active.begin(), ctx.active.end(), [&](const auto& s) { return s.find(ident.name) == s.end(); })) {
          if(std::find_if(ctx.args.begin(), ctx.args.end(), [&](const auto& p) { return p.first == ident.name; }) ==
             ctx.args.end()) {
            ctx.args.emplace_back(ident.name, expr.ref);
          }
        }

        return std::move(ctx);
      }},
    expr.v);
}

// `f` taken by ptr since `types` requires ptr stability in `f`
void annotate_types(TypedFunction* f, const Map<Variable, CompoundType<TypeID>>& types) {
  *f->header.type.output = output_type(types.at(&f->expr));
  *f->header.type.input = output_type(types.at(&f->header.pattern));

  knot::preorder(f->expr, [&](TypedAssignment& assign) { assign.type = output_type(types.at(assign.expr.get())); });
}

} // namespace

ContextualResult<TypedFunction> type_name_resolution(const Env& e, const UnTypedFunction& f) {
  return type_name_resolution<TypedFunction>(e, std::tie(f.header, f.expr));
}

ContextualResult<TypedHeader> type_name_resolution(const Env& e, const UnTypedHeader& h) {
  return type_name_resolution<TypedHeader>(e, h);
}

ContextualResult<TypedExpr> type_name_resolution(const Env& e, const UnTypedExpr& expr) {
  return type_name_resolution<TypedExpr>(e, expr);
}

ContextualResult<TypedAssignment> type_name_resolution(const Env& e, const UnTypedAssignment& a) {
  return type_name_resolution<TypedAssignment>(e, a);
}

ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env& e, const CompoundType<NamedType>& t) {
  return type_name_resolution<CompoundType<TypeID>>(e, t);
}

TypedHeader inferred_header(const TypedExpr& expr) {
  const std::vector<std::pair<std::string, Slice>> args = inferred_header({}, expr).args;
  return {{transform_to_vec(args,
                            [](auto p) {
                              return Pattern{Ident{std::move(p.first)}, p.second};
                            })},
          {{{transform_to_vec(args, [](const auto&) { return floating_type<TypeID>(); })}}, {{Floating{}}}}};
}

ContextualResult<std::variant<CheckedFunction, TypedFunction>>
overload_resolution(const Env& e, TypedFunction f, std::optional<FunctionType<TypeID>> type_hint, bool debug) {
  auto prop_result = calculate_propagations(f);

  const Map<Variable, std::vector<VariablePropagation>> propagations = std::move(prop_result.first);
  const std::vector<TypeCheckError> undeclared_binding_errors = std::move(prop_result.second);

  auto cp_result =
    constraint_propagation(e, propagations, create_initial_candidates(e, f, std::move(type_hint)), debug);

  const Map<Variable, CompoundType<TypeID>> types = std::move(cp_result.first);
  const std::vector<TypeCheckError> cp_errors = std::move(cp_result.second);

  std::vector<TypeCheckError> errors = flatten(undeclared_binding_errors,
                                               cp_errors,
                                               find_returned_borrows(f, types),
                                               find_invalid_borrows(f, types),
                                               find_unused_bindings(f, propagations));

  if(errors.empty()) {
    annotate_types(&f, types);
  }

  TypeCheckResult<std::variant<CheckedFunction, TypedFunction>> result =
    find_unresolved_errors(types).empty()
      ? find_overloads(e, propagations, f, types).map(Construct<std::variant<CheckedFunction, TypedFunction>>{})
      : std::variant<CheckedFunction, TypedFunction>{f /* TODO move? (errors need to be owning) */};

  return result_and_errors(std::move(result), std::move(errors)).map_error([&](std::vector<TypeCheckError> errors) {
    return generate_errors(e, std::move(errors), propagations, types);
  });
}

ContextualResult<CheckedFunction>
overload_resolution_concrete(const Env& e, TypedFunction f, std::optional<FunctionType<TypeID>> type_hint, bool debug) {
  auto prop_result = calculate_propagations(f);

  const Map<Variable, std::vector<VariablePropagation>> propagations = std::move(prop_result.first);
  const std::vector<TypeCheckError> undeclared_binding_errors = std::move(prop_result.second);

  auto cp_result =
    constraint_propagation(e, propagations, create_initial_candidates(e, f, std::move(type_hint)), debug);

  const Map<Variable, CompoundType<TypeID>> types = std::move(cp_result.first);
  const std::vector<TypeCheckError> cp_errors = std::move(cp_result.second);

  std::vector<TypeCheckError> errors = flatten(undeclared_binding_errors,
                                               cp_errors,
                                               find_returned_borrows(f, types),
                                               find_invalid_borrows(f, types),
                                               find_unused_bindings(f, propagations),
                                               find_unresolved_errors(types));

  if(errors.empty()) {
    annotate_types(&f, types);
  }

  return result_and_errors(find_overloads(e, propagations, f, types), std::move(errors))
    .map_error(
      [&](std::vector<TypeCheckError> errors) { return generate_errors(e, std::move(errors), propagations, types); });
}

ContextualResult<CompoundType<TypeID>>
type_check(const Env& e, const ast::Pattern& pattern, CompoundType<TypeID> type) {
  Map<Variable, std::vector<VariablePropagation>> propagations;
  knot::preorder(pattern, [&](const Pattern& pattern) {
    knot::visit(pattern.v, [&](const std::vector<Pattern>& tuple) { add_tuple(propagations, tuple, &pattern); });
  });

  std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates = append_pattern_candidates(pattern, {});
  candidates.emplace_back(&pattern, std::move(type));

  auto [types, cp_errors] = constraint_propagation(e, propagations, std::move(candidates));
  return value_or_errors(types.at(&pattern), generate_errors(e, std::move(cp_errors), propagations, types));
}

} // namespace ooze
