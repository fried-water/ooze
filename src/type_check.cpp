#include "pch.h"

#include "ooze/tree.h"
#include "queries.h"
#include "type_check.h"

#include <deque>

namespace ooze {

using namespace ast;

namespace {

using Variable = std::variant<const TypedExpr*, const Pattern*>;

enum class PropDirection { Input, Output, Across };

struct VariablePropagation {
  Variable target = {};
  PropDirection dir;
  // If propagating input this is the src index otherwise its the destination idx
  int idx = 0;
};

CompoundType<TypeID> floating_tuple_type(size_t size) {
  return tuple_type(std::vector<CompoundType<TypeID>>(size, floating_type<TypeID>()));
}

std::optional<CompoundType<TypeID>> unify_types(const CompoundType<TypeID>& a, const CompoundType<TypeID>& b) {
  return std::visit(
    Overloaded{[](const Floating&, const Floating&) { return std::optional(floating_type<TypeID>()); },
               [](const Floating&, const auto& other) { return std::optional(CompoundType<TypeID>{other}); },
               [](const auto& other, const Floating&) { return std::optional(CompoundType<TypeID>{other}); },
               [](const std::vector<CompoundType<TypeID>>& a, const std::vector<CompoundType<TypeID>>& b) {
                 if(a.size() != b.size()) return std::optional<CompoundType<TypeID>>();

                 std::vector<CompoundType<TypeID>> result;
                 result.reserve(a.size());
                 for(size_t i = 0; i < a.size(); ++i) {
                   if(auto unified_type = unify_types(a[i], b[i]); unified_type) {
                     result.push_back(std::move(*unified_type));
                   } else {
                     return std::optional<CompoundType<TypeID>>();
                   }
                 }

                 return std::optional(tuple_type(std::move(result)));
               },
               [](const Borrow<TypeID>& a, const Borrow<TypeID>& b) {
                 auto unified = unify_types(*a.type, *b.type);
                 return unified ? std::optional(borrow_type(std::move(*unified))) : std::nullopt;
               },
               [](const TypeID& a, const TypeID& b) { return a == b ? std::optional(leaf_type(a)) : std::nullopt; },
               [](const FunctionType<TypeID>& f1, const FunctionType<TypeID>& f2) {
                 if(auto input = unify_types(*f1.input, *f2.input); input) {
                   if(auto output = unify_types(*f1.output, *f2.output); output) {
                     return std::optional(function_type(std::move(*input), std::move(*output)));
                   }
                 }
                 return std::optional<CompoundType<TypeID>>();
               },
               [](const auto&, const auto&) { return std::optional<CompoundType<TypeID>>{}; }},
    a.v,
    b.v);
}

std::optional<std::vector<std::pair<int, CompoundType<TypeID>>>>
overload_resolution(const Env& e, const std::string& name, const CompoundType<TypeID>& type) {
  if(const auto it = e.functions.find(name); it == e.functions.end()) {
    return std::nullopt;
  } else {
    std::vector<std::pair<int, CompoundType<TypeID>>> results;

    for(size_t i = 0; i < it->second.size(); i++) {
      if(auto opt = unify_types(it->second[i].type, type); opt) {
        results.emplace_back(i, std::move(*opt));
      }
    }

    return results;
  }
}

ContextualResult<int>
overload_resolution(const Env& e, Slice ref, const std::string& name, const CompoundType<TypeID>& type) {
  const auto overloads = overload_resolution(e, name, type);

  if(overloads && overloads->size() == 1) {
    return overloads->front().first;
  } else if(overloads) {
    const auto& functions = e.functions.at(name);
    return tl::unexpected{make_vector(ContextualError{
      ref,
      overloads->empty() ? "no matching overload found" : "function call is ambiguous",
      overloads->empty()
        ? transform_to_vec(
            functions,
            [&](const EnvFunction& f) { return fmt::format("  {}{}", name, type_string(e, f.type)); },
            make_vector(fmt::format("deduced {}{} [{} candidate(s)]", name, type_string(e, type), functions.size())))
        : transform_to_vec(
            *overloads,
            [&](const auto& p) { return fmt::format("  {}{}", name, type_string(e, functions[p.first].type)); },
            make_vector(
              fmt::format("deduced {}{} [{} candidate(s)]", name, type_string(e, type), overloads->size())))})};
  } else {
    return tl::unexpected{make_vector(ContextualError{ref, fmt::format("use of undeclared function '{}'", name)})};
  }
}

struct TypeCheckError {
  std::vector<std::pair<const TypedExpr*, const ast::Pattern*>> pattern_mismatches;
  std::vector<const TypedExpr*> undeclared_identifiers;
};

CompoundType<TypeID> output_type(const CompoundType<TypeID>& t) {
  return std::visit(Overloaded{[](const FunctionType<TypeID>& t) { return *t.output; },
                               [](const auto& t) { return CompoundType<TypeID>{t}; }},
                    t.v);
}

CompoundType<TypeID> derive_function_type(const Call<NamedFunction>& call,
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
                                   [&](const Call<NamedFunction>&) {
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
                                   [&](const BorrowExpr<NamedFunction>&) {
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
                   [&](const Call<NamedFunction>& call) {
                     return dir != PropDirection::Output
                              ? function_type(floating_tuple_type(call.parameters.size()), std::move(src_type))
                              : function_type(partial_tuple(call.parameters.size(), idx, std::move(src_type)),
                                              floating_type<TypeID>());
                   },
                   [&](const BorrowExpr<NamedFunction>&) {
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

ContextualResult<Map<Variable, std::vector<VariablePropagation>>> calculate_propagations(const TypedFunction& f) {
  std::vector<const TypedExpr*> undeclared_identifiers;
  Map<std::string, const Pattern*> binding_names;
  Map<Variable, std::vector<VariablePropagation>> propagations;

  const auto visit_pattern = [&](const Pattern& pattern) {
    knot::preorder(pattern, [&](const Pattern& pattern) {
      if(const auto* ident = std::get_if<Ident>(&pattern.v); ident) {
        binding_names[ident->name] = &pattern;
      }
    });
  };

  const auto visit_expr = [&](const TypedExpr& root) {
    knot::preorder(root, [&](const TypedExpr& expr) {
      if(const auto* ident = std::get_if<IdentExpr>(&expr.v); ident) {
        if(const auto it = binding_names.find(ident->name); it != binding_names.end()) {
          add_pair(propagations, &expr, it->second);
        } else {
          undeclared_identifiers.push_back(&expr);
        }
      }
    });
  };

  visit_pattern(f.header.pattern);

  for(const TypedAssignment& assign : f.scope.assignments) {
    visit_expr(assign.expr);
    visit_pattern(assign.pattern);
  }

  visit_expr(f.scope.result);

  for(const TypedAssignment& a : f.scope.assignments) {
    add_pair(propagations, &a.pattern, &a.expr, 0, true);
  }

  knot::preorder(f.scope, [&](const TypedExpr& expr) {
    knot::visit(expr.v,
                Overloaded{[&](const Call<NamedFunction>& call) { add_tuple(propagations, call.parameters, &expr); },
                           [&](const std::vector<TypedExpr>& tuple) { add_tuple(propagations, tuple, &expr); },
                           [&](const BorrowExpr<NamedFunction>& b) { add_pair(propagations, &expr, b.e.get()); }});
  });

  knot::preorder(f, [&](const Pattern& pattern) {
    knot::visit(pattern.v, [&](const std::vector<Pattern>& tuple) { add_tuple(propagations, tuple, &pattern); });
  });

  return value_or_errors(std::move(propagations), transform_to_vec(undeclared_identifiers, [](const TypedExpr* expr) {
                           return ContextualError{
                             expr->ref,
                             fmt::format("use of undeclared binding '{}'", std::get<IdentExpr>(expr->v).name)};
                         }));
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

std::deque<std::pair<Variable, CompoundType<TypeID>>> create_initial_candidates(const Env& e, const TypedFunction& f) {
  std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates;

  // Push types according to language rules
  knot::preorder(f.scope, [&](const TypedExpr& expr) {
    candidates.emplace_back(
      &expr,
      std::visit(
        Overloaded{[&](const std::vector<TypedExpr>& v) { return floating_tuple_type(v.size()); },
                   [&](const Literal& l) { return leaf_type(type_of(l)); },
                   [&](const Call<NamedFunction>& c) { return derive_function_type(c, {}, floating_type<TypeID>()); },
                   [&](const BorrowExpr<NamedFunction>&) { return borrow_type(floating_type<TypeID>()); },
                   [&](const IdentExpr&) { return floating_type<TypeID>(); }},
        expr.v));
  });

  candidates = append_pattern_candidates(f, std::move(candidates));

  for(const auto& assign : f.scope.assignments) {
    candidates.emplace_back(&assign.pattern, assign.type);
  }

  candidates.emplace_back(&f.header.pattern, *f.header.type.input);
  candidates.emplace_back(&f.scope.result, find_dst(&f.scope.result, PropDirection::Input, 0, *f.header.type.output));

  return candidates;
}

std::vector<std::vector<Variable>> cluster_adjacent(const Env& e,
                                                    const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                                    const Set<Variable>& variables) {
  std::vector<std::vector<Variable>> groups;

  Set<Variable> visited;

  for(Variable var : variables) {
    std::vector<Variable> group;
    std::vector<Variable> to_visit{var};
    while(!to_visit.empty()) {
      Variable v = to_visit.back();
      to_visit.pop_back();

      if(visited.insert(v).second) {
        group.push_back(v);
        if(const auto pit = propagations.find(v); pit != propagations.end()) {
          for(const VariablePropagation& p : pit->second) {
            if(variables.find(p.target) != variables.end()) {
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

std::vector<ContextualError> generate_errors(const Env& e,
                                             const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                             const Map<Variable, CompoundType<TypeID>>& types,
                                             const Map<Variable, CompoundType<TypeID>>& conflicting_types) {
  const std::vector<std::vector<Variable>> error_groups = cluster_adjacent(
    e,
    propagations,
    knot::map<Set<Variable>>(conflicting_types,
                             [](const std::pair<const Variable, CompoundType<TypeID>>& p) { return p.first; }));

  std::vector<ContextualError> errors;

  for(const auto& group : error_groups) {
    const auto ref = [](Variable v) { return std::visit([](const auto* p) { return p->ref; }, v); };
    const auto call_name = [](Variable v) {
      return std::visit(Overloaded{[](const Expr<NamedFunction>* e) {
                                     const auto* c = std::get_if<Call<NamedFunction>>(&e->v);
                                     return c ? std::optional(c->function.name) : std::nullopt;
                                   },
                                   [](const Pattern*) { return std::optional<std::string>(); }},
                        v);
    };

    const Variable v = *std::min_element(group.begin(), group.end(), [&](Variable lhs, Variable rhs) {
      return std::tuple(!call_name(lhs), ref(lhs).end) < std::tuple(!call_name(rhs), ref(rhs).end);
    });

    const auto& t1 = types.at(v);
    const auto& t2 = conflicting_types.at(v);

    if(const auto opt_name = call_name(v); opt_name) {
      if(auto r1 = overload_resolution(e, ref(v), *opt_name, t1); !r1.has_value()) {
        errors = to_vec(std::move(r1.error()), std::move(errors));
      } else if(auto r2 = overload_resolution(e, ref(v), *opt_name, t2); !r2.has_value()) {
        errors = to_vec(std::move(r2.error()), std::move(errors));
      } else {
        errors.push_back({ref(v), fmt::format("expected {}, given {}", type_string(e, t1), type_string(e, t2))});
      }
    } else {
      errors.push_back({ref(v), fmt::format("expected {}, given {}", type_string(e, t1), type_string(e, t2))});
    }
  }

  return errors;
}

ContextualResult<Map<Variable, CompoundType<TypeID>>>
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
                 std::visit([](const auto* t) { return pretty_print(*t); }, target),
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
      knot::visit(t->v, Overloaded{[&](const Call<NamedFunction>& call) {
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
                       std::visit([](const auto* t) { return pretty_print(*t); }, dst_target));
          }

          to_visit.emplace_back(dst_target, std::move(dst_type));
        }
      }
    }
  }

  if(debug) {
    fmt::print("Final types\n");
    for(const auto& [target, type] : types) {
      fmt::print("    {:<10} {}\n",
                 std::visit([](const auto* t) { return pretty_print(*t); }, target),
                 pretty_print(untype<CompoundType<NamedType>>(e, type)));
    }

    fmt::print("Conflicting types {}\n", conflicting_types.size());
    for(const auto& [target, type] : conflicting_types) {
      fmt::print("    {:<10} -> {}\n",
                 std::visit([](const auto* t) { return pretty_print(*t); }, target),
                 pretty_print(untype<CompoundType<NamedType>>(e, type)));
    }
  }

  auto conflicting_errors = generate_errors(e, propagations, types, conflicting_types);
  return value_or_errors(std::move(types), std::move(conflicting_errors));
}

ContextualResult<CheckedFunction> find_overloads(const Env& e,
                                                 const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                                 const TypedFunction& f,
                                                 const Map<Variable, CompoundType<TypeID>>& types) {
  Map<const NamedFunction*, int> overloads;
  Map<Variable, std::vector<ContextualError>> variable_errors;
  Set<Variable> failing_variables;

  knot::preorder(f.scope, [&](const TypedExpr& expr) {
    if(const auto* call = std::get_if<Call<NamedFunction>>(&expr.v); call) {
      auto result = overload_resolution(
        e, expr.ref, call->function.name, derive_function_type(*call, types, output_type(types.at(&expr))));
      if(result) {
        overloads.emplace(&call->function, result.value());
      } else {
        variable_errors.emplace(&expr, std::move(result.error()));
        failing_variables.insert(&expr);
      }
    }
  });

  if(failing_variables.empty()) {
    return knot::map<CheckedFunction>(f, [&](const NamedFunction& f) {
      return EnvFunctionRef{f.name, overloads.at(&f)};
    });
  } else {
    std::vector<ContextualError> errors;

    for(const std::vector<Variable>& group : cluster_adjacent(e, propagations, failing_variables)) {
      const auto ref = [](Variable v) { return std::visit([](const auto* p) { return p->ref; }, v); };
      const auto call_name_exists = [&](Variable v) {
        const Expr<NamedFunction>* expr = std::get<const Expr<NamedFunction>*>(v);
        return e.functions.find(std::get<Call<NamedFunction>>(expr->v).function.name) != e.functions.end();
      };

      const Variable v = *std::min_element(group.begin(), group.end(), [&](Variable lhs, Variable rhs) {
        return std::tuple(call_name_exists(lhs), ref(lhs).end) < std::tuple(call_name_exists(rhs), ref(rhs).end);
      });

      errors = to_vec(std::move(variable_errors.at(v)), std::move(errors));
    }

    return tl::unexpected{std::move(errors)};
  }
}

std::vector<ContextualError> extra_checks(const TypedFunction& f,
                                          const Map<Variable, std::vector<VariablePropagation>>& propagations,
                                          const Map<Variable, CompoundType<TypeID>>& types,
                                          bool debug) {
  std::vector<ContextualError> errors;

  knot::preorder(f.scope.result, [&](const Expr<NamedFunction>& e) {
    return std::visit(Overloaded{[&](const std::vector<Expr<NamedFunction>>&) { return true; },
                                 [&](const auto& inner) {
                                   const bool has_borrow =
                                     knot::preorder_accumulate(output_type(types.at(&e)),
                                                               false,
                                                               [](bool, const Borrow<TypeID>&) { return true; });

                                   if(has_borrow) {
                                     errors.push_back({e.ref, "cannot return a borrowed value"});
                                   }
                                   return false;
                                 }},
                      e.v);
  });

  const auto check_borrowed_type = [&](Slice ref, const CompoundType<TypeID>& t) {
    knot::visit(t.v,
                Overloaded{
                  [&](const std::vector<CompoundType<TypeID>>&) {
                    errors.push_back({ref, "cannot borrow a tuple"});
                  },
                  [&](const FunctionType<TypeID>&) {
                    errors.push_back({ref, "cannot borrow a function"});
                  },
                  [&](const Borrow<TypeID>&) {
                    errors.push_back({ref, "cannot borrow a borrow"});
                  },
                });
  };

  knot::preorder(f.header.type, [&](const Borrow<TypeID>& b) { check_borrowed_type(f.header.pattern.ref, *b.type); });

  knot::preorder(f.scope, [&](const TypedExpr& e) {
    knot::visit(e.v, [&](const BorrowExpr<NamedFunction>& b) { check_borrowed_type(e.ref, types.at(b.e.get())); });
  });

  knot::preorder(f, [&](const Pattern& p) {
    knot::visit(p.v, [&](const Ident& i) {
      const auto& props = propagations.at(&p);
      const auto uses = std::count_if(props.begin(), props.end(), [](const auto& p) {
        return p.dir == PropDirection::Output && std::holds_alternative<const TypedExpr*>(p.target);
      });

      if(uses == 0 && i.name[0] != '_') {
        errors.push_back(
          {p.ref, fmt::format("unused binding '{}'", i.name), {"prefix with an _ to silence this error"}});
      }
    });
  });

  return errors;
}

template <typename Typed, typename Untyped>
ContextualResult<Typed> type_name_resolution(const Env& e, const Untyped& u) {
  std::vector<ContextualError> errors;

  auto typed = knot::map<Typed>(u, [&](const NamedType& type) {
    if(const auto it = e.type_ids.find(type.name); it != e.type_ids.end()) {
      return it->second;
    } else {
      errors.push_back({type.ref, "undefined type"});
      return TypeID{};
    }
  });

  return value_or_errors(std::move(typed), std::move(errors));
}

} // namespace

ContextualResult<TypedFunction> type_name_resolution(const Env& e, const UnTypedFunction& f) {
  return type_name_resolution<TypedFunction>(e, std::tie(f.header, f.scope));
}

ContextualResult<TypedHeader> type_name_resolution(const Env& e, const UnTypedHeader& h) {
  return type_name_resolution<TypedHeader>(e, h);
}

ContextualResult<TypedScope> type_name_resolution(const Env& e, const UnTypedScope& b) {
  return type_name_resolution<TypedScope>(e, b);
}

ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env& e, const CompoundType<NamedType>& t) {
  return type_name_resolution<CompoundType<TypeID>>(e, t);
}

TypedHeader inferred_header(const TypedScope& scope) {
  Set<std::string> active;
  std::vector<std::pair<std::string, Slice>> arg_names;

  const auto process_expr = [&](const TypedExpr& expr) {
    knot::preorder(expr, [&](const TypedExpr& inner_expr) {
      knot::visit(inner_expr.v, [&](const IdentExpr& e) {
        if(active.find(e.name) == active.end()) {
          if(std::find_if(arg_names.begin(), arg_names.end(), [&](const auto& p) { return p.first == e.name; }) ==
             arg_names.end()) {
            arg_names.emplace_back(e.name, inner_expr.ref);
          }
        }
      });
    });
  };

  for(const TypedAssignment& assign : scope.assignments) {
    process_expr(assign.expr);
    knot::preorder(assign.pattern, [&](const Ident& i) { active.insert(i.name); });
  }

  process_expr(scope.result);

  return {{transform_to_vec(arg_names,
                            [](auto p) {
                              return Pattern{Ident{std::move(p.first)}, p.second};
                            })},
          {{{transform_to_vec(arg_names, [](const auto&) { return floating_type<TypeID>(); })}}, {{Floating{}}}}};
}

ContextualResult<CheckedFunction> overload_resolution(const Env& e, TypedFunction f, bool debug) {
  return calculate_propagations(f)
    .and_then([&](auto propagations) {
      auto cp_res = constraint_propagation(e, propagations, create_initial_candidates(e, f), debug);
      return merge(std::move(propagations), std::move(cp_res));
    })
    .and_then([&](auto tup) {
      const auto& [propagations, types] = tup;
      std::vector<ContextualError> extra_errors = extra_checks(f, propagations, types, debug);

      *f.header.type.output = output_type(types.at(&f.scope.result));
      *f.header.type.input = output_type(types.at(&f.header.pattern));

      for(auto& assign : f.scope.assignments) {
        assign.type = output_type(types.at(&assign.expr));
      }

      auto overload_result = find_overloads(e, propagations, std::move(f), types);

      if(extra_errors.empty()) {
        return overload_result;
      } else if(!overload_result.has_value()) {
        overload_result.error().insert(overload_result.error().end(),
                                       std::make_move_iterator(extra_errors.begin()),
                                       std::make_move_iterator(extra_errors.end()));
        return overload_result;
      } else {
        return ContextualResult<CheckedFunction>{tl::unexpected{std::move(extra_errors)}};
      }
    })
    .and_then([&](CheckedFunction f) {
      std::vector<ContextualError> errors;
      const auto append_errors = [&](const auto& pe, const CompoundType<TypeID>& t, const auto&, const auto&) {
        if(knot::preorder_accumulate(t, false, [](bool, Floating) { return true; })) {
          errors.push_back({pe.ref,
                            fmt::format("unable to fully deduce type, deduced: {}",
                                        pretty_print(untype<CompoundType<NamedType>>(e, t)))});
        }
      };

      co_visit(f.header.pattern, *f.header.type.input, append_errors);

      for(const auto& assign : f.scope.assignments) {
        co_visit(assign.pattern, assign.type, append_errors);
      }

      co_visit(f.scope.result, *f.header.type.output, append_errors);

      return value_or_errors(std::move(f), std::move(errors));
    });
}

ContextualResult<CompoundType<TypeID>>
type_check(const Env& e, const ast::Pattern& pattern, CompoundType<TypeID> type) {
  Map<Variable, std::vector<VariablePropagation>> propagations;
  knot::preorder(pattern, [&](const Pattern& pattern) {
    knot::visit(pattern.v, [&](const std::vector<Pattern>& tuple) { add_tuple(propagations, tuple, &pattern); });
  });

  std::deque<std::pair<Variable, CompoundType<TypeID>>> candidates = append_pattern_candidates(pattern, {});
  candidates.emplace_back(&pattern, std::move(type));

  return constraint_propagation(e, std::move(propagations), std::move(candidates)).map([&](auto types) {
    return types.at(&pattern);
  });
}

} // namespace ooze
