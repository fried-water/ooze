#include "pch.h"

#include "queries.h"
#include "type_check.h"

namespace ooze {

using namespace ast;

namespace {

using BindingRef = std::variant<const Binding<TypeID>*, std::string>;

struct PropagationCaches {
  Map<const TypedExpr*, const TypedExpr*> function_receivers;
  Map<const TypedExpr*, const std::vector<Binding<TypeID>>*> expr_bindings;

  Map<BindingRef, std::vector<const TypedExpr*>> uses;
  Map<BindingRef, std::tuple<const TypedExpr*, int, int>> binding_src;
  Map<const TypedExpr*, BindingRef> binding_of;
};

struct PropagationCandidate {
  const TypedExpr* expr = nullptr;
  std::optional<std::vector<std::optional<TypeProperties>>> types;
};

bool type_check(Span<TypeProperties> expected, Span<TypeProperties> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    // Don't allow copies (take by value, given borrowed)
    if(expected[i].id != given[i].id || expected[i].value > given[i].value) {
      return false;
    }
  }

  return true;
}

bool type_check(Span<TypeProperties> expected, Span<std::optional<TypeProperties>> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    // Don't allow copies (take by value, given borrowed)
    if(given[i] && (expected[i].id != given[i]->id || expected[i].value > given[i]->value)) {
      return false;
    }
  }

  return true;
}

bool type_check(Span<TypeID> expected, Span<TypeID> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    if(expected[i] != given[i]) {
      return false;
    }
  }

  return true;
}

bool output_type_check(Span<TypeID> expected, Span<std::optional<TypeProperties>> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    if(given[i] && expected[i] != given[i]->id) {
      return false;
    }
  }

  return true;
}

ContextualResult<std::tuple<int, const EnvFunction*>>
overload_resolution(const Env& e,
                    const TypedExpr* expr,
                    const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                    const std::optional<std::vector<std::optional<TypeProperties>>>& propagated_outputs) {
  const auto& call = std::get<Call<NamedFunction>>(expr->v);

  std::vector<ContextualError> errors;
  const auto fit = e.functions.find(call.function.name);

  if(fit == e.functions.end()) {
    errors.push_back({{}, fmt::format("use of undeclared function '{}'", call.function.name)});
  }

  std::vector<std::optional<TypeProperties>> inputs;

  for(const TypedExpr& parameter_expr : call.parameters) {
    if(const auto eit = expr_types.find(&parameter_expr); eit == expr_types.end()) {
      inputs.push_back(std::nullopt);
    } else if(eit->second.size() == 1) {
      inputs.push_back(eit->second.front());
    } else {
      errors.push_back({{}, fmt::format("call to function {} takes an expr that returns a tuple", call.function.name)});
    }
  }

  if(!errors.empty()) {
    return tl::unexpected{std::move(errors)};
  }

  const auto oit = expr_types.find(expr);

  const std::vector<std::optional<TypeProperties>>* outputs =
    propagated_outputs ? &(*propagated_outputs) : (oit != expr_types.end() ? &(oit->second) : nullptr);

  std::vector<std::tuple<int, const EnvFunction*>> results;

  for(int i = 0; i < (int)fit->second.size(); i++) {
    const auto& fn = fit->second[i];
    if(type_check(input_types(fn), inputs) && (outputs == nullptr || output_type_check(output_types(fn), *outputs))) {
      results.emplace_back(i, &fn);
    }
  }

  if(results.size() == 1) {
    return results.front();
  } else if(results.empty()) {
    ContextualError error{{},
                          fmt::format("no matching overload found, deduced {}{} -> {} [{} candidate(s)]",
                                      call.function.name,
                                      type_list_string(e, inputs),
                                      outputs ? type_list_string(e, *outputs) : "_",
                                      fit->second.size())};

    for(const auto& function : fit->second) {
      error.notes.push_back(fmt::format("  {}", function_string(e, call.function.name, function)));
    }

    return tl::unexpected{std::vector<ContextualError>{std::move(error)}};
  } else {
    ContextualError error{{},
                          fmt::format("function call is ambiguous, deduced {}{} -> {} [{} candidate(s)]",
                                      call.function.name,
                                      type_list_string(e, inputs),
                                      outputs ? type_list_string(e, *outputs) : "_",
                                      results.size())};

    for(const auto& [ref, function] : results) {
      error.notes.push_back(fmt::format("  {}", function_string(e, call.function.name, *function)));
    }

    return tl::unexpected{std::vector<ContextualError>{std::move(error)}};
  }
}

Map<const TypedExpr*, const std::vector<Binding<TypeID>>*>
find_expr_bindings(const std::vector<TypedAssignment>& assignments) {
  return knot::map<Map<const TypedExpr*, const std::vector<Binding<TypeID>>*>>(
    assignments, [](const TypedAssignment& a) {
      return std::pair<const TypedExpr* const, const std::vector<Binding<TypeID>>*>(&a.expr, &a.bindings);
    });
}

Map<const TypedExpr*, const TypedExpr*> find_function_receivers(const TypedBody& b) {
  Map<const TypedExpr*, const TypedExpr*> function_receivers;

  knot::preorder(b, [&](const TypedExpr& expr) {
    if(const auto* call = std::get_if<Call<NamedFunction>>(&expr.v); call) {
      for(const TypedExpr& parameter_expr : call->parameters) {
        function_receivers.emplace(&parameter_expr, &expr);
      }
    }
  });

  return function_receivers;
}

PropagationCaches calculate_propagation_caches(const TypedBody& b) {
  Map<std::string, const Binding<TypeID>*> binding_names;

  Map<BindingRef, std::vector<const TypedExpr*>> uses;
  Map<BindingRef, std::tuple<const TypedExpr*, int, int>> binding_src;
  Map<const TypedExpr*, BindingRef> binding_of;

  const auto visit_expr = [&](const TypedExpr& root) {
    knot::preorder(root, [&](const TypedExpr& e) {
      knot::visit(e.v, [&](const std::string& name) {
        if(const auto it = binding_names.find(name); it != binding_names.end()) {
          uses[it->second].push_back(&e);
          binding_of.emplace(&e, it->second);
        } else {
          uses[name].push_back(&e);
          binding_of.emplace(&e, name);
        }
      });
    });
  };

  for(const TypedAssignment& assign : b.assignments) {
    visit_expr(assign.expr);

    int i = 0;
    for(const Binding<TypeID>& b : assign.bindings) {
      binding_names[b.name] = &b;
      binding_src.emplace(&b, std::tuple(&assign.expr, i++, (int)assign.bindings.size()));
    }
  }

  for(const TypedExpr& e : b.result) {
    visit_expr(e);
  }

  return PropagationCaches{find_function_receivers(b),
                           find_expr_bindings(b.assignments),
                           std::move(uses),
                           std::move(binding_src),
                           std::move(binding_of)};
}

auto append_header_candidates(const TypedFunction& f,
                              const Map<BindingRef, std::vector<const TypedExpr*>>& uses,
                              std::vector<PropagationCandidate> candidates = {}) {

  // Propagate parameters
  for(const Parameter<TypeID>& p : f.header.parameters) {
    if(const auto it = uses.find(p.name); it != uses.end()) {
      for(const TypedExpr* expr : it->second) {
        candidates.push_back({expr, {{TypeProperties{p.type, !p.borrow}}}});
      }
    }
  }

  // Propagate return value
  if(f.header.result.empty() || f.body.result.size() == 1) {
    std::vector<std::optional<TypeProperties>> types;

    for(size_t i = 0; i < f.header.result.size(); i++) {
      types.push_back(TypeProperties{f.header.result[i], true});
    }

    for(size_t i = 0; i < f.body.result.size(); i++) {
      candidates.push_back({&f.body.result[i], types});
    }
  } else if(f.body.result.size() == f.header.result.size()) {
    for(size_t i = 0; i < f.body.result.size(); i++) {
      candidates.push_back({&f.body.result[i], {{TypeProperties{f.header.result[i], true}}}});
    }
  }

  return candidates;
}

auto append_body_candidates(const Env& e, const TypedBody& b, std::vector<PropagationCandidate> candidates = {}) {
  // Append assignment hints
  for(const TypedAssignment& assign : b.assignments) {
    candidates.push_back(
      {&assign.expr,
       knot::map<std::vector<std::optional<TypeProperties>>>(assign.bindings, [&](const Binding<TypeID>& binding) {
         return binding.type ? std::optional(TypeProperties{*binding.type, true}) : std::nullopt;
       })});
  }

  // Append literal types
  knot::preorder(b, [&](const TypedExpr& expr) {
    if(const auto* literal = std::get_if<Literal>(&expr.v); literal) {
      candidates.push_back({&expr, {{TypeProperties{type_of(*literal), true}}}});
    }
  });

  // Append function calls with a single overload
  knot::preorder(b, [&](const TypedExpr& expr) {
    if(const auto* call = std::get_if<Call<NamedFunction>>(&expr.v); call) {
      if(const auto it = e.functions.find(call->function.name); it != e.functions.end() && it->second.size() == 1) {
        candidates.push_back({&expr, std::nullopt});
      }
    }
  });

  return candidates;
}

auto append_binding_candidates(const TypedBody& b,
                               const Map<std::string, TypeID>& bindings,
                               std::vector<PropagationCandidate> candidates = {}) {
  knot::preorder(b, [&](const TypedExpr& expr) {
    if(const std::string* str = std::get_if<std::string>(&expr.v); str) {
      if(const auto it = bindings.find(*str); it != bindings.end()) {
        candidates.push_back({&expr, std::vector{std::optional{TypeProperties{it->second, true}}}});
      }
    }
  });

  return candidates;
}

std::optional<std::vector<std::optional<TypeProperties>>>
propagate_function(const Env& e,
                   const TypedExpr* expr,
                   const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                   std::optional<std::vector<std::optional<TypeProperties>>> propagated_types,
                   std::vector<PropagationCandidate>& to_visit) {
  const auto function = overload_resolution(e, expr, expr_types, propagated_types);

  if(function) {
    const auto& call = std::get<Call<NamedFunction>>(expr->v);

    const auto& inputs = input_types(*std::get<1>(*function));
    for(size_t i = 0; i < inputs.size(); i++) {
      const TypedExpr& parameter = call.parameters[i];
      to_visit.push_back({&parameter, {{inputs[i]}}});
    }

    return knot::map<std::vector<std::optional<TypeProperties>>>(output_types(*std::get<1>(*function)),
                                                                 [](TypeID type) {
                                                                   return std::optional(TypeProperties{type, true});
                                                                 });
  } else {
    return propagated_types;
  }
}

std::optional<std::vector<std::optional<TypeProperties>>>
merge_types(std::vector<std::optional<TypeProperties>> t1, const std::vector<std::optional<TypeProperties>>& t2) {
  if(t1.size() != t2.size()) {
    return std::nullopt;
  }

  for(size_t i = 0; i < t1.size(); i++) {
    if(!t1[i]) {
      t1[i] = t2[i];
    } else if(!t2[i]) {
      // nothing to do
    } else if(t1[i]->id == t2[i]->id) {
      t1[i]->value = std::min(t1[i]->value, t2[i]->value);
    } else {
      return std::nullopt;
    }
  }

  return t1;
}

auto constraint_propagation(const Env& e, const PropagationCaches& pc, std::vector<PropagationCandidate> to_visit) {
  Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>> expr_types;
  Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>> conflicting_types;

  while(!to_visit.empty()) {
    PropagationCandidate c = std::move(to_visit.front());
    to_visit.erase(to_visit.begin());

    if(std::holds_alternative<Call<NamedFunction>>(c.expr->v)) {
      c.types = propagate_function(e, c.expr, expr_types, std::move(c.types), to_visit);
    }

    const auto it = expr_types.find(c.expr);

    if(c.types && it != expr_types.end()) {
      auto orig_types = *c.types;
      c.types = merge_types(std::move(*c.types), it->second);

      if(!c.types) {
        conflicting_types.emplace(c.expr, std::move(orig_types));
      }
    }

    if(c.types && (it == expr_types.end() || *c.types != it->second)) {
      expr_types[c.expr] = *c.types;
      auto& types = *c.types;

      if(const auto rit = pc.function_receivers.find(c.expr); rit != pc.function_receivers.end()) {
        to_visit.push_back({rit->second, std::nullopt});
      } else if(const auto bit = pc.expr_bindings.find(c.expr); bit != pc.expr_bindings.end()) {
        assert(types.size() == bit->second->size());

        for(size_t i = 0; i < types.size(); i++) {
          if(const auto uit = pc.uses.find(&(*bit->second)[i]); uit != pc.uses.end()) {
            for(const TypedExpr* use : uit->second) {
              to_visit.push_back({use, {{types[i]}}});
            }
          }
        }
      }

      if(const auto bit = pc.binding_of.find(c.expr); bit != pc.binding_of.end()) {
        // Always propagate bindings as values
        for(auto& opt_type : types) {
          if(opt_type) {
            opt_type->value = true;
          }
        }

        if(const auto uit = pc.uses.find(bit->second); uit != pc.uses.end()) {
          for(const TypedExpr* use : uit->second) {
            if(use != c.expr) {
              to_visit.push_back({use, types});
            }
          }
        }

        if(const auto sit = pc.binding_src.find(bit->second); sit != pc.binding_src.end()) {
          assert(types.size() == 1);
          const auto& [expr, idx, size] = sit->second;
          std::vector<std::optional<TypeProperties>> binding_types(size);
          binding_types[idx] = types.front();
          to_visit.push_back({expr, binding_types});
        }
      }
    }
  }

  return std::pair(std::move(expr_types), std::move(conflicting_types));
}

ContextualResult<CheckedBody>
find_overloads(const Env& e,
               const TypedBody& b,
               const PropagationCaches& pc,
               const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types) {
  Map<const NamedFunction*, ContextualResult<std::tuple<int, const EnvFunction*>>> overloads;

  knot::preorder(b, [&](const TypedExpr& expr) {
    if(const auto* call = std::get_if<Call<NamedFunction>>(&expr.v); call) {
      overloads.emplace(&call->function, overload_resolution(e, &expr, expr_types, std::nullopt));
    }
  });

  std::vector<ContextualError> errors;

  CheckedBody checked_body = knot::map<CheckedBody>(b, [&](const NamedFunction& f) {
    auto& result = overloads.at(&f);
    if(!result.has_value()) {
      for(auto& error : result.error()) {
        errors.push_back(std::move(error));
      }
    }
    return EnvFunctionRef{f.name, result.has_value() ? std::get<0>(*result) : 0};
  });

  return value_or_errors(std::move(checked_body), std::move(errors));
}

std::optional<TypeProperties>
deduced_single_type(const std::vector<const TypedExpr*>& exprs,
                    const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types) {
  TypeProperties p = {};

  for(const TypedExpr* expr : exprs) {
    const auto it = expr_types.find(expr);
    if(it == expr_types.end() || it->second.size() != 1 || !it->second.front().has_value()) {
      return std::nullopt;
    }

    p.id = it->second.front()->id;
    p.value |= it->second.front()->value;
  }

  return p == TypeProperties{} ? std::nullopt : std::optional(p);
}

ContextualResult<TypedHeader>
generate_header(const TypedBody& b,
                const Map<BindingRef, std::vector<const TypedExpr*>>& uses,
                const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types) {
  std::vector<ContextualError> errors;

  TypedHeader header;

  // Determine parameters
  for(const auto& [name, exprs] : uses) {
    if(const auto name_ptr = std::get_if<std::string>(&name); name_ptr) {
      if(const auto p = deduced_single_type(exprs, expr_types); p) {
        header.parameters.push_back(ast::Parameter<TypeID>{*name_ptr, p->id, !p->value});
      } else {
        errors.push_back({{}, fmt::format("unable to deduce type of '{}'", *name_ptr)});
      }
    }
  }

  std::sort(header.parameters.begin(), header.parameters.end());

  for(const TypedExpr& expr : b.result) {
    if(const auto it = expr_types.find(&expr); it != expr_types.end()) {
      for(const auto& opt_type : it->second) {
        if(!opt_type) {
          errors.push_back({expr.ref, "unable to deduce return type"});
        } else {
          header.result.push_back(opt_type->id);
        }
      }
    } else {
      errors.push_back({expr.ref, "unable to deduce return type"});
    }
  }

  return value_or_errors(std::move(header), std::move(errors));
}

ContextualResult<void> extra_checks(const TypedFunction& f, const PropagationCaches& pc) {
  std::vector<ContextualError> errors;

  for(const auto& [name, exprs] : pc.uses) {
    if(const auto name_ptr = std::get_if<std::string>(&name); name_ptr) {
      const auto it = std::find_if(
        f.header.parameters.begin(), f.header.parameters.end(), [&](const auto& p) { return p.name == *name_ptr; });
      if(it == f.header.parameters.end()) {
        errors.push_back({{}, fmt::format("use of undeclared binding '{}'", *name_ptr)});
      }
    }
  }

  for(const auto& result_expr : f.body.result) {
    if(const auto binding_it = pc.binding_of.find(&result_expr); binding_it != pc.binding_of.end()) {
      if(const auto name_ptr = std::get_if<std::string>(&binding_it->second); name_ptr) {
        const auto it = std::find_if(
          f.header.parameters.begin(), f.header.parameters.end(), [&](const auto& p) { return p.name == *name_ptr; });
        if(it != f.header.parameters.end() && it->borrow) {
          errors.push_back({{}, fmt::format("attempting to return borrowed parameter '{}'", *name_ptr)});
        }
      }
    }
  }

  if(!f.header.result.empty() && f.body.result.size() != 1 && f.header.result.size() != f.body.result.size()) {
    errors.push_back({{},
                      fmt::format("function header expects {} return values, given {}",
                                  f.header.result.size(),
                                  f.body.result.size())});
  }

  return errors.empty() ? ContextualResult<void>{} : tl::unexpected{std::move(errors)};
}

ContextualResult<void> check_missing_bindings(const Map<BindingRef, std::vector<const TypedExpr*>>& uses,
                                              const std::unordered_map<std::string, TypeID>& bindings) {
  std::vector<ContextualError> errors;

  for(const auto& [name, exprs] : uses) {
    if(const auto name_ptr = std::get_if<std::string>(&name); name_ptr) {
      if(const auto it = bindings.find(*name_ptr); it == bindings.end()) {
        errors.push_back({{}, fmt::format("use of undeclared binding '{}'", *name_ptr)});
      }
    }
  }

  return errors.empty() ? ContextualResult<void>{} : tl::unexpected{std::move(errors)};
}

ContextualResult<void>
check_conflicting_errors(const Env& e,
                         const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                         const Map<const TypedExpr*, std::vector<std::optional<TypeProperties>>>& conflicting_types) {
  std::vector<ContextualError> errors;

  for(const auto& [expr, types] : conflicting_types) {
    const auto& expected_types = expr_types.at(expr);
    errors.push_back(
      {{},
       fmt::format("expected {}, found {}",
                   expected_types.size() == 1 ? type_name_or_id(e, *expected_types.front())
                                              : type_list_string(e, expected_types),
                   types.size() == 1 ? type_name_or_id(e, *types.front()) : type_list_string(e, types))});
  }

  return errors.empty() ? ContextualResult<void>{} : tl::unexpected{std::move(errors)};
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

UnTypedBody convert_to_function_body(std::variant<UnTypedExpr, UnTypedAssignment> v) {
  return std::visit(Overloaded{[](UnTypedExpr e) {
                                 return UnTypedBody{{}, {std::move(e)}};
                               },
                               [](UnTypedAssignment a) {
                                 UnTypedBody b{{std::move(a)}};
                                 for(const auto& binding : b.assignments.front().bindings) {
                                   b.result.push_back(UnTypedExpr{binding.name});
                                 }
                                 return b;
                               }},
                    std::move(v));
}

ContextualResult<TypedFunction> type_name_resolution(const Env& e, const UnTypedFunction& f) {
  return type_name_resolution<TypedFunction>(e, std::tie(f.header, f.body));
}
ContextualResult<TypedBody> type_name_resolution(const Env& e, const UnTypedBody& b) {
  return type_name_resolution<TypedBody>(e, b);
}

ContextualResult<CheckedFunction> overload_resolution(const Env& e, const TypedFunction& f) {
  const PropagationCaches pc = calculate_propagation_caches(f.body);

  const auto [expr_types, conflicting_types] =
    constraint_propagation(e, pc, append_body_candidates(e, f.body, append_header_candidates(f, pc.uses)));

  auto conflicting_result = check_conflicting_errors(e, expr_types, conflicting_types);

  return merge(extra_checks(f, pc), find_overloads(e, f.body, pc, expr_types))
    .and_then([&](auto tup) { return merge(std::move(std::get<0>(tup)), std::move(conflicting_result)); })
    .map([&](auto tup) {
      return CheckedFunction{std::move(f.header), std::move(std::get<0>(tup))};
    });
}

ContextualResult<CheckedFunction>
overload_resolution(const Env& e, const TypedBody& body, const std::unordered_map<std::string, TypeID>& bindings) {
  const PropagationCaches pc = calculate_propagation_caches(body);

  const auto [expr_types, conflicting_types] =
    constraint_propagation(e, pc, append_body_candidates(e, body, append_binding_candidates(body, bindings)));

  auto header_result = generate_header(body, pc.uses, expr_types);
  auto body_result = find_overloads(e, body, pc, expr_types);
  auto conflicting_result = check_conflicting_errors(e, expr_types, conflicting_types);

  return check_missing_bindings(pc.uses, bindings)
    .and_then([&]() { return std::move(body_result); })
    .and_then([&](CheckedBody b) { return merge(std::move(header_result), std::move(b)); })
    .and_then([&](auto tup) {
      return merge(std::move(std::get<0>(tup)), std::move(std::get<1>(tup)), std::move(conflicting_result));
    })
    .map([&](auto tup) {
      return CheckedFunction{std::move(std::get<0>(tup)), std::move(std::get<1>(tup))};
    });
}

} // namespace ooze
