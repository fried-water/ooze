#include "pch.h"

#include "overload_resolution.h"
#include "queries.h"

namespace ooze {

using namespace ast;

namespace {

using BindingRef = std::variant<const Binding<NamedType>*, const Parameter<NamedType>*>;

struct PropagationCaches {
  Map<const UnTypedExpr*, const UnTypedExpr*> function_receivers;
  Map<const UnTypedExpr*, const std::vector<Binding<NamedType>>*> expr_bindings;

  Map<BindingRef, std::vector<const UnTypedExpr*>> uses;
  Map<BindingRef, const UnTypedExpr*> binding_src;
  Map<const UnTypedExpr*, BindingRef> binding_of;
};

struct PropagationCandidate {
  const UnTypedExpr* expr = nullptr;
  std::optional<std::vector<std::optional<TypeProperties>>> types;
};

std::optional<TypeProperties> get_type(const Env& e, const NamedType& type, bool borrowed) {
  const auto it = e.type_ids.find(type.name);

  return it != e.type_ids.end() ? std::optional(TypeProperties{it->second, !borrowed}) : std::nullopt;
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

bool type_check(Span<TypeID> expected, Span<std::optional<TypeProperties>> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    // Don't allow copies (take by value, given borrowed)
    if(given[i] && (!given[i]->value || expected[i] != given[i]->id)) {
      return false;
    }
  }

  return true;
}

Result<std::tuple<int, const EnvFunction*>>
overload_resolution(const Env& e,
                    const UnTypedExpr* expr,
                    const Map<const UnTypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                    const std::optional<std::vector<std::optional<TypeProperties>>>& propagated_outputs) {
  const auto& call = *std::get<Indirect<Call<NamedFunction>>>(expr->v);

  std::vector<std::string> errors;
  const auto fit = e.functions.find(call.function.name);

  if(fit == e.functions.end()) {
    errors.push_back(fmt::format("use of undeclared function '{}'", call.function.name));
  }

  std::vector<std::optional<TypeProperties>> inputs;

  for(const UnTypedExpr& parameter_expr : call.parameters) {
    if(const auto eit = expr_types.find(&parameter_expr); eit == expr_types.end()) {
      inputs.push_back(std::nullopt);
    } else if(eit->second.size() == 1) {
      inputs.push_back(eit->second.front());
    } else {
      errors.push_back(fmt::format("call to function {} takes an expr that returns a tuple", call.function.name));
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
    if(type_check(input_types(fn), inputs) && (outputs == nullptr || type_check(output_types(fn), *outputs))) {
      results.emplace_back(i, &fn);
    }
  }

  if(results.size() == 1) {
    return results.front();
  } else if(results.empty()) {
    std::vector<std::string> errors{fmt::format("no matching overload found, deduced {}{} -> {} [{} candidate(s)]",
                                                call.function.name,
                                                type_list_string(e, inputs),
                                                outputs ? type_list_string(e, *outputs) : "_",
                                                fit->second.size())};

    for(const auto& function : fit->second) {
      errors.push_back(fmt::format("  {}", function_string(e, call.function.name, function)));
    }

    return tl::unexpected{std::move(errors)};
  } else {
    std::vector<std::string> errors{fmt::format("function call is ambiguous, deduced {}{} -> {} [{} candidate(s)]",
                                                call.function.name,
                                                type_list_string(e, inputs),
                                                outputs ? type_list_string(e, *outputs) : "_",
                                                results.size())};

    for(const auto& [ref, function] : results) {
      errors.push_back(fmt::format("  {}", function_string(e, call.function.name, *function)));
    }

    return tl::unexpected{std::move(errors)};
  }
}

Map<const UnTypedExpr*, const std::vector<Binding<NamedType>>*>
find_expr_bindings(const std::vector<UnTypedAssignment>& assignments) {
  Map<const UnTypedExpr*, const std::vector<Binding<NamedType>>*> expr_bindings;

  for(const auto& [bindings, expr] : assignments) {
    expr_bindings.emplace(&expr, &bindings);
  }

  return expr_bindings;
}

Map<const UnTypedExpr*, const UnTypedExpr*> find_function_receivers(const UnTypedFunction& f) {
  Map<const UnTypedExpr*, const UnTypedExpr*> function_receivers;

  knot::preorder(f, [&](const UnTypedExpr& expr) {
    if(const auto* call = std::get_if<Indirect<Call<NamedFunction>>>(&expr.v); call) {
      for(const UnTypedExpr& parameter_expr : (*call)->parameters) {
        function_receivers.emplace(&parameter_expr, &expr);
      }
    }
  });

  return function_receivers;
}

Result<PropagationCaches> calculate_propagation_caches(const UnTypedFunction& f) {

  Map<std::string, BindingRef> names;
  Map<BindingRef, std::vector<const UnTypedExpr*>> uses;
  Map<BindingRef, const UnTypedExpr*> binding_src;
  Map<const UnTypedExpr*, BindingRef> binding_of;

  std::vector<std::string> errors;

  const auto visit_expr = [&](const UnTypedExpr& root) {
    knot::preorder(root, [&](const UnTypedExpr& e) {
      knot::visit(e.v, [&](const std::string& name) {
        if(const auto it = names.find(name); it != names.end()) {
          uses[it->second].push_back(&e);
          binding_of.emplace(&e, it->second);
        } else {
          errors.push_back(fmt::format("use of undeclared binding '{}'", name));
        }
      });
    });
  };

  for(const Parameter<NamedType>& p : f.parameters) {
    names.emplace(p.name, &p);
  }

  for(const UnTypedAssignment& assign : f.assignments) {
    visit_expr(assign.expr);

    for(const Binding<NamedType>& b : assign.bindings) {
      names[b.name] = &b;
      binding_src.emplace(&b, &assign.expr);
    }
  }

  for(const UnTypedExpr& e : f.ret) {
    visit_expr(e);
  }

  if(errors.empty()) {
    return PropagationCaches{find_function_receivers(f),
                             find_expr_bindings(f.assignments),
                             std::move(uses),
                             std::move(binding_src),
                             std::move(binding_of)};
  } else {
    return tl::unexpected{std::move(errors)};
  }
}

auto append_parameter_candidates(const Env& e,
                                 const std::vector<Parameter<NamedType>>& parameters,
                                 const Map<BindingRef, std::vector<const UnTypedExpr*>>& uses,
                                 std::vector<PropagationCandidate> candidates = {}) {
  for(const Parameter<NamedType>& p : parameters) {
    const std::optional<TypeProperties> type = get_type(e, p.type, p.borrow);
    if(const auto it = uses.find(&p); it != uses.end()) {
      for(const UnTypedExpr* expr : it->second) {
        candidates.push_back({expr, {{type}}});
      }
    }
  }

  return candidates;
}

auto append_return_candidates(const Env& e,
                              const UnTypedFunction& f,
                              std::vector<PropagationCandidate> candidates = {}) {
  assert(f.ret.size() == 1 || f.ret.size() == f.result.size());

  if(f.ret.size() == 1) {
    std::vector<std::optional<TypeProperties>> types;
    for(const NamedType& output : f.result) {
      types.push_back(get_type(e, output, false));
    }
    candidates.push_back({&f.ret.front(), std::move(types)});
  } else {
    for(size_t i = 0; i < f.ret.size(); i++) {
      candidates.push_back({&f.ret[i], {{get_type(e, f.result[i], false)}}});
    }
  }

  return candidates;
}

auto append_assignment_hint_candidates(const Env& e,
                                       const std::vector<UnTypedAssignment>& assignments,
                                       std::vector<PropagationCandidate> candidates = {}) {
  for(const UnTypedAssignment& assign : assignments) {
    std::vector<std::optional<TypeProperties>> types;
    for(const Binding<NamedType>& binding : assign.bindings) {
      types.push_back(binding.type ? get_type(e, *binding.type, false) : std::nullopt);
    }
    candidates.push_back({&assign.expr, std::move(types)});
  }

  return candidates;
}

auto append_literal_candidates(const UnTypedFunction& f, std::vector<PropagationCandidate> candidates = {}) {
  knot::preorder(f, [&](const UnTypedExpr& expr) {
    if(const auto* literal = std::get_if<Literal>(&expr.v); literal) {
      candidates.push_back({&expr, {{TypeProperties{type_of(*literal), true}}}});
    }
  });

  return candidates;
}

auto append_single_overload_candidates(const Env& e,
                                       const UnTypedFunction& f,
                                       std::vector<PropagationCandidate> candidates = {}) {
  knot::preorder(f, [&](const UnTypedExpr& expr) {
    if(const auto* call = std::get_if<Indirect<Call<NamedFunction>>>(&expr.v); call) {
      if(const auto it = e.functions.find((*call)->function.name); it != e.functions.end() && it->second.size() == 1) {
        candidates.push_back({&expr, std::nullopt});
      }
    }
  });

  return candidates;
}

auto append_binding_candidates(const Env& e,
                               const UnTypedFunction& f,
                               const Map<std::string, TypeID>& bindings,
                               std::vector<PropagationCandidate> candidates = {}) {
  knot::preorder(f, [&](const UnTypedExpr& expr) {
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
                   const UnTypedExpr* expr,
                   const Map<const UnTypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                   std::optional<std::vector<std::optional<TypeProperties>>> propagated_types,
                   std::vector<PropagationCandidate>& to_visit) {
  const auto function = overload_resolution(e, expr, expr_types, propagated_types);

  if(function) {
    const auto& call = *std::get<Indirect<Call<NamedFunction>>>(expr->v);

    const auto& inputs = input_types(*std::get<1>(*function));
    for(size_t i = 0; i < inputs.size(); i++) {
      const UnTypedExpr& parameter = call.parameters[i];
      to_visit.push_back({&parameter, {{inputs[i]}}});
    }

    std::vector<std::optional<TypeProperties>> outputs;
    for(const TypeID type : output_types(*std::get<1>(*function))) {
      outputs.push_back(TypeProperties{type, true});
    }

    return outputs;
  } else {
    return propagated_types;
  }
}

auto constraint_propagation(const Env& e,
                            const UnTypedFunction& f,
                            const PropagationCaches& pc,
                            std::vector<PropagationCandidate> to_visit) {
  Map<const UnTypedExpr*, std::vector<std::optional<TypeProperties>>> expr_types;

  while(!to_visit.empty()) {
    PropagationCandidate c = std::move(to_visit.front());
    to_visit.erase(to_visit.begin());

    if(std::holds_alternative<Indirect<Call<NamedFunction>>>(c.expr->v)) {
      c.types = propagate_function(e, c.expr, expr_types, std::move(c.types), to_visit);
    }

    if(!c.types) {
      continue;
    }

    std::vector<std::optional<TypeProperties>>& types = *c.types;

    auto it = expr_types.find(c.expr);

    const std::optional<std::vector<std::optional<TypeProperties>>> original_types =
      it != expr_types.end() ? std::optional(it->second) : std::nullopt;

    if(it == expr_types.end()) {
      it = expr_types.emplace(c.expr, types).first;
    } else {
      auto& existing_types = it->second;

      if(existing_types.size() == types.size()) {
        for(size_t i = 0; i < types.size(); i++) {
          if(!existing_types[i]) {
            existing_types[i] = types[i];
          } else if(types[i] && existing_types[i]->id == types[i]->id) {
            existing_types[i]->value = std::min(existing_types[i]->value, types[i]->value);
          }
        }
      }
    }

    if(original_types != it->second) {
      if(const auto rit = pc.function_receivers.find(c.expr); rit != pc.function_receivers.end()) {
        to_visit.push_back({rit->second, std::nullopt});
      } else if(const auto bit = pc.expr_bindings.find(c.expr); bit != pc.expr_bindings.end()) {
        assert(types.size() == bit->second->size());

        for(size_t i = 0; i < types.size(); i++) {
          if(const auto uit = pc.uses.find(&(*bit->second)[i]); uit != pc.uses.end()) {
            for(const UnTypedExpr* use : uit->second) {
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
          for(const UnTypedExpr* use : uit->second) {
            if(use != c.expr) {
              to_visit.push_back({use, types});
            }
          }
        }

        if(const auto sit = pc.binding_src.find(bit->second); sit != pc.binding_src.end()) {
          to_visit.push_back({sit->second, types});
        }
      }
    }
  }

  return expr_types;
}

Map<const NamedFunction*, Result<std::tuple<int, const EnvFunction*>>>
gather_overloads(const Env& e,
                 const Map<const UnTypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types,
                 const UnTypedFunction& f) {
  Map<const NamedFunction*, Result<std::tuple<int, const EnvFunction*>>> overloads;

  knot::preorder(f, [&](const UnTypedExpr& expr) {
    if(const auto* call = std::get_if<Indirect<Call<NamedFunction>>>(&expr.v); call) {
      overloads.emplace(&(*call)->function, overload_resolution(e, &expr, expr_types, std::nullopt));
    }
  });

  return overloads;
}

Result<TypedFunction>
type_check(const Env& e,
           const UnTypedFunction& function,
           const PropagationCaches& pc,
           const Map<const UnTypedExpr*, std::vector<std::optional<TypeProperties>>>& expr_types) {
  auto overloads = gather_overloads(e, expr_types, function);
  std::vector<std::string> errors;

  TypedFunction typed_function =
    knot::map<TypedFunction>(std::tie(function.parameters, function.assignments, function.ret),
                             Overloaded{[&](const NamedType& t) {
                                          const auto it = e.type_ids.find(t.name);

                                          if(it != e.type_ids.end()) {
                                            return it->second;
                                          } else {
                                            if(!t.name.empty()) {
                                              errors.push_back(fmt::format("use of undefined type {}", t.name));
                                            }
                                            return TypeID{};
                                          }
                                        },
                                        [&](const NamedFunction& f) {
                                          auto& result = overloads.at(&f);
                                          if(!result.has_value()) {
                                            for(std::string& error : result.error()) {
                                              errors.push_back(std::move(error));
                                            }
                                          }
                                          return EnvFunctionRef{f.name, result.has_value() ? std::get<0>(*result) : 0};
                                        }});

  for(int i = 0; i < function.parameters.size(); i++) {

    const auto uses_it = pc.uses.find(&function.parameters[i]);
    assert(uses_it != pc.uses.end() || typed_function.parameters[i].type != TypeID{});

    if(uses_it != pc.uses.end()) {
      TypeProperties p = {};

      for(const UnTypedExpr* expr : uses_it->second) {
        const auto it = expr_types.find(expr);
        assert(it != expr_types.end() && it->second.size() == 1 && it->second.front());
        p.id = it->second.front()->id;
        p.value |= it->second.front()->value;
      }

      Parameter<TypeID>& param = typed_function.parameters[i];

      if(param.type == TypeID{}) {
        param.type = p.id;
        param.borrow = !p.value;
      } else {
        assert(param.type == p.id);
        if(param.borrow == p.value) {
          assert(!param.borrow);
          errors.push_back(fmt::format("parameter '{}' only borrowed but taken by value", param.name));
        }
      }
    }
  }

  return errors.empty() ? Result<TypedFunction>{std::move(typed_function)} : tl::unexpected{std::move(errors)};
}

// Add dummy parameters
UnTypedFunction untyped_sign(const std::variant<UnTypedExpr, UnTypedAssignment>& var,
                             const std::unordered_map<std::string, TypeID>& bindings) {
  std::vector<ast::Parameter<NamedType>> params;

  knot::preorder(var, [&](const UnTypedExpr& expr) {
    if(const std::string* str = std::get_if<std::string>(&expr.v); str) {
      if(bindings.find(*str) != bindings.end()) {
        params.push_back({*str});
      }
    }
  });

  std::sort(params.begin(), params.end());
  params.erase(std::unique(params.begin(), params.end()), params.end());

  return std::visit(Overloaded{[&](const UnTypedExpr& e) {
                                 return UnTypedFunction{"", std::move(params), {}, {}, {e}};
                               },
                               [&](const UnTypedAssignment& a) {
                                 UnTypedFunction f{"", std::move(params), {}, {a}, {}};
                                 for(const auto& binding : a.bindings) {
                                   f.ret.push_back(UnTypedExpr{binding.name});
                                 }
                                 return f;
                               }},
                    var);
}

} // namespace

Result<EnvFunction> overload_resolution(const Env& e,
                                        const std::string& name,
                                        Span<TypeProperties> inputs,
                                        std::optional<Span<TypeID>> outputs) {
  const auto it = e.functions.find(name);

  if(it == e.functions.end()) {
    return err(fmt::format("use of undeclared function '{}'", name));
  }

  std::vector<EnvFunction> results;

  std::copy_if(it->second.begin(), it->second.end(), std::back_inserter(results), [&](const auto& fn) {
    return type_check(input_types(fn), inputs) && (!outputs || type_check(output_types(fn), *outputs));
  });

  if(results.size() == 1) {
    return std::move(results.front());
  } else if(results.empty()) {
    std::vector<std::string> errors{outputs ? fmt::format("no matching overload found for {}{} -> {} [{} candidate(s)]",
                                                          name,
                                                          type_list_string(e, inputs),
                                                          output_type_list_string(e, *outputs),
                                                          it->second.size())
                                            : fmt::format("no matching overload found for {}{} [{} candidate(s)]",
                                                          name,
                                                          type_list_string(e, inputs),
                                                          it->second.size())};

    for(const auto& function : it->second) {
      errors.push_back(fmt::format("  {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  } else {
    std::vector<std::string> errors{fmt::format(
      "function call is ambiguous {}{} [{} candidate(s)]", name, type_list_string(e, inputs), results.size())};

    for(const auto& function : results) {
      errors.push_back(fmt::format("  {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  }
}

Result<TypedFunction> overload_resolution(const Env& e, const UnTypedFunction& f) {
  return calculate_propagation_caches(f).and_then([&](const PropagationCaches& pc) {
    auto candidates = append_parameter_candidates(e, f.parameters, pc.uses);
    candidates = append_literal_candidates(f, std::move(candidates));
    candidates = append_return_candidates(e, f, std::move(candidates));
    candidates = append_assignment_hint_candidates(e, f.assignments, std::move(candidates));
    candidates = append_single_overload_candidates(e, f, std::move(candidates));

    return type_check(e, f, pc, constraint_propagation(e, f, pc, std::move(candidates)));
  });
}

Result<TypedFunction> overload_resolution(const Env& e,
                                          const std::variant<UnTypedExpr, UnTypedAssignment>& var,
                                          const std::unordered_map<std::string, TypeID>& bindings) {
  UnTypedFunction f = untyped_sign(var, bindings);

  return calculate_propagation_caches(f).and_then([&](const PropagationCaches& pc) {
    // Don't propagate parameters or return types
    auto candidates = append_binding_candidates(e, f, bindings);
    candidates = append_literal_candidates(f, std::move(candidates));
    candidates = append_assignment_hint_candidates(e, f.assignments, std::move(candidates));
    candidates = append_single_overload_candidates(e, f, std::move(candidates));

    return type_check(e, f, pc, constraint_propagation(e, f, pc, std::move(candidates)));
  });
}

} // namespace ooze
