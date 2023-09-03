#include "pch.h"

#include "ooze/tree.h"
#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

struct TypeNameResolution {
  const Env& e;
  std::vector<Slice>* errors;

  CompoundType<TypeID> operator()(const CompoundType<NamedType>& type) {
    return std::visit(
      Overloaded{[&](const NamedType& named) {
                   if(const auto it = e.type_ids.find(named.name); it != e.type_ids.end()) {
                     return CompoundType<TypeID>{it->second, type.ref};
                   } else {
                     errors->push_back(type.ref);
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
  std::vector<Slice> error_slices;
  auto typed = knot::map<Typed>(u, TypeNameResolution{e, &error_slices});

  std::sort(error_slices.begin(), error_slices.end());

  std::vector<ContextualError> errors;
  std::transform(error_slices.begin(),
                 std::unique(error_slices.begin(), error_slices.end()),
                 std::back_inserter(errors),
                 [](Slice ref) {
                   return ContextualError{ref, "undefined type"};
                 });

  return value_or_errors(std::move(typed), std::move(errors));
}

struct InferBindingCtx {
  std::vector<Set<std::string>> active;
  std::vector<std::pair<std::string, Slice>> args;
};

InferBindingCtx inferred_bindings(InferBindingCtx ctx, const TypedExpr& expr) {
  return std::visit(
    Overloaded{
      [&](const std::vector<TypedExpr>& tuple) { return knot::accumulate(tuple, std::move(ctx), inferred_bindings); },
      [&](const TypedScopeExpr& scope) {
        ctx.active.emplace_back();

        for(const TypedAssignment& assign : scope.assignments) {
          ctx = inferred_bindings(std::move(ctx), *assign.expr);
          knot::preorder(assign.pattern, [&](const ast::Ident& i) { ctx.active.back().insert(i.name); });
        }

        ctx = inferred_bindings(std::move(ctx), *scope.result);

        ctx.active.pop_back();

        return std::move(ctx);
      },
      [&](const TypedSelectExpr& select) {
        return knot::accumulate(select, std::move(ctx), [](auto ctx, const auto& sub_expr) {
          return inferred_bindings(std::move(ctx), *sub_expr);
        });
      },
      [&](const TypedBorrowExpr& borrow) { return inferred_bindings(std::move(ctx), *borrow.expr); },
      [&](const TypedCallExpr& call) {
        return inferred_bindings(inferred_bindings(std::move(ctx), *call.callee), *call.arg);
      },
      [&](const Literal&) { return std::move(ctx); },
      [&](const ast::Ident& ident) {
        if(std::all_of(ctx.active.begin(), ctx.active.end(), [&](const auto& s) {
             return s.find(ident.name) == s.end();
           })) {
          if(std::find_if(ctx.args.begin(), ctx.args.end(), [&](const auto& p) { return p.first == ident.name; }) ==
             ctx.args.end()) {
            ctx.args.emplace_back(ident.name, expr.ref);
          }
        }

        return std::move(ctx);
      }},
    expr.v);
}

struct FunctionConverter {
  const Map<const TypedExpr*, EnvFunctionRef>& overloads;

  CheckedExpr operator()(const TypedExpr& expr) {
    const auto it = overloads.find(&expr);
    return CheckedExpr{
      it != overloads.end() ? it->second : knot::map<ast::ExprVariant<TypeID, EnvFunctionRef>>(expr.v, std::ref(*this)),
      expr.type,
      expr.ref};
  }
};

} // namespace

ContextualResult<TypedFunction> type_name_resolution(const Env& e, const UnTypedFunction& f) {
  return type_name_resolution<TypedFunction>(e, std::tie(f.pattern, f.expr));
}

ContextualResult<TypedExpr> type_name_resolution(const Env& e, const UnTypedExpr& expr) {
  return type_name_resolution<TypedExpr>(e, expr);
}

ContextualResult<TypedPattern> type_name_resolution(const Env& e, const UnTypedPattern& pattern) {
  return type_name_resolution<TypedPattern>(e, pattern);
}

ContextualResult<TypedAssignment> type_name_resolution(const Env& e, const UnTypedAssignment& a) {
  return type_name_resolution<TypedAssignment>(e, a);
}

ContextualResult<CompoundType<TypeID>> type_name_resolution(const Env& e, const CompoundType<NamedType>& t) {
  return type_name_resolution<CompoundType<TypeID>>(e, t);
}

TypedPattern inferred_inputs(const TypedExpr& expr, Set<std::string> active) {
  const std::vector<std::pair<std::string, Slice>> args =
    inferred_bindings({make_vector(std::move(active))}, expr).args;
  return {{transform_to_vec(args,
                            [](auto p) {
                              return TypedPattern{ast::Ident{std::move(p.first)}, floating_type<TypeID>(), p.second};
                            })},
          {transform_to_vec(args, [](const auto&) { return floating_type<TypeID>(); })}};
}

ContextualResult<CheckedFunction> overload_resolution(const Env& env, const TypedFunction& f) {
  Map<const TypedExpr*, EnvFunctionRef> all_overloads;

  std::vector<ContextualError> overload_errors;
  for(const TypedExpr* expr : undeclared_bindings(f)) {
    const auto& name = std::get<ast::Ident>(expr->v).name;

    if(const auto it = env.functions.find(name); it == env.functions.end()) {
      overload_errors.push_back({expr->ref, fmt::format("use of undeclared binding '{}'", name)});
    } else {
      std::vector<int> overloads;

      for(size_t i = 0; i < it->second.size(); i++) {
        if(unify_types({it->second[i].type}, expr->type)) {
          overloads.push_back(i);
        }
      }

      if(overloads.size() == 1) {
        all_overloads.emplace(expr, EnvFunctionRef{name, overloads.front()});
      } else if(overloads.empty()) {
        overload_errors.push_back(
          {expr->ref,
           "no matching overload found",
           transform_to_vec(
             it->second,
             [&](const auto& f) { return fmt::format("  {}", pretty_print(env, f.type)); },
             make_vector(
               fmt::format("deduced {} [{} candidate(s)]", pretty_print(env, expr->type), it->second.size())))});
      } else {
        overload_errors.push_back(
          {expr->ref,
           "function call is ambiguous",
           transform_to_vec(
             overloads,
             [&](int i) { return fmt::format("  {}", pretty_print(env, it->second[i].type)); },
             make_vector(
               fmt::format("deduced {} [{} candidate(s)]", pretty_print(env, expr->type), overloads.size())))});
      }
    }
  }

  std::vector<ContextualError> errors =
    !overload_errors.empty() ? std::move(overload_errors) : check_fully_resolved(env, f);

  return errors.empty()
           ? ContextualResult<CheckedFunction>{knot::map<CheckedFunction>(f, FunctionConverter{all_overloads})}
           : Failure{std::move(errors)};
}

} // namespace ooze
