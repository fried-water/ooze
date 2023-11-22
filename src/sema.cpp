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

  Type<TypeID> operator()(const Type<NamedType>& type) {
    return std::visit(
      Overloaded{[&](const NamedType& named) {
                   if(const auto it = e.type_ids.find(named.name); it != e.type_ids.end()) {
                     return Type<TypeID>{it->second, type.ref};
                   } else {
                     errors->push_back(type.ref);
                     return Type<TypeID>{TypeID{}, type.ref};
                   }
                 },
                 [&](const std::vector<Type<NamedType>>& v) {
                   return Type<TypeID>{knot::map<std::vector<Type<TypeID>>>(v, *this), type.ref};
                 },
                 [&](const FunctionType<NamedType>& f) {
                   return Type<TypeID>{knot::map<FunctionType<TypeID>>(f, *this), type.ref};
                 },
                 [&](const FloatingType&) { return floating_type<TypeID>(type.ref); },
                 [&](const BorrowType<NamedType>& b) {
                   return Type<TypeID>{knot::map<BorrowType<TypeID>>(b, *this), type.ref};
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

struct IdentToBindingCtx {
  Map<ASTID, ASTID> expr_to_binding;
  std::vector<ASTID> unbound_exprs;
  std::vector<std::pair<std::string_view, ASTID>> stack;
};

void ident_to_binding_references(IdentToBindingCtx& ctx, ASTID id, std::string_view src, const AST& ast) {
  switch(ast.forest[id]) {
  case ASTTag::PatternIdent: ctx.stack.emplace_back(sv(src, ast.srcs[id.get()]), id); break;
  case ASTTag::Fn:
  case ASTTag::ExprWith: {
    const size_t original_size = ctx.stack.size();
    for(ASTID child : ast.forest.child_ids(id)) {
      ident_to_binding_references(ctx, child, src, ast);
    }
    ctx.stack.erase(ctx.stack.begin() + original_size, ctx.stack.end());
    break;
  }
  case ASTTag::ExprIdent: {
    const std::string_view ident = sv(src, ast.srcs[id.get()]);
    const auto it = std::find_if(
      ctx.stack.rbegin(), ctx.stack.rend(), applied([=](std::string_view v, ASTID) { return v == ident; }));
    if(it == ctx.stack.rend()) {
      ctx.unbound_exprs.push_back(id);
    } else {
      const auto [sv, pattern_id] = *it;
      ctx.expr_to_binding.emplace(id, pattern_id);
    }
    break;
  }
  case ASTTag::RootFn:
  case ASTTag::Assignment: {
    const ASTID pattern = *ast.forest.first_child(id);
    const ASTID expr = *ast.forest.next_sibling(pattern);

    // Parse expr before pattern
    ident_to_binding_references(ctx, expr, src, ast);
    ident_to_binding_references(ctx, pattern, src, ast);
    break;
  }
  case ASTTag::PatternWildCard:
  case ASTTag::PatternTuple:
  case ASTTag::ExprLiteral:
  case ASTTag::ExprCall:
  case ASTTag::ExprSelect:
  case ASTTag::ExprBorrow:
  case ASTTag::ExprTuple:
    for(ASTID child : ast.forest.child_ids(id)) {
      ident_to_binding_references(ctx, child, src, ast);
    }
    break;
  };
}

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

ContextualResult<Type<TypeID>> type_name_resolution(const Env& e, const Type<NamedType>& t) {
  return type_name_resolution<Type<TypeID>>(e, t);
}

ContextualResult<std::vector<TypeID>>
type_name_resolution(const Env& e, std::string_view src, const Graph<TypeRef, TypeTag, Slice>& type_graph) {
  std::vector<TypeID> ids(type_graph.num_nodes(), TypeID::Invalid());
  std::vector<ContextualError> errors;

  for(TypeRef t : type_graph.nodes()) {
    if(type_graph.get<TypeTag>(t) == TypeTag::Leaf) {
      if(const auto it = e.type_ids.find(std::string(sv(src, type_graph.get<Slice>(t)))); it != e.type_ids.end()) {
        ids[t.get()] = it->second;
      } else {
        errors.push_back(ContextualError{type_graph.get<Slice>(t), "undefined type"});
      }
    }
  }

  return value_or_errors(std::move(ids), std::move(errors));
}

std::tuple<Map<ASTID, ASTID>, std::vector<ASTID>> ident_to_binding_references(std::string_view src, const AST& ast) {
  IdentToBindingCtx ctx;

  for(ASTID root : ast.forest.root_ids()) {
    ident_to_binding_references(ctx, root, src, ast);
  }

  return std::tuple(std::move(ctx.expr_to_binding), std::move(ctx.unbound_exprs));
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
