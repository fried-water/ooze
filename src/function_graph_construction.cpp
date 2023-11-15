#include "pch.h"

#include "function_graph_construction.h"
#include "function_graph_inner.h"
#include "ooze/tree.h"

#include <numeric>

namespace ooze {

namespace {

std::vector<PassBy> pass_bys_of(const Env& e, const Type<TypeID>& type, std::vector<PassBy> pass_bys = {}) {
  knot::preorder(
    type,
    Overloaded{
      [&](TypeID t) {
        pass_bys.push_back(e.copy_types.find(t) != e.copy_types.end() ? PassBy::Copy : PassBy::Move);
        return false;
      },
      [&](const BorrowType<TypeID>& t) {
        pass_bys.push_back(PassBy::Borrow);
        return false;
      },
      [&](const FunctionType<TypeID>& t) {
        pass_bys.push_back(PassBy::Copy);
        return false;
      },
    });

  return pass_bys;
}

std::vector<bool> borrows_of(const Type<TypeID>& type) {
  std::vector<bool> borrows;

  knot::preorder(
    type,
    Overloaded{
      [&](TypeID t) {
        borrows.push_back(false);
        return false;
      },
      [&](const BorrowType<TypeID>& t) {
        borrows.push_back(true);
        return false;
      },
      [&](const FunctionType<TypeID>& t) {
        borrows.push_back(false);
        return false;
      },
    });

  return borrows;
}

int output_count_of(const Type<TypeID>& type) {
  int count = 0;
  knot::preorder(type,
                 Overloaded{
                   [&](TypeID) { ++count; },
                   [&](const FunctionType<TypeID>&) {
                     ++count;
                     return false;
                   },
                 });

  return count;
}

struct GraphContext {
  ConstructingGraph cg;
  std::vector<Map<std::string, std::vector<Oterm>>> bindings;
};

GraphContext append_bindings(const TypedPattern& pattern, const std::vector<Oterm>& terms, GraphContext ctx) {
  int i = 0;
  co_visit(pattern,
           pattern.type,
           Overloaded{[&](const auto&, const auto& type, const ast::Ident& ident, const auto&) {
                        ctx.bindings.back()[ident.name] =
                          knot::preorder_accumulate(type, std::vector<Oterm>{}, [&](auto v, TypeID) {
                            v.push_back(terms[i++]);
                            return v;
                          });
                      },
                      [&](const ast::WildCard&, const auto& type) { knot::preorder(type, [&](TypeID) { i++; }); }});
  return ctx;
}

std::pair<GraphContext, std::vector<Oterm>> add_expr(const Env&, const CheckedExpr&, GraphContext);

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const std::vector<CheckedExpr>& exprs, const Type<TypeID>&, GraphContext ctx) {
  return knot::accumulate(
    exprs, std::pair(std::move(ctx), std::vector<Oterm>{}), [&](auto pair, const CheckedExpr& expr) {
      auto [ctx, terms] = add_expr(e, expr, std::move(pair.first));
      return std::pair(std::move(ctx), to_vec(std::move(terms), std::move(pair.second)));
    });
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const ast::ScopeExpr<TypeID, EnvFunctionRef>& scope, const Type<TypeID>&, GraphContext ctx) {
  ctx.bindings.emplace_back();

  ctx = knot::accumulate(scope.assignments, std::move(ctx), [&](GraphContext ctx, const CheckedAssignment& assignment) {
    std::vector<Oterm> terms;
    std::tie(ctx, terms) = add_expr(e, *assignment.expr, std::move(ctx));
    return append_bindings(assignment.pattern, terms, std::move(ctx));
  });

  std::vector<Oterm> terms;
  std::tie(ctx, terms) = add_expr(e, *scope.result, std::move(ctx));
  ctx.bindings.pop_back();

  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>> add_expr(
  const Env& e, const ast::SelectExpr<TypeID, EnvFunctionRef>& select, const Type<TypeID>& type, GraphContext ctx) {
  std::vector<Oterm> cond_terms;
  std::vector<Oterm> if_terms;
  std::vector<Oterm> else_terms;

  std::tie(ctx, cond_terms) = add_expr(e, *select.condition, std::move(ctx));
  std::tie(ctx, if_terms) = add_expr(e, *select.if_expr, std::move(ctx));
  std::tie(ctx, else_terms) = add_expr(e, *select.else_expr, std::move(ctx));

  assert(cond_terms.size() == 1);
  assert(if_terms.size() == else_terms.size());

  std::vector<PassBy> pass_bys;
  pass_bys.reserve(cond_terms.size() + if_terms.size() + else_terms.size());
  pass_bys = pass_bys_of(e, select.condition->type, std::move(pass_bys));
  pass_bys = pass_bys_of(e, select.if_expr->type, std::move(pass_bys));
  pass_bys = pass_bys_of(e, select.else_expr->type, std::move(pass_bys));

  std::vector<Oterm> terms =
    ctx.cg.add(create_async_select(),
               flatten(std::move(cond_terms), std::move(if_terms), std::move(else_terms)),
               pass_bys,
               output_count_of(type));
  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const ast::CallExpr<TypeID, EnvFunctionRef>& call, const Type<TypeID>& type, GraphContext ctx) {
  std::vector<Oterm> arg_terms;
  std::tie(ctx, arg_terms) = add_expr(e, *call.arg, std::move(ctx));

  const int output_count = output_count_of(type);

  if(const auto* ref = std::get_if<EnvFunctionRef>(&call.callee->v); ref) {
    const auto it = e.functions.find(ref->name);
    assert(it != e.functions.end() && ref->overload_idx < it->second.size());
    const EnvFunction& ef = it->second[ref->overload_idx];

    std::vector<Oterm> terms = std::visit(
      Overloaded{
        [&](const AsyncFn& f) { return ctx.cg.add(f, arg_terms, pass_bys_of(e, call.arg->type), output_count); },
        [&](const FunctionGraph& f) { return ctx.cg.add(f, arg_terms); },
        [&](const TypedFunction&) {
          const auto it = find_if(ef.instatiations, [&](const auto& p) {
            return std::get<FunctionType<TypeID>>(call.callee->type.v) == p.first;
          });
          assert(it != ef.instatiations.end());
          return ctx.cg.add(it->second, arg_terms);
        }},
      ef.f);
    return {std::move(ctx), std::move(terms)};
  } else {
    auto [ctx2, callee_terms] = add_expr(e, *call.callee, std::move(ctx));
    assert(callee_terms.size() == 1);

    std::vector<PassBy> pass_bys;
    pass_bys.reserve(callee_terms.size() + arg_terms.size());
    pass_bys = pass_bys_of(e, call.callee->type, std::move(pass_bys));
    pass_bys = pass_bys_of(e, call.arg->type, std::move(pass_bys));

    std::vector<Oterm> terms =
      ctx2.cg.add(create_async_functional(output_count),
                  flatten(std::move(callee_terms), std::move(arg_terms)),
                  pass_bys,
                  output_count);
    return {std::move(ctx2), std::move(terms)};
  }
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const ast::BorrowExpr<TypeID, EnvFunctionRef>& borrow, const Type<TypeID>&, GraphContext ctx) {
  return add_expr(e, *borrow.expr, std::move(ctx));
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const ast::Ident& ident, const Type<TypeID>& type, GraphContext ctx) {
  std::optional<std::vector<Oterm>> terms = std::accumulate(
    ctx.bindings.rbegin(),
    ctx.bindings.rend(),
    std::optional<std::vector<Oterm>>{},
    [&](auto acc, const auto& bindings) {
      if(acc) {
        return acc;
      } else {
        const auto it = bindings.find(ident.name);
        return it != bindings.end() ? std::optional(it->second) : std::nullopt;
      }
    });

  return {std::move(ctx), std::move(*terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env&, const Literal& literal, const Type<TypeID>&, GraphContext ctx) {
  std::vector<Oterm> terms =
    std::visit([&](const auto& value) { return ctx.cg.add(create_async_value(Any(value)), {}, {}, 1); }, literal);
  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>>
add_expr(const Env& e, const EnvFunctionRef& fn_ref, const Type<TypeID>& type, GraphContext ctx) {
  const EnvFunction& ef = e.functions.at(fn_ref.name)[fn_ref.overload_idx];

  AsyncFn f = std::visit(
    Overloaded{[&](const AsyncFn& f) { return f; },
               [](const FunctionGraph& f) { return create_async_graph(f); },
               [&](const TypedFunction&) {
                 const auto it = find_if(ef.instatiations, [&](const auto& p) {
                   return std::get<FunctionType<TypeID>>(type.v) == p.first;
                 });
                 assert(it != ef.instatiations.end());
                 return create_async_graph(it->second);
               }},
    ef.f);

  std::vector<Oterm> terms = ctx.cg.add(create_async_value(Any(std::move(f))), {}, {}, 1);

  return {std::move(ctx), std::move(terms)};
}

std::pair<GraphContext, std::vector<Oterm>> add_expr(const Env& e, const CheckedExpr& expr, GraphContext ctx) {
  return std::visit([&](const auto& sub_expr) { return add_expr(e, sub_expr, expr.type, std::move(ctx)); }, expr.v);
}

} // namespace

FunctionGraph create_graph(const Env& e, const CheckedFunction& f) {
  auto [cg, terms] = make_graph(borrows_of(f.pattern.type));
  auto [ctx, output_terms] = add_expr(e, f.expr, append_bindings(f.pattern, terms, GraphContext{std::move(cg), {{}}}));
  return std::move(ctx.cg).finalize(output_terms, pass_bys_of(e, f.expr.type));
}

} // namespace ooze
