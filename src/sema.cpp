#include "pch.h"

#include "pretty_print.h"
#include "sema.h"
#include "type_check.h"

namespace ooze {

namespace {

struct IdentGraphCtx {
  std::vector<std::vector<ASTID>> fanouts;
  std::vector<std::pair<std::string_view, ASTID>> globals;
  std::vector<std::pair<std::string_view, ASTID>> stack;

  std::vector<ASTID> undeclared_bindings;
};

void calculate_ident_graph(IdentGraphCtx& ctx, ASTID id, Span<std::string_view> srcs, const AST& ast) {
  switch(ast.forest[id]) {
  case ASTTag::PatternIdent: ctx.stack.emplace_back(sv(srcs, ast.srcs[id.get()]), id); break;
  case ASTTag::Fn:
  case ASTTag::ExprWith: {
    const size_t original_size = ctx.stack.size();
    for(const ASTID child : ast.forest.child_ids(id)) {
      calculate_ident_graph(ctx, child, srcs, ast);
    }
    ctx.stack.erase(ctx.stack.begin() + i64(original_size), ctx.stack.end());
    break;
  }
  case ASTTag::ExprIdent: {
    const std::string_view ident = sv(srcs, ast.srcs[id.get()]);
    const auto it = std::find_if(
      ctx.stack.rbegin(), ctx.stack.rend(), applied([=](std::string_view v, ASTID) { return v == ident; }));
    if(it != ctx.stack.rend()) {
      const auto [sv, pattern_id] = *it;
      ctx.fanouts[id.get()].push_back(pattern_id);
      ctx.fanouts[pattern_id.get()].push_back(id);
    } else {
      for(const auto& [name, pattern_id] : ctx.globals) {
        if(ident == name) {
          ctx.fanouts[id.get()].push_back(pattern_id);
          ctx.fanouts[pattern_id.get()].push_back(id);
        }
      }

      if(ctx.fanouts[id.get()].empty()) {
        ctx.undeclared_bindings.push_back(id);
      }
    }
    break;
  }
  case ASTTag::Assignment: {
    const auto [pattern, expr] = ast.forest.child_ids(id).take<2>();

    // Parse expr before pattern
    calculate_ident_graph(ctx, expr, srcs, ast);

    // Skip identifier of globals, already added up front
    if(!ast.forest.is_root(id)) {
      calculate_ident_graph(ctx, pattern, srcs, ast);
    }
    break;
  }
  case ASTTag::EnvValue:
  case ASTTag::PatternWildCard:
  case ASTTag::PatternTuple:
  case ASTTag::ExprLiteral:
  case ASTTag::ExprCall:
  case ASTTag::ExprSelect:
  case ASTTag::ExprIf:
  case ASTTag::ExprBorrow:
  case ASTTag::ExprTuple:
    for(const ASTID child : ast.forest.child_ids(id)) {
      calculate_ident_graph(ctx, child, srcs, ast);
    }
    break;
  };
}

ContextualResult<Map<ASTID, ASTID>, AST, TypeGraph> global_binding_resolution(
  Span<std::string_view> srcs,
  const TypeCache& tc,
  const TypeNames& type_names,
  const Graph<ASTID>& ident_graph,
  AST ast,
  TypeGraph tg) {
  Map<ASTID, ASTID> binding_of;

  std::vector<ContextualError> errors;

  for(const ASTID id : ast.forest.ids()) {
    if(ast.forest[id] != ASTTag::ExprIdent) continue;

    assert(ident_graph.num_fanout(id) > 0);

    if(auto [overload, type, matches] = overload_resolution(tc, tg, ident_graph, ast.types, id); matches == 1) {
      binding_of.emplace(id, overload);
    } else if(matches == 0) {
      errors.push_back(
        {ast.srcs[id.get()],
         "no matching overload found",
         transform_to_vec(
           ident_graph.fanout(id),
           [&](ASTID id) { return fmt::format("  {}", pretty_print(srcs, tg, type_names, ast.types[id.get()])); },
           make_vector(fmt::format("deduced {} [{} candidate(s)]",
                                   pretty_print(srcs, tg, type_names, ast.types[id.get()]),
                                   ident_graph.fanout(id).size())))});
    } else {
      errors.push_back(
        {ast.srcs[id.get()],
         "ambiguous overload",
         transform_to_vec(
           ident_graph.fanout(id),
           [&](ASTID id) { return fmt::format("  {}", pretty_print(srcs, tg, type_names, ast.types[id.get()])); },
           make_vector(fmt::format(
             "deduced {} [{} candidate(s)]", pretty_print(srcs, tg, type_names, ast.types[id.get()]), matches)))});
    }
  }

  return value_or_errors(std::move(binding_of), std::move(errors), std::move(ast), std::move(tg));
}

ContextualResult<CallGraphData, AST, TypeGraph> create_call_graph_data(
  Span<std::string_view> srcs,
  const TypeCache& tc,
  const TypeNames& type_names,
  const Graph<ASTID>& ident_graph,
  AST ast,
  TypeGraph tg) {

  return global_binding_resolution(srcs, tc, type_names, ident_graph, std::move(ast), std::move(tg))
    .map([&](Map<ASTID, ASTID> binding_of, AST ast, TypeGraph tg) {
      std::vector<std::vector<ASTID>> call_graph_vec(ast.forest.size());

      for(const auto [id, overload] : binding_of) {
        const ASTID root = ast.forest.root(id);
        if(ast.forest[root] == ASTTag::Assignment && is_global(ast.forest, overload)) {
          call_graph_vec[ast.forest.first_child(root)->get()].push_back(overload);
        }
      }

      for(auto& v : call_graph_vec) {
        v = unique(sorted(std::move(v)));
      }

      return std::tuple(
        CallGraphData{Graph<ASTID>(call_graph_vec), std::move(binding_of)}, std::move(ast), std::move(tg));
    });
}

} // namespace

ContextualResult<void, TypeGraph>
type_name_resolution(Span<std::string_view> srcs,
                     const TypeNames& type_names,
                     const std::vector<std::pair<Type, SrcRef>>& type_srcs,
                     TypeGraph tg) {

  assert(std::is_sorted(type_names.begin(), type_names.end()));

  std::vector<ContextualError> errors;

  for(const auto& [t, src] : type_srcs) {
    if(const auto opt_id = find_id(type_names, sv(srcs, src)); opt_id) {
      tg.set<TypeID>(t, *opt_id);
    } else {
      errors.push_back({src, "undefined type"});
    }
  }

  return void_or_errors(std::move(errors), std::move(tg));
}

ContextualResult<Graph<ASTID>> calculate_ident_graph(Span<std::string_view> srcs, const AST& ast) {
  IdentGraphCtx ctx = {
    std::vector<std::vector<ASTID>>(ast.forest.size()), transform_filter_to_vec(ast.forest.root_ids(), [&](ASTID id) {
      return ast.forest[id] == ASTTag::Assignment
               ? std::optional(std::pair(
                   sv(srcs, ast.srcs[ast.forest.child_ids(id).get<0>().get()]), ast.forest.child_ids(id).get<0>()))
               : std::nullopt;
    })};

  for(const ASTID root : ast.forest.root_ids()) {
    calculate_ident_graph(ctx, root, srcs, ast);
  }

  return ctx.undeclared_bindings.empty()
           ? ContextualResult<Graph<ASTID>>{Graph<ASTID>(ctx.fanouts)}
           : ContextualResult<Graph<ASTID>>{Failure{transform_to_vec(ctx.undeclared_bindings, [&](ASTID id) {
               return ContextualError{
                 ast.srcs[id.get()], fmt::format("undeclared binding '{}'", sv(srcs, ast.srcs[id.get()]))};
             })}};
}

ContextualResult<CallGraphData, AST, TypeGraph>
sema(Span<std::string_view> srcs, const TypeCache& tc, const NativeTypeInfo& native_types, AST ast, TypeGraph tg) {
  return apply_language_rules(srcs, tc, native_types.names, std::move(ast), std::move(tg))
    .and_then([&](AST ast, TypeGraph tg) {
      return calculate_ident_graph(srcs, ast).append_state(std::move(ast), std::move(tg));
    })
    .map([&](Graph<ASTID> ig, AST ast, TypeGraph tg) {
      auto propagations = calculate_propagations(ig, ast.forest);
      return std::tuple(std::tuple(std::move(ig), std::move(propagations)), std::move(ast), std::move(tg));
    })
    .and_then(flattened([&](Graph<ASTID> ig, auto propagations, AST ast, TypeGraph tg) {
      return constraint_propagation(srcs, tc, native_types, ig, propagations, std::move(ast), std::move(tg))
        .map([&](AST ast, TypeGraph tg) {
          return std::tuple(std::tuple(std::move(ig), std::move(propagations)), std::move(ast), std::move(tg));
        });
    }))
    .and_then(flattened([&](Graph<ASTID> ig, auto propagations, AST ast, TypeGraph tg) {
      return create_call_graph_data(srcs, tc, native_types.names, ig, std::move(ast), std::move(tg))
        .map([&](CallGraphData cg, AST ast, TypeGraph tg) {
          return std::tuple(std::tuple(std::move(cg), std::move(propagations)), std::move(ast), std::move(tg));
        });
    }))
    .and_then(flattened([&](CallGraphData cg, auto propagations, AST ast, TypeGraph tg) {
      auto errors = check_fully_resolved(srcs, propagations, ast, tg, native_types.names);
      return value_or_errors(std::move(cg), std::move(errors), std::move(ast), std::move(tg));
    }));
}

} // namespace ooze
