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

template <typename IDRange>
IdentGraphCtx add_globals(Span<std::string_view> srcs, const AST& ast, IdentGraphCtx ctx, IDRange range) {
  ctx.globals = transform_filter_to_vec(
    range,
    [&](ASTID id) {
      return ast.forest[id] == ASTTag::Assignment
               ? std::optional(std::pair(
                   sv(srcs, ast.srcs[ast.forest.child_ids(id).get<0>().get()]), ast.forest.child_ids(id).get<0>()))
               : std::nullopt;
    },
    std::move(ctx.globals));
  return ctx;
}

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
  case ASTTag::ExprQualified:
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
    if(!is_global(ast.forest, id)) {
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
  case ASTTag::Module: {
    const size_t initial_size = ctx.globals.size();
    ctx = add_globals(srcs, ast, std::move(ctx), ast.forest.child_ids(id));
    for(const ASTID child : ast.forest.child_ids(id)) {
      calculate_ident_graph(ctx, child, srcs, ast);
    }
    ctx.globals.erase(ctx.globals.begin() + i64(initial_size), ctx.globals.end());
    break;
  }
  case ASTTag::ModuleRef: break;
  };
}

struct InternalSemaState {
  std::vector<ASTID> roots;
  Map<ASTID, ASTID> overloads;
  std::vector<ASTID> resolved_roots;
  std::vector<ASTID> generic_roots;
};

std::tuple<InternalSemaState, AST> instantiate(OverloadResolutionData ord, InternalSemaState s, AST ast) {
  s.overloads.insert(ord.overloads.begin(), ord.overloads.end());

  const auto recurse = [&](auto self, ASTID id) -> void {
    if(ast.forest[id] == ASTTag::Module) {
      for(const ASTID child : ast.forest.child_ids(id)) {
        self(self, child);
      }
    } else if(is_resolved(ast.tg, global_type(ast, id))) {
      s.resolved_roots.push_back(id);
    } else {
      s.generic_roots.push_back(id);
    }
  };

  for(const ASTID id : s.roots) {
    recurse(recurse, id);
  }

  s.roots.clear();

  for(const auto& [overload, type, uses] : ord.instantiations) {
    const ASTID root = *ast.forest.parent(overload);
    assert(ast.forest.is_root(root));

    const auto tree_size = distance(ast.forest.post_order_ids(root));

    ast.types.resize(ast.forest.size() + tree_size, Type::Invalid());
    ast.srcs.resize(ast.forest.size() + tree_size, SrcRef{});

    const ASTID copy = copy_tree(ast.forest, root, ast.forest, [&](ASTID old_id, ASTID new_id) {
      // TODO literals
      ast.types[new_id.get()] = ast.types[old_id.get()];
      ast.srcs[new_id.get()] = ast.srcs[old_id.get()];
    });
    const ASTID instantiation = *ast.forest.first_child(copy);

    ast.types[instantiation.get()] = type;

    s.roots.push_back(copy);

    for(const ASTID use : uses) {
      s.overloads.emplace(use, instantiation);
    }
  }

  return std::tuple(std::move(s), std::move(ast));
}

ContextualResult<InternalSemaState, AST> sema_iter(
  Span<std::string_view> srcs, const TypeCache& tc, const NativeTypeInfo& native_types, AST ast, InternalSemaState s) {
  return apply_language_rules(tc, native_types.names, std::move(ast), s.roots)
    .and_then([&](AST ast) { return calculate_ident_graph(srcs, ast, s.roots).append_state(std::move(ast)); })
    .and_then([&](Graph<ASTID> ig, AST ast) {
      return constraint_propagation(srcs, tc, native_types, ig, std::move(ast), s.roots);
    })
    .map([&](OverloadResolutionData ord, AST ast) {
      return instantiate(std::move(ord), std::move(s), std::move(ast));
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

ContextualResult<Graph<ASTID>> calculate_ident_graph(Span<std::string_view> srcs, const AST& ast, Span<ASTID> roots) {
  IdentGraphCtx ctx =
    add_globals(srcs, ast, IdentGraphCtx{std::vector<std::vector<ASTID>>(ast.forest.size())}, ast.forest.root_ids());

  for(const ASTID root : roots) {
    calculate_ident_graph(ctx, root, srcs, ast);
  }

  return ctx.undeclared_bindings.empty()
           ? ContextualResult<Graph<ASTID>>{Graph<ASTID>(ctx.fanouts)}
           : ContextualResult<Graph<ASTID>>{Failure{transform_to_vec(ctx.undeclared_bindings, [&](ASTID id) {
               return ContextualError{
                 ast.srcs[id.get()], fmt::format("undeclared binding '{}'", sv(srcs, ast.srcs[id.get()]))};
             })}};
}

ContextualResult<SemaData, AST>
sema(Span<std::string_view> srcs, const TypeCache& tc, const NativeTypeInfo& native_types, AST ast, Span<ASTID> roots) {
  ContextualResult<InternalSemaState, AST> res{InternalSemaState{to_vec(roots)}, std::move(ast)};
  while(res && !res.value().roots.empty()) {
    res = std::move(res).and_then([&](InternalSemaState s, AST ast) {
      return sema_iter(srcs, tc, native_types, std::move(ast), std::move(s));
    });
  }

  return std::move(res).map([](InternalSemaState s, AST ast) {
    return std::tuple(SemaData{std::move(s.overloads), std::move(s.resolved_roots), std::move(s.generic_roots)},
                      std::move(ast));
  });
}

} // namespace ooze
