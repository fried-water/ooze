#include "pch.h"

#include "bindings.h"
#include "frontend.h"
#include "function_graph_construction.h"
#include "parser.h"
#include "parser_combinators.h"
#include "pretty_print.h"
#include "runtime.h"
#include "sema.h"
#include "type_check.h"
#include "user_msg.h"

#include "ooze/core.h"

namespace ooze {

namespace {

bool is_binding_copyable(const TypeGraph& tg, const std::unordered_set<TypeID>& copy_types, Type type) {
  bool is_copyable = true;

  preorder(tg, type, [&](Type t) {
    switch(tg.get<TypeTag>(t)) {
    case TypeTag::Leaf:
      is_copyable = is_copyable && copy_types.find(tg.get<TypeID>(t)) != copy_types.end();
      return false;
    case TypeTag::Fn: return false;
    case TypeTag::Tuple: return true;
    case TypeTag::Floating:
    case TypeTag::Borrow: assert(false); return false;
    }
    assert(false);
    return true;
  });

  return is_copyable;
}

Type copy_type(Env& env, const TypeGraph& tg, Type type) {
  return tg.get<TypeTag>(type) == TypeTag::Leaf
           ? env.ast.tg.add_node(TypeTag::Leaf, tg.get<TypeID>(type))
           : env.ast.tg.add_node(
               transform_to_vec(tg.fanout(type), [&](Type fanout) { return copy_type(env, tg, fanout); }),
               tg.get<TypeTag>(type),
               tg.get<TypeID>(type));
}

std::tuple<std::vector<AsyncValue>, Map<ASTID, std::vector<AsyncValue>>> run_function(
  const AST& ast,
  const Map<ASTID, ASTID>& binding_of,
  const std::unordered_set<TypeID>& copy_types,
  const std::unordered_map<ASTID, Inst>& functions,
  Program p,
  ExecutorRef ex,
  Map<ASTID, std::vector<AsyncValue>> bindings,
  ASTID expr_id) {
  assert(is_expr(ast.forest[expr_id]));

  auto [p2, value_inputs, borrow_inputs, fg] = create_graph(std::move(p), ast, copy_types, binding_of, expr_id);
  const Inst graph_inst = p2.add(std::move(fg));

  std::vector<BorrowedFuture> borrowed;
  for(const ASTID id : borrow_inputs) {
    const auto it = bindings.find(id);
    assert(it != bindings.end());
    borrowed = transform_to_vec(
      it->second, [](AsyncValue& v) { return borrow(v); }, std::move(borrowed));
  }

  std::vector<Future> futures;
  for(const ASTID id : value_inputs) {
    if(const auto it = bindings.find(id); it == bindings.end()) {
      const auto fn_it = functions.find(id);
      assert(fn_it != functions.end());
      futures.emplace_back(Any(fn_it->second));
    } else if(is_binding_copyable(ast.tg, copy_types, ast.types[id.get()])) {
      futures = transform_to_vec(
        it->second, [](AsyncValue& v) { return borrow(v).then([](const Any& a) { return a; }); }, std::move(futures));
    } else {
      futures = transform_to_vec(
        std::move(it->second), [](AsyncValue v) { return take(std::move(v)); }, std::move(futures));
      bindings.erase(it);
    }
  }

  return std::tuple(
    transform_to_vec(
      execute(std::make_shared<Program>(std::move(p2)), graph_inst, ex, std::move(futures), std::move(borrowed)),
      Construct<AsyncValue>{}),
    std::move(bindings));
}

auto assign_values(
  const AST& ast, Map<ASTID, std::vector<AsyncValue>> bindings, std::vector<AsyncValue> values, ASTID pattern) {
  int offset = 0;
  for(const ASTID id : ast.forest.leaf_ids(pattern)) {
    const int size = size_of(ast.tg, ast.types[id.get()]);
    if(ast.forest[id] == ASTTag::PatternIdent) {
      bindings.emplace(id,
                       std::vector<AsyncValue>(std::make_move_iterator(values.begin() + offset),
                                               std::make_move_iterator(values.begin() + offset + size)));
    }
    offset += size;
  }
  assert(offset == values.size());
  return bindings;
}

Env generate_functions(Span<std::string_view> srcs, Env env, const AST& ast, const Map<ASTID, ASTID>& overloads) {
  Map<ASTID, ASTID> to_env_id;
  std::vector<std::pair<ASTID, Inst>> fns;

  for(const ASTID root : ast.forest.root_ids()) {
    assert(ast.forest[root] == ASTTag::Assignment || ast.forest[root] == ASTTag::Module);
    if(ast.forest[root] == ASTTag::Module) continue;
    const auto [ident_id, value_id] = ast.forest.child_ids(root).take<2>();

    if(ast.forest[value_id] == ASTTag::Fn) {
      const Inst inst = env.program.placeholder();
      fns.emplace_back(value_id, inst);
      to_env_id.emplace(
        ident_id,
        env.add_function(sv(srcs, ast.srcs[ident_id.get()]), copy_type(env, ast.tg, ast.types[ident_id.get()]), inst));
    } else {
      to_env_id.emplace(ident_id, ident_id);
    }
  }

  for(const auto [id, inst] : fns) {
    FunctionGraphData fg_data = create_graph(std::move(env.program), ast, env.native_types.copyable, overloads, id);

    env.program = std::move(fg_data.program);

    assert(fg_data.captured_borrows.empty());

    if(fg_data.captured_values.empty()) {
      env.program.set(inst, std::move(fg_data.graph));
    } else {
      std::vector<Any> values = transform_to_vec(fg_data.captured_values, [&](ASTID id) {
        const auto it = to_env_id.find(id);
        assert(it != to_env_id.end());
        const auto fn_it = env.functions.find(it->second);
        assert(fn_it != env.functions.end());
        return Any(fn_it->second);
      });

      const Inst graph_inst = env.program.add(std::move(fg_data.graph));
      env.program.set(inst, graph_inst, std::move(values));
    }
  }

  return env;
}

std::tuple<std::string, AST, std::vector<ASTID>, Map<ASTID, std::vector<AsyncValue>>>
append_global_bindings(std::string env_src, AST ast, Bindings str_bindings) {
  Map<ASTID, std::vector<AsyncValue>> bindings;
  std::vector<ASTID> roots;
  roots.reserve(str_bindings.size());
  for(auto& [name, binding] : str_bindings) {
    const ASTID pattern = add_global(ast, SrcRef{SrcID{0}, append_src(env_src, name)}, binding.type);
    bindings.emplace(pattern, std::move(binding.values));
    roots.push_back(*ast.forest.parent(pattern));
  }

  return {std::move(env_src), std::move(ast), std::move(roots), std::move(bindings)};
}

std::tuple<Env, Bindings>
to_str_bindings(Span<std::string_view> srcs, const AST& ast, Env env, Map<ASTID, std::vector<AsyncValue>> bindings) {
  Bindings str_bindings;
  for(auto& [id, values] : bindings) {
    str_bindings.emplace(std::string(sv(srcs, ast.srcs[id.get()])),
                         Binding{copy_type(env, ast.tg, ast.types[id.get()]), std::move(values)});
  }
  return {std::move(env), std::move(str_bindings)};
}

auto run_or_assign(ExecutorRef ex,
                   const AST& ast,
                   const Map<ASTID, ASTID>& binding_of,
                   Env env,
                   Map<ASTID, std::vector<AsyncValue>> bindings,
                   ASTID id) {
  assert(is_global(ast.forest, id));

  const bool expr = is_expr(ast.forest[id]);
  const Type type = copy_type(env, ast.tg, ast.types[id.get()]);

  std::vector<AsyncValue> values;
  std::tie(values, bindings) = run_function(
    ast,
    binding_of,
    env.native_types.copyable,
    env.functions,
    env.program,
    ex,
    std::move(bindings),
    expr ? id : ast.forest.child_ids(id).get<1>());

  return expr ? std::tuple(Binding{type, std::move(values)}, std::move(env), std::move(bindings))
              : std::tuple(Binding{type},
                           std::move(env),
                           assign_values(ast, std::move(bindings), std::move(values), *ast.forest.first_child(id)));
}

} // namespace

StringResult<void> type_check_expr(const Env& env, std::string_view expr) {
  const auto srcs = make_sv_array(env.src, expr);
  return frontend(parse_expr, srcs, env.native_types, env.ast)
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void> type_check_fn(const Env& env, std::string_view fn) {
  const auto srcs = make_sv_array(env.src, fn);
  return frontend(parse_fn, srcs, env.native_types, env.ast)
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void> type_check_binding(const Env& env, std::string_view binding) {
  const auto srcs = make_sv_array(env.src, binding);
  return parse_and_name_resolution(parse_binding, srcs, env.native_types.names, env.ast, SrcID{1})
    .and_then([&](ASTID pattern_root, AST ast) {
      if(ast.forest[pattern_root] != ASTTag::PatternIdent) {
        return ContextualResult<Map<ASTID, ASTID>, AST>{
          Failure{make_vector(ContextualError{ast.srcs[pattern_root.get()], "not a binding"})}, std::move(ast)};
      } else {
        // Type check it as if it were an expr with the given type
        ast.forest[pattern_root] = ASTTag::ExprIdent;
        const TypeCache tc = create_type_cache(ast.tg);
        return sema(srcs, tc, env.native_types, std::move(ast), std::array{pattern_root});
      }
    })
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void, Env> parse_scripts(Env env, Span<std::string_view> files) {
  const std::string env_src = env.src;
  const auto srcs = flatten(make_sv_array(env_src), files);

  return accumulate_result(
           id_range(SrcID(1), SrcID(int(srcs.size()))),
           ContextualResult<ParserResult<std::vector<ASTID>>, AST>{ParserResult<std::vector<ASTID>>{}, env.ast},
           [&srcs](SrcID src, AST ast) { return parse(std::move(ast), src, srcs[src.get()]); },
           [](auto x, auto y) {
             return ParserResult<std::vector<ASTID>>{
               to_vec(std::move(y.parsed), std::move(x.parsed)),
               to_vec(std::move(y.type_srcs), std::move(x.type_srcs)),
             };
           },
           [](auto x, auto y) { return to_vec(std::move(y), std::move(x)); })
    .and_then([&](ParserResult<std::vector<ASTID>> pr, AST ast) {
      return type_name_resolution(srcs, env.native_types.names, std::move(pr), std::move(ast));
    })
    .and_then([&](std::vector<ASTID> roots, AST ast) {
      const TypeCache tc = create_type_cache(ast.tg);
      return sema(srcs, tc, env.native_types, std::move(ast), roots);
    })
    .append_state(std::move(env))
    .map([&](Map<ASTID, ASTID> overloads, AST ast, Env env) {
      env = generate_functions(srcs, std::move(env), ast, overloads);
      return std::tuple(std::move(ast), std::move(env));
    })
    .map_state([](AST, Env env) { return env; })
    .map_error([&srcs](auto errors, Env env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

StringResult<Binding, Env, Bindings> run(ExecutorRef ex, Env env, Bindings str_bindings, std::string_view expr) {
  auto [env_src, ast, _binding_roots, bindings] = append_global_bindings(env.src, env.ast, std::move(str_bindings));
  auto binding_roots = std::move(_binding_roots);

  const SrcRef runtime_src = {SrcID{0}, append_src(env_src, "#runtime")};
  const SrcRef current_src = {SrcID{0}, append_src(env_src, "#current")};

  const auto srcs = make_sv_array(env_src, expr);

  return parse_and_name_resolution(parse_repl, srcs, env.native_types.names, std::move(ast), SrcID{1})
    .and_then([&](const ASTID root, AST ast) {
      const TypeCache tc = create_type_cache(ast.tg);

      const ASTID current_mod = append_root(ast, ASTTag::Module, current_src, tc.unit, as_span(root));
      binding_roots.push_back(current_mod);

      const ASTID runtime_mod = append_root(ast, ASTTag::Module, runtime_src, tc.unit, binding_roots);

      return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod})
        .map([&](Map<ASTID, ASTID> overloads, AST ast) {
          return std::tuple(std::tuple(std::move(overloads), root), std::move(ast));
        });
    })
    .append_state(std::move(env), std::move(bindings))
    .map(flattened(
      [&](Map<ASTID, ASTID> overloads, ASTID expr, AST ast, Env env, Map<ASTID, std::vector<AsyncValue>> bindings) {
        Binding result;
        std::tie(result, env, bindings) = run_or_assign(ex, ast, overloads, std::move(env), std::move(bindings), expr);
        return std::tuple(std::move(result), std::move(ast), std::move(env), std::move(bindings));
      }))
    .map_state([&](AST ast, Env env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map_error([&](auto errors, Env env, Bindings bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });
}

StringResult<Future, Env, Bindings>
run_to_string(ExecutorRef ex, Env env, Bindings str_bindings, std::string_view expr) {
  auto [env_src, ast, _binding_roots, bindings] = append_global_bindings(env.src, env.ast, std::move(str_bindings));
  auto binding_roots = std::move(_binding_roots);

  const SrcRef runtime_ref = {SrcID{0}, append_src(env_src, "#runtime")};
  const SrcRef current_ref = {SrcID{0}, append_src(env_src, "#current")};
  const SrcRef to_string_ref = {SrcID{0}, append_src(env_src, "to_string")};

  const auto srcs = make_sv_array(env_src, expr);

  return parse_and_name_resolution(parse_repl, srcs, env.native_types.names, std::move(ast), SrcID{1})
    .and_then([&](const ASTID root, AST ast) {
      const TypeCache tc = create_type_cache(ast.tg);

      binding_roots.push_back(append_root(ast, ASTTag::Module, current_ref, tc.unit, as_span(root)));
      const ASTID runtime_mod = append_root(ast, ASTTag::Module, runtime_ref, tc.unit, binding_roots);
      binding_roots.pop_back();

      if(ast.forest[root] == ASTTag::Assignment) {
        return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod})
          .map([&](Map<ASTID, ASTID> overloads, AST ast) {
            return std::tuple(std::tuple(std::move(overloads), root), std::move(ast));
          });
      } else {
        return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod}).and_then([&](auto, AST ast) {
          pop_last_root(ast);
          pop_last_root(ast);

          const Type expr_type = ast.types[root.get()];
          const Type borrow_type = ast.tg.add_node(std::array{expr_type}, TypeTag::Borrow, TypeID{});
          const Type tuple_type = ast.tg.add_node(std::array{borrow_type}, TypeTag::Tuple, TypeID{});
          const Type string_type = ast.tg.add_node(TypeTag::Leaf, type_id(knot::Type<std::string>{}));
          const Type fn_type = ast.tg.add_node(std::array{tuple_type, string_type}, TypeTag::Fn, TypeID{});

          const ASTID borrow_id = append_root(ast, ASTTag::ExprBorrow, SrcRef{}, borrow_type, std::array{root});
          const ASTID tuple_id = append_root(ast, ASTTag::ExprTuple, SrcRef{}, tuple_type, std::array{borrow_id});
          const ASTID callee_id = append_root(ast, ASTTag::ExprIdent, to_string_ref, fn_type);
          const ASTID call_id =
            append_root(ast, ASTTag::ExprCall, SrcRef{}, string_type, std::array{callee_id, tuple_id});

          binding_roots.push_back(append_root(ast, ASTTag::Module, current_ref, tc.unit, std::array{call_id}));

          const ASTID runtime_mod = append_root(ast, ASTTag::Module, runtime_ref, tc.unit, binding_roots);

          return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod})
            .map([&](Map<ASTID, ASTID> overloads, AST ast) {
              return std::tuple(std::tuple(std::move(overloads), call_id), std::move(ast));
            });
        });
      }
    })
    .append_state(std::move(env), std::move(bindings))
    .map(flattened(
      [&](Map<ASTID, ASTID> overloads, ASTID expr, AST ast, Env env, Map<ASTID, std::vector<AsyncValue>> bindings) {
        Binding result;
        std::tie(result, env, bindings) = run_or_assign(ex, ast, overloads, std::move(env), std::move(bindings), expr);
        return std::tuple(std::move(result), std::move(ast), std::move(env), std::move(bindings));
      }))
    .map_state([&](AST ast, Env env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map([](Binding b, Env env, Bindings bindings) {
      assert(b.values.size() <= 1);
      assert(b.values.empty() || env.ast.tg.get<TypeID>(b.type) == type_id(knot::Type<std::string>{}));
      return std::tuple(b.values.size() == 1 ? take(std::move(b.values[0])) : Future(Any(std::string())),
                        std::move(env),
                        std::move(bindings));
    })
    .map_error([&](auto errors, Env env, Bindings bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });
}

} // namespace ooze
