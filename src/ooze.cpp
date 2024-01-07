#include "pch.h"

#include "async_functions.h"
#include "bindings.h"
#include "function_graph_construction.h"
#include "io.h"
#include "ooze/core.h"
#include "ooze/executor/task_executor.h"
#include "parser.h"
#include "parser_combinators.h"
#include "parser_flat.h"
#include "pretty_print.h"
#include "repl.h"
#include "sema.h"
#include "type_check.h"
#include "user_msg.h"

namespace ooze {

namespace {
struct Command {
  bool run_main = false;
  std::vector<std::string> filenames;
};

std::optional<Command> parse_cmd_line(int argc, const char** argv) {
  if(argc <= 1) {
    return Command{};
  } else {
    const std::string_view cmd = argv[1];

    std::vector<std::string> filenames;
    for(int i = 2; i < argc; i++) {
      filenames.push_back(argv[i]);
    }

    if(cmd == "run") {
      return Command{true, std::move(filenames)};
    } else if(cmd == "repl") {
      return Command{false, std::move(filenames)};
    } else {
      return std::nullopt;
    }
  }
}

bool is_binding_copyable(const TypeGraph& tg, const std::unordered_set<TypeID>& copy_types, TypeRef type) {
  bool is_copyable = true;

  preorder(tg, type, [&](TypeRef t) {
    switch(tg.get<TypeTag>(t)) {
    case TypeTag::Leaf:
      is_copyable = is_copyable && copy_types.find(tg.get<TypeID>(t)) != copy_types.end();
      return false;
    case TypeTag::Fn: return false;
    case TypeTag::Floating: assert(false); return false;
    case TypeTag::Borrow: assert(false); return false;
    case TypeTag::Tuple: return true;
    }
    assert(false);
    return true;
  });

  return is_copyable;
}

StringResult<void, Env> parse_scripts(Env e, const std::vector<std::string>& filenames) {
  return knot::accumulate(
    filenames, StringResult<void, Env>{std::move(e)}, [&](auto result, const std::string& filename) {
      return std::move(result)
        .and_then([&](Env e) { return read_text_file(filename).append_state(std::move(e)); })
        .and_then([&](const std::string& script, Env e) { return parse_script(std::move(e), script); });
    });
}

std::vector<std::string> gather_binding_strings(std::vector<Binding> bindings) {
  return transform_to_vec(std::move(bindings), [](Binding b) {
    return any_cast<std::string>(take(std::move(b)).wait());
  });
}

TypedFunction lift_only_borrowed_idents(TypedFunction f) {
  const auto count_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(f.expr, 0, [&](int acc, const TypedExpr& e) {
      return knot::accumulate(e.v, acc, [&](int acc, const ast::Ident& i) { return i.name == name ? acc + 1 : acc; });
    });
  };

  const auto get_borrowed_usages = [&](const std::string& name) {
    return knot::preorder_accumulate(f.expr, std::vector<TypedExpr*>{}, [&](auto acc, TypedExpr& e) {
      knot::visit(e.v, [&](const TypedBorrowExpr& b) {
        knot::visit(b.expr->v, [&](const ast::Ident& i) {
          if(i.name == name) {
            acc.push_back(&e);
          }
        });
      });
      return acc;
    });
  };

  co_visit(f.pattern, f.pattern.type, [&](const TypedPattern&, Type<TypeID>& t, const ast::Ident& i, const auto&) {
    const int uses = count_usages(i.name);
    auto borrows = get_borrowed_usages(i.name);
    if(uses == borrows.size()) {
      t = borrow_type(std::move(t));
      for(TypedExpr* e : borrows) {
        *e = TypedExpr{ast::Ident{i.name}};
      }
    }
  });

  return f;
}

ContextualResult<TypedFunction> infer_header_from_env(const Env& env, const Bindings& bindings, TypedExpr expr) {
  Set<std::string> active;
  for(const auto& [name, fn] : env.functions) {
    if(bindings.find(name) == bindings.end()) {
      active.insert(name);
    }
  }

  TypedPattern pattern = inferred_inputs(expr, std::move(active));

  std::vector<ContextualError> errors;

  co_visit(
    pattern,
    pattern.type,
    Overloaded{[](TypedPattern&, Type<TypeID>&, const ast::WildCard& pattern, const auto& type) { assert(false); },
               [&](TypedPattern& p, Type<TypeID>& t, const ast::Ident& ident, const auto&) {
                 if(const auto it = bindings.find(ident.name); it != bindings.end()) {
                   t = type(it->second);
                 } else {
                   errors.push_back({p.ref, fmt::format("use of undeclared binding '{}'", ident.name)});
                 }
               }});

  return value_or_errors(TypedFunction{std::move(pattern), std::move(expr)}, std::move(errors));
}

bool is_instantiated(const Env& e, const EnvFunctionRef& call, const FunctionType<TypeID>& type) {
  const EnvFunction& ef = e.functions.at(call.name)[call.overload_idx];
  return type == ef.type || any_of(ef.instatiations, [&](const auto& p) { return type == p.first; });
}

std::vector<std::pair<EnvFunctionRef, FunctionType<TypeID>>>
uninstantiated_functions(const Env& e, const CheckedFunction& f) {
  return knot::preorder_accumulate(
    f, std::vector<std::pair<EnvFunctionRef, FunctionType<TypeID>>>{}, [&](auto acc, const CheckedExpr& expr) {
      knot::visit(expr.v, [&](const EnvFunctionRef& call) {
        const auto& ftype = std::get<FunctionType<TypeID>>(expr.type.v);
        if(!is_instantiated(e, call, ftype)) {
          acc.emplace_back(call, ftype);
        }
      });

      return acc;
    });
}

ContextualResult<FunctionGraph, Env> create_graph_and_instantiations(Env e, const CheckedFunction& f) {
  std::vector<ContextualError> errors;

  std::vector<std::pair<EnvFunctionRef, FunctionType<TypeID>>> typed_functions = uninstantiated_functions(e, f);
  std::vector<std::tuple<EnvFunctionRef, FunctionType<TypeID>, CheckedFunction>> checked_functions;

  Set<EnvFunctionRef> visited;

  for(size_t i = 0; i < typed_functions.size(); i++) {
    const auto& [call, type] = typed_functions[i];
    if(visited.insert(call).second) {
      const EnvFunction& ef = e.functions.at(call.name)[call.overload_idx];
      auto result = type_check(e, std::get<TypedFunction>(ef.f), type).and_then([&](const TypedFunction& f) {
        return overload_resolution(e, f);
      });

      if(result) {
        typed_functions = to_vec(uninstantiated_functions(e, *result), std::move(typed_functions));
        checked_functions.emplace_back(
          std::move(typed_functions[i].first), std::move(typed_functions[i].second), std::move(*result));
      } else {
        errors = to_vec(std::move(result.error()), std::move(errors));
      }
    }
  }

  if(errors.empty()) {
    for(int i = checked_functions.size() - 1; i >= 0; i--) {
      const auto& [call, type, f] = checked_functions[i];
      e.functions.at(call.name)[call.overload_idx].instatiations.emplace_back(type, create_graph(e, f));
    }

    return ContextualResult<FunctionGraph>{create_graph(e, f)}.append_state(std::move(e));
  } else {
    return {Failure{std::move(errors)}, std::move(e)};
  }
}

ContextualResult<void, Env> add_function(Env env, const std::string& name, TypedFunction f) {
  if(auto checked_result = overload_resolution(env, f); checked_result.has_value()) {
    return create_graph_and_instantiations(std::move(env), checked_result.value()).map([&](FunctionGraph g, Env env) {
      env.functions[name].push_back({{std::move(f.pattern.type), std::move(f.expr.type)}, std::move(g)});
      return env;
    });
  } else {
    env.functions[name].push_back({{f.pattern.type, f.expr.type}, std::move(f)});
    return ContextualResult<void, Env>{std::move(env)};
  }
}

ContextualResult<Tree<Binding>, Env, Bindings>
run_function(ExecutorRef executor, Env e, Bindings bindings, const CheckedFunction& f) {
  return create_graph_and_instantiations(std::move(e), f)
    .append_state(std::move(bindings))
    .map([&](FunctionGraph g, Env e, Bindings bindings) {
      std::vector<Future> value_inputs;
      std::vector<BorrowedFuture> borrowed_inputs;

      co_visit(f.pattern,
               f.pattern.type,
               Overloaded{[&](auto&, auto&, const ast::Ident& i, const auto&) {
                            value_inputs = to_vec(*take(bindings, i.name), std::move(value_inputs));
                          },
                          [&](auto&, auto&, const ast::Ident& i, const BorrowType<TypeID>&) {
                            borrowed_inputs = to_vec(*borrow(bindings, i.name), std::move(borrowed_inputs));
                          }});

      std::vector<Future> futures =
        create_async_graph(g)(executor, std::move(value_inputs), std::move(borrowed_inputs));

      int idx = 0;
      const auto converter = Overloaded{
        // result vector should be in order of a preorder traversal of the leaves in the output type
        [&](TypeID t) {
          return Binding{{t}, std::move(futures[idx++])};
        },
        [&](const FunctionType<TypeID>& t) {
          return Binding{{t}, std::move(futures[idx++])};
        },

        // These can't be part of the output type of executed functions
        [](const FloatingType&) -> Binding {
          assert(false);
          exit(1);
        },
        [](const BorrowType<TypeID>&) -> Binding {
          assert(false);
          exit(1);
        }};

      return std::tuple(knot::map<Tree<Binding>>(f.expr.type, std::cref(converter)), std::move(e), std::move(bindings));
    });
}

ContextualResult<Tree<Binding>, Env, Bindings>
run_expr(ExecutorRef executor, Env env, Bindings bindings, TypedExpr expr) {
  return infer_header_from_env(env, bindings, std::move(expr))
    .map(lift_only_borrowed_idents)
    .and_then([&](TypedFunction f) { return type_check(env, std::move(f)); })
    .and_then([&](const TypedFunction& f) { return overload_resolution(env, f); })
    .append_state(std::move(env), std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    });
}

ContextualResult<std::string, Env, Bindings>
run_expr_to_string(ExecutorRef executor, Env env, Bindings bindings, TypedExpr expr) {
  return infer_header_from_env(env, bindings, std::move(expr))
    .map(lift_only_borrowed_idents)
    .and_then([&](TypedFunction f) { return type_check(env, std::move(f)); })
    .and_then([&](TypedFunction f) {
      auto result = overload_resolution(env, f);
      return result ? ContextualResult<TypedFunction>{std::move(f)} : Failure{std::move(result.error())};
    })
    .map([&](TypedFunction f) {
      TypedScopeExpr scope;

      const int args = std::visit(
        Overloaded{[](const std::vector<Type<TypeID>>& v) { return int(v.size()); }, [](const auto&) { return 0; }},
        f.pattern.type.v);

      if(std::holds_alternative<ast::Ident>(f.expr.v) && args == 1) {
        Type<TypeID> ident_type = f.expr.type;

        knot::preorder(f,
                       Overloaded{
                         [](TypedPattern& p) { p.type = floating_type<TypeID>(); },
                         [](TypedExpr& p) { p.type = floating_type<TypeID>(); },
                       });

        scope.assignments.push_back({{ast::Ident{"x"}, borrow_type(std::move(ident_type))}, std::move(f.expr)});
        scope.result =
          TypedExpr{TypedCallExpr{{{ast::Ident{"to_string"}}}, {{std::vector{TypedExpr{ast::Ident{"x"}}}}}}};
      } else {
        scope.assignments.push_back({{ast::Ident{"x"}, f.expr.type}, std::move(f.expr)});
        scope.result = TypedExpr{TypedCallExpr{
          {{ast::Ident{"to_string"}}}, {{std::vector{TypedExpr{TypedBorrowExpr{TypedExpr{ast::Ident{"x"}}}}}}}}};
      }

      f.expr.type = leaf_type(type_id(knot::Type<std::string>{}));
      f.expr.v = std::move(scope);

      return f;
    })
    .and_then([&](TypedFunction f) { return type_check(env, std::move(f)); })
    .and_then([&](TypedFunction f) { return overload_resolution(env, std::move(f)); })
    .append_state(std::move(env), std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    })
    .map([](Tree<Binding> t, Env env, Bindings b) {
      std::vector<Future> futures = take(std::move(t));
      return std::tuple(any_cast<std::string>(std::move(futures[0]).wait()), std::move(env), std::move(b));
    });
}

ContextualResult<void, Env, Bindings> run_assign(ExecutorRef executor, Env env, Bindings bindings, TypedAssignment a) {
  return infer_header_from_env(env, bindings, std::move(*a.expr))
    .map(lift_only_borrowed_idents)
    .and_then([&](TypedFunction f) { return type_check(env, std::move(f)); })
    .and_then([&](TypedFunction f) { return overload_resolution(env, std::move(f)); })
    .and_then([&](CheckedFunction f) {
      auto result = type_check(env, a.pattern, f.expr.type);
      return result ? ContextualResult<CheckedFunction>{std::move(f)} : Failure{std::move(result.error())};
    })
    .append_state(std::move(env), std::move(bindings))
    .and_then([&](CheckedFunction f, Env env, Bindings b) {
      return run_function(executor, std::move(env), std::move(b), f);
    })
    .map([&](Tree<Binding> results, Env env, Bindings bindings) {
      co_visit(a.pattern, results, [&](const TypedPattern&, Tree<Binding>& tree, const ast::Ident& ident, const auto&) {
        bindings[ident.name] = std::move(tree);
      });
      return std::tuple(std::move(env), std::move(bindings));
    });
}

struct TypeOfBindingConverter {
  Type<TypeID> operator()(const Tree<Binding>& tree) const {
    return std::visit(Overloaded{[&](const std::vector<Tree<Binding>>& v) {
                                   return Type<TypeID>{knot::map<std::vector<Type<TypeID>>>(v, *this)};
                                 },
                                 [&](const Binding& b) { return b.type; }},
                      tree.v);
  }
};

TypeRef copy_type(Span<std::string_view> srcs, Env& env, Map<TypeRef, TypeRef>& m, const TypeGraph& tg, TypeRef type) {
  if(const auto it = m.find(type); it != m.end()) {
    return it->second;
  } else if(tg.get<TypeTag>(type) == TypeTag::Leaf) {
    const TypeID type_id = tg.get<TypeID>(type);
    const auto it = env.type_cache.native.find(type_id);
    return it != env.type_cache.native.end()
             ? it->second.first
             : env.tg.add_node(
                 TypeTag::Leaf, SrcRef{SrcID{0}, append_src(env.src, sv(srcs, tg.get<SrcRef>(type)))}, type_id);
  } else {
    std::vector<TypeRef> children =
      transform_to_vec(tg.fanout(type), [&](TypeRef fanout) { return copy_type(srcs, env, m, tg, fanout); });

    const TypeRef new_type = env.tg.add_node(children, tg.get<TypeTag>(type), SrcRef{}, tg.get<TypeID>(type));
    m.emplace(type, new_type);
    return new_type;
  }
}

std::tuple<std::vector<Binding>, Map<ASTID, std::vector<Binding>>> run_function(
  Span<std::string_view> srcs,
  const AST& ast,
  const TypeGraph& tg,
  const Map<ASTID, ASTID>& binding_of,
  const std::unordered_set<TypeID>& copy_types,
  const std::unordered_map<ASTID, AsyncFn>& flat_functions,
  ExecutorRef ex,
  Map<ASTID, std::vector<Binding>> bindings,
  ASTID expr_id) {
  assert(is_expr(ast.forest[expr_id]));

  auto [value_inputs, borrow_inputs, fg] = create_graph(ast, tg, copy_types, binding_of, expr_id);

  std::vector<BorrowedFuture> borrowed;
  for(ASTID id : borrow_inputs) {
    if(const auto it = bindings.find(id); it != bindings.end()) {
      borrowed = transform_to_vec(
        it->second, [](Binding& b) { return borrow(b); }, std::move(borrowed));
    } else {
      borrowed.push_back(borrow(Future(Any(flat_functions.at(id)))).first);
    }
  }

  std::vector<Future> futures;
  for(ASTID id : value_inputs) {
    const auto it = bindings.find(id);
    assert(it != bindings.end());

    if(is_binding_copyable(tg, copy_types, ast.types[id.get()])) {
      for(Binding& b : it->second) {
        futures = transform_to_vec(
          it->second, [](Binding& b) { return borrow(b).then([](const Any& a) { return a; }); }, std::move(futures));
      }
    } else {
      futures = transform_to_vec(
        std::move(it->second), [](Binding b) { return take(std::move(b)); }, std::move(futures));
      bindings.erase(it);
    }
  }

  AsyncFn fn = create_async_graph(std::move(fg));

  return std::tuple(transform_to_vec(fn(ex, std::move(futures), std::move(borrowed)),
                                     [](Future f) {
                                       return Binding{floating_type<TypeID>(), std::move(f)};
                                     }),
                    std::move(bindings));
}

auto assign_values(const AST& ast,
                   const TypeGraph& tg,
                   Map<ASTID, std::vector<Binding>> bindings,
                   std::vector<Binding> values,
                   ASTID pattern) {
  int offset = 0;
  for(const ASTID id : ast.forest.leaf_ids(pattern)) {
    const int size = size_of(tg, ast.types[id.get()]);
    if(ast.forest[id] == ASTTag::PatternIdent) {
      bindings.emplace(id,
                       std::vector<Binding>(std::make_move_iterator(values.begin() + offset),
                                            std::make_move_iterator(values.begin() + offset + size)));
    }
    offset += size;
  }
  assert(offset == values.size());
  return bindings;
}

auto run_or_assign(ExecutorRef ex,
                   Span<std::string_view> srcs,
                   const AST& ast,
                   const TypeGraph& tg,
                   const Map<ASTID, ASTID>& binding_of,
                   Map<TypeRef, TypeRef>& to_env_type,
                   Env env,
                   Map<ASTID, std::vector<Binding>> bindings,
                   ASTID id) {
  assert(ast.forest.is_root(id));

  const bool expr = is_expr(ast.forest[id]);
  const TypeRef type = copy_type(srcs, env, to_env_type, tg, ast.types[id.get()]);

  std::vector<Binding> values;
  std::tie(values, bindings) = run_function(
    srcs,
    ast,
    tg,
    binding_of,
    env.copy_types,
    env.flat_functions,
    ex,
    std::move(bindings),
    expr ? id : ast.forest.child_ids(id).get<1>());

  return expr ? std::tuple(Binding2{type, std::move(values)}, std::move(env), std::move(bindings))
              : std::tuple(Binding2{type},
                           std::move(env),
                           assign_values(ast, tg, std::move(bindings), std::move(values), *ast.forest.first_child(id)));
}

Env generate_functions(
  Span<std::string_view> srcs, Env env, const AST& ast, const TypeGraph& tg, const CallGraphData& cg) {
  Map<TypeRef, TypeRef> to_env_type;

  // type graph was copied from env initially, so all those nodes should be identical
  for(TypeRef t : id_range(TypeRef{env.tg.num_nodes()})) {
    to_env_type.emplace(t, t);
  }

  Map<ASTID, ASTID> to_env_id;

  for(ASTID id : bfs_traversal(invert(cg.call_graph), cg.leaf_fns)) {
    const auto fn_id = ast.forest.next_sibling(id);
    assert(fn_id);

    if(ast.forest[*fn_id] == ASTTag::Fn) {
      const auto fn_id = ast.forest.next_sibling(id);
      assert(fn_id && ast.forest[*fn_id] == ASTTag::Fn);
      auto [global_values, global_borrows, fg] = create_graph(ast, tg, env.copy_types, cg.binding_of, *fn_id);

      assert(global_values.empty());

      std::vector<BorrowedFuture> borrowed = transform_to_vec(global_borrows, [&](ASTID id) {
        const auto it = to_env_id.find(id);
        assert(it != to_env_id.end());
        return borrow(Future(Any(env.flat_functions.at(it->second)))).first;
      });

      const TypeRef env_fn_type = copy_type(srcs, env, to_env_type, tg, ast.types[id.get()]);

      to_env_id.emplace(
        id,
        env.add_function(
          sv(srcs, ast.srcs[id.get()]), env_fn_type, curry(create_async_graph(std::move(fg)), std::move(borrowed))));
    } else {
      to_env_id.emplace(id, id);
    }
  }

  return env;
}

std::tuple<std::string, AST, Map<ASTID, std::vector<Binding>>>
append_global_bindings(std::string env_src, AST ast, Bindings2 str_bindings) {
  Map<ASTID, std::vector<Binding>> bindings;
  for(auto& [name, binding] : str_bindings) {
    bindings.emplace(add_global(ast, SrcRef{SrcID{0}, append_src(env_src, name)}, binding.type),
                     std::move(binding.values));
  }

  return std::tuple(std::move(env_src), std::move(ast), std::move(bindings));
}

std::tuple<Env, Bindings2> to_str_bindings(
  Span<std::string_view> srcs,
  const AST& ast,
  const TypeGraph& tg,
  Map<TypeRef, TypeRef>& to_env_type,
  Env env,
  Map<ASTID, std::vector<Binding>> bindings) {
  Bindings2 str_bindings;
  for(auto& [id, values] : bindings) {
    str_bindings.emplace(std::string(sv(srcs, ast.srcs[id.get()])),
                         Binding2{copy_type(srcs, env, to_env_type, tg, ast.types[id.get()]), std::move(values)});
  }
  return std::tuple(std::move(env), std::move(str_bindings));
}

} // namespace

Tree<Any> await(Tree<Binding> tree) {
  return knot::map<Tree<Any>>(std::move(tree), [](Binding b) -> Any { return take(std::move(b)).wait(); });
}

Type<TypeID> type(const Tree<Binding>& tree) { return TypeOfBindingConverter{}(tree); }

StringResult<void, Env> parse_script(Env env, std::string_view script) {
  return parse(script)
    .append_state(std::move(env))
    .and_then([](UnTypedAST ast, Env env) {
      return knot::accumulate(ast, ContextualResult<void, Env>{std::move(env)}, [&](auto result, const auto& tup) {
        const auto& name = std::get<0>(tup);
        return std::move(result)
          .and_then([&](Env env) { return type_name_resolution(env, std::get<1>(tup)).append_state(std::move(env)); })
          .and_then([](TypedFunction f, Env env) { return type_check(env, std::move(f)).append_state(std::move(env)); })
          .and_then([&](TypedFunction f, Env env) { return add_function(std::move(env), name, std::move(f)); });
      });
    })
    .map_error([&](auto errors, Env env) {
      return std::tuple(contextualize(script, std::move(errors)), std::move(env));
    });
}

StringResult<Tree<Binding>, Env, Bindings>
run(ExecutorRef executor, Env env, Bindings bindings, std::string_view expr) {
  return parse_repl(expr)
    .append_state(std::move(env), std::move(bindings))
    .and_then(visited(Overloaded{
      [&](UnTypedExpr expr, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(expr))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedExpr expr, Env env, Bindings bindings) {
            return run_expr(executor, std::move(env), std::move(bindings), std::move(expr));
          });
      },
      [&](UnTypedAssignment assign, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(assign))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedAssignment assign, Env env, Bindings bindings) {
            return run_assign(executor, std::move(env), std::move(bindings), std::move(assign));
          })
          .map([](Env env, Bindings b) { return std::tuple(Tree<Binding>{}, std::move(env), std::move(b)); });
      }}))
    .map_error([&](auto errors, auto&&... ts) {
      return std::tuple(contextualize(expr, std::move(errors)), std::move(ts)...);
    });
}

StringResult<std::string, Env, Bindings>
run_to_string(ExecutorRef executor, Env env, Bindings bindings, std::string_view expr) {
  return parse_repl(expr)
    .append_state(std::move(env), std::move(bindings))
    .and_then(visited(Overloaded{
      [&](UnTypedExpr expr, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(expr))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedExpr expr, Env env, Bindings bindings) {
            return run_expr_to_string(executor, std::move(env), std::move(bindings), std::move(expr));
          });
      },
      [&](UnTypedAssignment assign, Env env, Bindings bindings) {
        return type_name_resolution(env, std::move(assign))
          .append_state(std::move(env), std::move(bindings))
          .and_then([&](TypedAssignment assign, Env env, Bindings bindings) {
            return run_assign(executor, std::move(env), std::move(bindings), std::move(assign));
          })
          .map([](Env env, Bindings bindings) {
            return std::tuple(std::string(), std::move(env), std::move(bindings));
          });
      },
    }))
    .map_error([&](auto errors, auto&&... ts) {
      return std::tuple(contextualize(expr, std::move(errors)), std::move(ts)...);
    });
}

int main(int argc, const char** argv, Env e) {
  const std::optional<Command> cmd = parse_cmd_line(argc, argv);

  if(!cmd) {
    const char* msg =
      "Usage:\n"
      "  run [scripts...]\n"
      "  repl [scripts...]\n";

    fmt::print("{}", msg);
    return 1;
  }

  Executor executor = make_task_executor();

  const auto result =
    parse_scripts(std::move(e), cmd->filenames).append_state(Bindings{}).and_then([&](Env env, Bindings bindings) {
      if(cmd->run_main) {
        return run_to_string(executor, std::move(env), std::move(bindings), "main()")
          .map([](std::string s, Env e, Bindings b) {
            return std::tuple(make_vector(std::move(s)), std::move(e), std::move(b));
          });
      } else {
        std::tie(env, bindings) = run_repl(executor, std::move(env), std::move(bindings));
        return success(
          knot::Type<std::vector<std::string>>{}, std::vector<std::string>{}, std::move(env), std::move(bindings));
      }
    });

  for(const std::string& line : result ? result.value() : result.error()) {
    fmt::print("{}\n", line);
  }

  return !result.has_value();
}

StringResult<void, Env> parse_scripts(Env env, Span<std::string_view> files) {
  const std::string env_src = env.src;
  const auto srcs = flatten(make_sv_array(env_src), files);

  return accumulate_errors<ContextualError2>(
           [&srcs](SrcID src, AST ast, TypeGraph tg) {
             return parse2(std::move(ast), std::move(tg), src, srcs[src.get()]);
           },
           id_range(SrcID(1), SrcID(srcs.size())),
           env.ast,
           env.tg)
    .append_state(std::move(env))
    .and_then([&srcs](AST ast, TypeGraph tg, Env env) {
      return sema(srcs, env.type_cache, env.type_ids, env.copy_types, std::move(ast), std::move(tg))
        .append_state(std::move(env));
    })
    .map([&srcs](CallGraphData cg, AST ast, TypeGraph tg, Env env) {
      env = generate_functions(srcs, std::move(env), ast, tg, cg);
      return std::tuple(std::move(ast), std::move(tg), std::move(env));
    })
    .map_state([](AST, TypeGraph, Env env) { return env; })
    .map_error([&srcs](auto errors, Env env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

StringResult<Binding2, Env, Bindings2> run(ExecutorRef ex, Env env, Bindings2 str_bindings, std::string_view expr) {
  Map<TypeRef, TypeRef> to_env_type;

  auto [env_src, ast, bindings] = append_global_bindings(env.src, env.ast, std::move(str_bindings));

  const auto srcs = make_sv_array(env_src, expr);

  return parse_repl2(std::move(ast), env.tg, SrcID{1}, srcs[1])
    .and_then([&](AST ast, TypeGraph tg) {
      return sema(srcs, env.type_cache, env.type_ids, env.copy_types, std::move(ast), std::move(tg));
    })
    .append_state(std::move(env), std::move(bindings))
    .map([&](CallGraphData cg, AST ast, TypeGraph tg, Env env, Map<ASTID, std::vector<Binding>> bindings) {
      Binding2 result;
      std::tie(result, env, bindings) = run_or_assign(
        ex,
        srcs,
        ast,
        tg,
        cg.binding_of,
        to_env_type,
        std::move(env),
        std::move(bindings),
        ASTID{i32(ast.forest.size() - 1)});
      return std::tuple(std::move(result), std::move(ast), std::move(tg), std::move(env), std::move(bindings));
    })
    .map_state([&](AST ast, TypeGraph tg, Env env, Map<ASTID, std::vector<Binding>> bindings) {
      return to_str_bindings(srcs, ast, tg, to_env_type, std::move(env), std::move(bindings));
    })
    .map_error([&](auto errors, Env env, Bindings2 bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });
}

StringResult<std::string, Env, Bindings2>
run_to_string(ExecutorRef ex, Env env, Bindings2 str_bindings, std::string_view expr) {
  Map<TypeRef, TypeRef> to_env_type;

  auto [env_src, ast, bindings] = append_global_bindings(env.src, env.ast, std::move(str_bindings));
  const SrcRef to_string_ref = {SrcID{0}, append_src(env_src, "to_string")};
  const SrcRef string_ref = {SrcID{0}, append_src(env_src, "string")};

  const auto srcs = make_sv_array(env_src, expr);

  return parse_repl2(std::move(ast), env.tg, SrcID{1}, srcs[1])
    .and_then([&](AST ast, TypeGraph tg) {
      return sema(srcs, env.type_cache, env.type_ids, env.copy_types, std::move(ast), std::move(tg));
    })
    .and_then([&](CallGraphData cg, AST ast, TypeGraph tg) {
      const auto id = ASTID{i32(ast.forest.size() - 1)};
      if(ast.forest[id] == ASTTag::Assignment) {
        return success(knot::Type<std::vector<ContextualError2>>{}, std::move(cg), std::move(ast), std::move(tg));
      } else {
        const TypeRef expr_type = ast.types[id.get()];
        const TypeRef borrow_type = tg.add_node(std::array{expr_type}, TypeTag::Borrow, SrcRef{}, TypeID{});
        const TypeRef tuple_type = tg.add_node(std::array{borrow_type}, TypeTag::Tuple, SrcRef{}, TypeID{});
        const TypeRef string_type = tg.add_node(TypeTag::Leaf, string_ref, type_id(knot::Type<std::string>{}));
        const TypeRef fn_type = tg.add_node(std::array{tuple_type, string_type}, TypeTag::Fn, SrcRef{}, TypeID{});

        const ASTID borrow_id = append_root(ast, ASTTag::ExprBorrow, SrcRef{}, borrow_type, std::array{id});
        const ASTID tuple_id = append_root(ast, ASTTag::ExprTuple, SrcRef{}, tuple_type, std::array{borrow_id});
        const ASTID callee_id = append_root(ast, ASTTag::ExprIdent, to_string_ref, fn_type);
        const ASTID fn_id = append_root(ast, ASTTag::ExprCall, SrcRef{}, string_type, std::array{callee_id, tuple_id});

        return sema(srcs, env.type_cache, env.type_ids, env.copy_types, std::move(ast), std::move(tg));
      }
    })
    .append_state(std::move(env), std::move(bindings))
    .map([&](CallGraphData cg, AST ast, TypeGraph tg, Env env, Map<ASTID, std::vector<Binding>> bindings) {
      Binding2 result;
      std::tie(result, env, bindings) = run_or_assign(
        ex,
        srcs,
        ast,
        tg,
        cg.binding_of,
        to_env_type,
        std::move(env),
        std::move(bindings),
        ASTID{i32(ast.forest.size() - 1)});
      return std::tuple(std::move(result), std::move(ast), std::move(tg), std::move(env), std::move(bindings));
    })
    .map_state([&](AST ast, TypeGraph tg, Env env, Map<ASTID, std::vector<Binding>> bindings) {
      return to_str_bindings(srcs, ast, tg, to_env_type, std::move(env), std::move(bindings));
    })
    .map([](Binding2 b, Env env, Bindings2 bindings) {
      assert(b.values.size() <= 1);
      assert(b.values.empty() || env.tg.get<TypeID>(b.type) == type_id(knot::Type<std::string>{}));
      return std::tuple(
        b.values.size() == 1 ? any_cast<std::string>(take(std::move(b.values[0])).wait()) : std::string(),
        std::move(env),
        std::move(bindings));
    })
    .map_error([&](auto errors, Env env, Bindings2 bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });

  return {"", std::move(env), std::move(str_bindings)};
}

} // namespace ooze
