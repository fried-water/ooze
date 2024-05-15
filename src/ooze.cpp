#include "pch.h"

#include "bindings.h"
#include "frontend.h"
#include "function_graph_construction.h"
#include "parser.h"
#include "pretty_print.h"
#include "runtime.h"
#include "sema.h"
#include "user_msg.h"

#include "ooze/core.h"

namespace ooze {

struct EnvData {
  std::string src;
  AST ast;
  NativeTypeInfo native_types;
  Program program;

  Map<ASTID, Inst> fns;
  Map<std::string, Binding> bindings;

  TypeCache type_cache;
  ASTID native_module;
  std::vector<ASTID> parsed_roots;

  SrcRef bindings_ref;
  SrcRef scripts_ref;
  SrcRef to_string_ref;
};

namespace {

template <typename T>
auto add_global(AST ast, Map<ASTID, T> ident_map, std::vector<ASTID> roots, T t, SrcRef ref, Type type, Type unit) {
  const ASTID ident = append_root(ast, ASTTag::PatternIdent, ref, type);
  const ASTID value = append_root(ast, ASTTag::EnvValue, ref, type);
  const ASTID root = append_root(ast, ASTTag::Assignment, SrcRef{}, unit, std::array{ident, value});

  ident_map.emplace(ident, std::move(t));
  roots.push_back(root);
  return std::tuple(std::move(ast), std::move(ident_map), std::move(roots));
}

void add_fn(EnvData& env, std::string_view name, Type type, Inst fn) {
  const SrcRef ref = {SrcID{0}, append_src(env.src, name)};
  std::tie(env.ast, env.fns, env.parsed_roots) =
    add_global(std::move(env.ast), std::move(env.fns), std::move(env.parsed_roots), fn, ref, type, env.type_cache.unit);
}

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

Type copy_type(EnvData& env, const TypeGraph& tg, Type type) {
  assert(type.is_valid());
  return tg.get<TypeTag>(type) == TypeTag::Leaf
           ? env.ast.tg.add_node(TypeTag::Leaf, tg.get<TypeID>(type))
           : env.ast.tg.add_node(
               transform_to_vec(tg.fanout(type), [&](Type fanout) { return copy_type(env, tg, fanout); }),
               tg.get<TypeTag>(type),
               tg.get<TypeID>(type));
}

std::tuple<std::vector<AsyncValue>, Map<ASTID, std::vector<AsyncValue>>> run_function(
  const AST& ast,
  const std::unordered_set<TypeID>& copy_types,
  const Map<ASTID, Inst>& functions,
  Executor& ex,
  Program program,
  FunctionGraphData fg_data,
  Map<ASTID, std::vector<AsyncValue>> bindings) {
  const int output_count = fg_data.graph.output_count;
  const Inst graph_inst = program.add(std::move(fg_data.graph));

  auto borrowed = fold(fg_data.captured_borrows, std::vector<BorrowedFuture>{}, [&](auto acc, ASTID id) {
    const auto it = bindings.find(id);
    assert(it != bindings.end());
    return transform_to_vec(
      it->second, [](AsyncValue& v) { return borrow(v); }, std::move(acc));
  });

  auto futures = fold(fg_data.captured_values, std::vector<Future>{}, [&](auto acc, ASTID id) {
    if(const auto it = bindings.find(id); it == bindings.end()) {
      const auto fn_it = functions.find(id);
      assert(fn_it != functions.end());
      acc.emplace_back(Any(fn_it->second));
    } else if(is_binding_copyable(ast.tg, copy_types, ast.types[id.get()])) {
      acc = transform_to_vec(
        it->second, [](AsyncValue& v) { return borrow(v).then([](const Any& a) { return a; }); }, std::move(acc));
    } else {
      acc = transform_to_vec(
        std::move(it->second), [](AsyncValue v) { return take(std::move(v)); }, std::move(acc));
      bindings.erase(it);
    }
    return acc;
  });

  std::vector<Future> results(output_count);
  execute(
    std::make_shared<Program>(std::move(program)), graph_inst, ex, std::move(futures), std::move(borrowed), results);

  return std::tuple(transform_to_vec(std::move(results), Construct<AsyncValue>{}), std::move(bindings));
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

ContextualResult<std::vector<std::tuple<ASTID, ASTID, Inst>>, Program>
generate_fns(Program program,
             const AST& ast,
             const Map<ASTID, Inst>& existing_fns,
             const std::unordered_set<TypeID>& copy_types,
             const Map<ASTID, ASTID>& overloads,
             Span<ASTID> new_fns) {
  std::vector<std::tuple<ASTID, ASTID, Inst>> fns = transform_to_vec(new_fns, [&](ASTID root) {
    assert(ast.forest[root] == ASTTag::Assignment);
    const auto [pattern, expr] = ast.forest.child_ids(root).take<2>();
    assert(ast.forest[expr] == ASTTag::Fn);
    return std::tuple(pattern, expr, program.placeholder());
  });

  std::vector<ContextualError> errors;

  std::tie(program, errors) = fold(
    fns,
    std::pair(std::move(program), std::vector<ContextualError>{}),
    flattened([&](Program p0, std::vector<ContextualError> errors, ASTID, ASTID expr, Inst inst) {
      auto [new_errors, p] =
        create_graph(std::move(p0), ast, copy_types, overloads, expr)
          .and_then([&](FunctionGraphData fg, Program p) {
            if(fg.captured_values.empty()) {
              p.set(inst, std::move(fg.graph));
            } else {
              std::vector<Any> values = transform_to_vec(fg.captured_values, [&](ASTID id) {
                if(const auto it = existing_fns.find(id); it != existing_fns.end()) {
                  return Any(it->second);
                } else {
                  const auto it2 = stdr::find_if(fns, flattened([&](ASTID pat, ASTID, Inst) { return pat == id; }));
                  assert(it2 != fns.end());
                  return Any(std::get<2>(*it2));
                }
              });

              p.set(inst, p.add(std::move(fg.graph)), std::move(values));
            }

            return ContextualResult<void, Program>{Failure{std::vector<ContextualError>{}}, std::move(p)};
          })
          .error_and_state();

      return std::pair(std::move(p), to_vec(std::move(new_errors), std::move(errors)));
    }));

  return value_or_errors(std::move(fns), std::move(errors), std::move(program));
}

EnvData copy_generic_fns(Span<std::string_view> srcs, EnvData env, const AST& ast, Span<ASTID> generic_roots) {
  for(const ASTID root : generic_roots) {
    const auto tree_size = stdr::distance(ast.forest.post_order_ids(root));

    env.ast.types.resize(env.ast.forest.size() + tree_size, Type{});
    env.ast.srcs.resize(env.ast.forest.size() + tree_size, SrcRef{});

    const i32 original_offset = ast.srcs[root.get()].slice.begin;
    const Slice new_slice = append_src(env.src, sv(srcs, ast.srcs[root.get()]));
    const i32 new_offset = new_slice.begin;

    const auto module = owning_module(ast.forest, root);
    assert(module);

    const ASTID copy = copy_tree_under(ast.forest, root, env.ast.forest, *module, [&](ASTID old_id, ASTID new_id) {
      // TODO literals
      env.ast.types[new_id.get()] = copy_type(env, ast.tg, ast.types[old_id.get()]);
      const Slice old_slice = ast.srcs[old_id.get()].slice;
      const i32 offset = new_offset + old_slice.begin - original_offset;
      env.ast.srcs[new_id.get()] = SrcRef{SrcID(0), {offset, offset + size(old_slice)}};
    });

    env.parsed_roots.push_back(copy);
  }

  return env;
}

auto prepare_ast(const EnvData& env, Map<std::string, Binding> str_bindings) {
  std::string src = env.src;
  AST ast = env.ast;
  Map<ASTID, std::vector<AsyncValue>> bindings;
  std::vector<ASTID> roots;
  roots.reserve(str_bindings.size());

  const Type unit = env.type_cache.unit;
  const ASTID env_module = append_root(ast, ASTTag::Module, env.scripts_ref, unit, env.parsed_roots);

  for(auto& [name, binding] : str_bindings) {
    std::tie(ast, bindings, roots) = add_global(
      std::move(ast),
      std::move(bindings),
      std::move(roots),
      std::move(binding.values),
      SrcRef{SrcID{0}, append_src(src, name)},
      binding.type,
      unit);
  }

  const ASTID binding_module = append_root(ast, ASTTag::Module, env.bindings_ref, unit, roots);

  return std::tuple(
    std::move(src), std::move(ast), std::move(bindings), std::array{env.native_module, env_module, binding_module});
}

EnvData to_str_bindings(
  Span<std::string_view> srcs, const AST& ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
  for(auto& [id, values] : bindings) {
    env.bindings.emplace(std::string(sv(srcs, ast.srcs[id.get()])),
                         Binding{copy_type(env, ast.tg, ast.types[id.get()]), std::move(values)});
  }
  return env;
}

ContextualResult<Binding, EnvData, Map<ASTID, std::vector<AsyncValue>>> run_or_assign(
  Span<std::string_view> srcs,
  Executor& ex,
  const AST& ast,
  const SemaData& s,
  EnvData env,
  Map<ASTID, std::vector<AsyncValue>> bindings,
  ASTID id) {
  assert(owning_module(ast.forest, id));
  assert(s.generic_roots.empty());

  return generate_fns(std::move(env.program),
                      ast,
                      env.fns,
                      env.native_types.copyable,
                      s.overloads,
                      filter_to_vec(s.resolved_roots, [&](ASTID id) { return !ast.forest.is_root(id); }))
    .and_then([&](auto generated_fns, Program p) {
      env.program = p;
      auto fns = env.fns;

      for(const auto [pattern, expr, inst] : generated_fns) {
        assert(pattern.is_valid());
        fns.emplace(pattern, inst);
        add_fn(env, sv(srcs, ast.srcs[pattern.get()]), copy_type(env, ast.tg, ast.types[pattern.get()]), inst);
      }

      return create_graph(env.program,
                          ast,
                          env.native_types.copyable,
                          s.overloads,
                          is_expr(ast.forest[id]) ? id : ast.forest.child_ids(id).get<1>())
        .map([&](FunctionGraphData fg_data, Program p) {
          return std::tuple(std::tuple(std::move(fg_data), std::move(fns), std::move(p)), Program{});
        });
    })
    .map_state(nullify())
    .append_state(std::move(env), std::move(bindings))
    .map(flattened([&](FunctionGraphData fg_data, auto fns, Program p, EnvData env, auto bindings) {
      std::vector<AsyncValue> values;
      std::tie(values, bindings) =
        run_function(ast, env.native_types.copyable, fns, ex, std::move(p), std::move(fg_data), std::move(bindings));

      const Type type = copy_type(env, ast.tg, ast.types[id.get()]);

      return is_expr(ast.forest[id])
               ? std::tuple(Binding{type, std::move(values)}, std::move(env), std::move(bindings))
               : std::tuple(Binding{type},
                            std::move(env),
                            assign_values(ast, std::move(bindings), std::move(values), *ast.forest.first_child(id)));
    }));
}

StringResult<void, EnvData> parse_scripts(EnvData env, Span<std::string_view> files) {
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
      return sema(srcs, env.type_cache, env.native_types, std::move(ast), roots, std::array{env.native_module});
    })
    .append_state(std::move(env))
    .and_then([&](SemaData s, AST ast, EnvData env) {
      return generate_fns(
               std::move(env.program), ast, env.fns, env.native_types.copyable, s.overloads, s.resolved_roots)
        .map_state([&](Program p) {
          env.program = std::move(p);
          return std::tuple(std::move(ast), std::move(env));
        })
        .map([&](auto generated_fns, AST ast, EnvData env) {
          for(const auto [pattern, expr, inst] : generated_fns) {
            assert(pattern.is_valid());
            add_fn(env, sv(srcs, ast.srcs[pattern.get()]), copy_type(env, ast.tg, ast.types[pattern.get()]), inst);
          }

          env = copy_generic_fns(srcs, std::move(env), ast, s.generic_roots);
          return std::tuple(std::move(ast), std::move(env));
        });
    })
    .map_state([](AST, EnvData env) { return env; })
    .map_error([&srcs](auto errors, EnvData env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

StringResult<Binding, EnvData> run(Executor& ex, EnvData env, std::string_view expr) {
  auto [env_src, ast, bindings, global_imports_] = prepare_ast(env, std::move(env.bindings));
  const auto global_imports = global_imports_;
  const auto srcs = make_sv_array(env_src, expr);

  return parse_and_name_resolution(parse_repl, srcs, env.native_types.names, std::move(ast), SrcID{1})
    .and_then([&](const ASTID root, AST ast) {
      return sema(srcs, env.type_cache, env.native_types, std::move(ast), std::array{root}, global_imports)
        .map([&](SemaData s, AST ast) { return std::tuple(std::tuple(std::move(s), root), std::move(ast)); });
    })
    .append_state(std::move(env), std::move(bindings))
    .and_then(
      flattened([&](SemaData s, ASTID expr, AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
        return run_or_assign(srcs, ex, ast, s, std::move(env), std::move(bindings), expr)
          .map_state([&](EnvData env, auto bindings) {
            return std::tuple(std::move(ast), std::move(env), std::move(bindings));
          });
      }))
    .map_state([&](AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map_error([&](auto errors, EnvData env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

StringResult<Future, EnvData> run_to_string(Executor& ex, EnvData env, std::string_view expr) {
  auto [env_src, ast, bindings, global_imports_] = prepare_ast(env, std::move(env.bindings));
  const auto global_imports = global_imports_;
  const auto srcs = make_sv_array(env_src, expr);

  return parse_and_name_resolution(parse_repl, srcs, env.native_types.names, std::move(ast), SrcID{1})
    .and_then([&](const ASTID root, AST ast) {
      if(ast.forest[root] == ASTTag::Assignment) {
        return sema(srcs, env.type_cache, env.native_types, std::move(ast), std::array{root}, global_imports)
          .map([&](SemaData s, AST ast) { return std::tuple(std::tuple(std::move(s), root), std::move(ast)); });
      } else {
        return sema(srcs, env.type_cache, env.native_types, std::move(ast), std::array{root}, global_imports)
          .and_then([&](auto, AST ast) {
            const Type expr_type = ast.types[root.get()];
            const Type borrow_type = ast.tg.add_node(std::array{expr_type}, TypeTag::Borrow, TypeID{});
            const Type tuple_type = ast.tg.add_node(std::array{borrow_type}, TypeTag::Tuple, TypeID{});
            const Type string_type = ast.tg.add_node(TypeTag::Leaf, type_id(knot::Type<std::string>{}));
            const Type fn_type = ast.tg.add_node(std::array{tuple_type, string_type}, TypeTag::Fn, TypeID{});

            const ASTID borrow_id = append_root(ast, ASTTag::ExprBorrow, SrcRef{}, borrow_type, std::array{root});
            const ASTID tuple_id = append_root(ast, ASTTag::ExprTuple, SrcRef{}, tuple_type, std::array{borrow_id});
            const ASTID callee_id = append_root(ast, ASTTag::ExprIdent, env.to_string_ref, fn_type);
            const ASTID call_id =
              append_root(ast, ASTTag::ExprCall, SrcRef{}, string_type, std::array{callee_id, tuple_id});

            return sema(srcs, env.type_cache, env.native_types, std::move(ast), std::array{call_id}, global_imports)
              .map([&](SemaData s, AST ast) { return std::tuple(std::tuple(std::move(s), call_id), std::move(ast)); });
          });
      }
    })
    .append_state(std::move(env), std::move(bindings))
    .and_then(
      flattened([&](SemaData s, ASTID expr, AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
        return run_or_assign(srcs, ex, ast, s, std::move(env), std::move(bindings), expr)
          .map_state([&](EnvData env, auto bindings) {
            return std::tuple(std::move(ast), std::move(env), std::move(bindings));
          });
      }))
    .map_state([&](AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map([](Binding b, EnvData env) {
      assert(b.values.size() <= 1);
      assert(b.values.empty() || env.ast.tg.get<TypeID>(b.type) == type_id(knot::Type<std::string>{}));
      return std::tuple(b.values.size() == 1 ? take(std::move(b.values[0])) : Future(Any(std::string())),
                        std::move(env));
    })
    .map_error([&](auto errors, EnvData env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

EnvData create_env_data(NativeRegistry r) {
  // TODO check invariants on type and fn names

  EnvData d = {};

  d.ast.tg = std::move(r.tg);
  d.native_types = std::move(r.types);
  d.type_cache = create_type_cache(d.ast.tg);

  d.bindings_ref = SrcRef{SrcID{0}, append_src(d.src, "#bindings")};
  d.scripts_ref = SrcRef{SrcID{0}, append_src(d.src, "#scripts")};
  d.to_string_ref = SrcRef{SrcID{0}, append_src(d.src, "to_string")};

  std::vector<ASTID> roots;
  roots.reserve(r.fns.size());

  for(NativeFn& fn : r.fns) {
    const Inst fn_inst = d.program.add(std::move(fn.fn), size_of(d.ast.tg, d.ast.tg.fanout(fn.type)[1]));
    const SrcRef ref = {SrcID{0}, append_src(d.src, fn.name)};
    std::tie(d.ast, d.fns, roots) =
      add_global(std::move(d.ast), std::move(d.fns), std::move(roots), fn_inst, ref, fn.type, d.type_cache.unit);
  }

  const SrcRef ref = {SrcID{0}, append_src(d.src, "#builtins")};
  d.native_module = append_root(d.ast, ASTTag::Module, ref, d.type_cache.unit, roots);

  return d;
}

} // namespace

NativeRegistry create_primitive_registry() {
  return NativeRegistry{}
    .add_type<bool>("bool")
    .add_type<i8>("i8")
    .add_type<i16>("i16")
    .add_type<i32>("i32")
    .add_type<i64>("i64")
    .add_type<u8>("u8")
    .add_type<u16>("u16")
    .add_type<u32>("u32")
    .add_type<u64>("u64")
    .add_type<f32>("f32")
    .add_type<f64>("f64")
    .add_type<std::string>("string")
    .add_type<std::vector<std::string>>("string_vector")
    .add_type<std::vector<std::byte>>("byte_vector")
    .add_fn("to_string", [](const bool& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const i8& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const i16& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const i32& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const i64& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const u8& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const u16& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const u32& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const u64& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const f32& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const f64& x) { return fmt::format("{}", x); })
    .add_fn("to_string", [](const std::string& x) { return fmt::format("{}", x); })
    .add_fn("println", [](const std::string& s) { fmt::println("{}", s); });
}

Env::Env() : _data(create_env_data(NativeRegistry{})) {}
Env::Env(NativeRegistry r) : _data(create_env_data(std::move(r))) {}

StringResult<void> Env::parse_scripts(Span<std::string_view> files) & {
  return ooze::parse_scripts(std::move(*_data), files).map_state([&](auto d) { *_data = std::move(d); });
}

StringResult<void, Env> Env::parse_scripts(Span<std::string_view> files) && {
  return ooze::parse_scripts(std::move(*_data), files).map_state([&](auto d) {
    *_data = std::move(d);
    return std::move(*this);
  });
}

StringResult<Binding> Env::run(Executor& ex, std::string_view expr) & {
  return ooze::run(ex, std::move(*_data), expr).map_state([&](EnvData env) { *_data = std::move(env); });
}

StringResult<Binding, Env> Env::run(Executor& ex, std::string_view expr) && {
  return ooze::run(ex, std::move(*_data), expr).map_state([&](EnvData env) {
    *_data = std::move(env);
    return std::move(*this);
  });
}

StringResult<Future> Env::run_to_string(Executor& ex, std::string_view expr) & {
  return ooze::run_to_string(ex, std::move(*_data), expr).map_state([&](EnvData env) { *_data = std::move(env); });
}

StringResult<Future, Env> Env::run_to_string(Executor& ex, std::string_view expr) && {
  return ooze::run_to_string(ex, std::move(*_data), expr).map_state([&](EnvData env) {
    *_data = std::move(env);
    return std::move(*this);
  });
}

bool Env::drop(std::string_view binding) { return _data->bindings.erase(std::string(binding)) > 0; }

void Env::insert(std::string_view name, Binding binding) { _data->bindings.emplace(name, std::move(binding)); }

void Env::insert(std::string_view name, Future f, TypeID type_id) {
  const Type type = _data->ast.tg.add_node(TypeTag::Leaf, type_id);
  _data->bindings.emplace(name, Binding{type, make_vector(AsyncValue{std::move(f)})});
}

StringResult<void> Env::type_check(std::string_view expr, std::string_view hint) const {
  const auto srcs = make_sv_array(_data->src, hint, expr);
  return parse_and_name_resolution(ooze::parse_type, srcs, _data->native_types.names, std::move(_data->ast), SrcID{1})
    .and_then([&](Type t, AST ast) {
      return parse_and_name_resolution(parse_expr, srcs, _data->native_types.names, std::move(ast), SrcID{2})
        .map([&](ASTID expr, AST ast) {
          ast.types[expr.get()] = t;
          return std::tuple(expr, std::move(ast));
        });
    })
    .and_then([&](ASTID expr, AST ast) {
      return sema(srcs,
                  _data->type_cache,
                  _data->native_types,
                  std::move(ast),
                  std::array{expr},
                  std::array{_data->native_module});
    })
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void> Env::type_check_fn(std::string_view fn) const {
  const auto srcs = make_sv_array(_data->src, fn);
  return frontend(parse_fn_expr, srcs, _data->native_types, _data->ast, std::array{_data->native_module})
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<Type> Env::parse_type(std::string_view type_src) {
  const auto srcs = make_sv_array(_data->src, type_src);

  return parse_and_name_resolution(ooze::parse_type, srcs, _data->native_types.names, std::move(_data->ast), SrcID{1})
    .map_state([&](AST ast) { _data->ast = std::move(ast); })
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

std::string Env::pretty_print(Type type) const {
  return ooze::pretty_print(_data->ast.tg, _data->native_types.names, type);
}

const NativeTypeInfo& Env::native_types() const { return _data->native_types; }

std::vector<std::pair<std::string, Type>> Env::globals() const {
  std::vector<std::pair<std::string, Type>> globals;

  const AST& ast = _data->ast;
  const auto srcs = make_sv_array(_data->src);

  const auto handle_id = [&](auto self, ASTID id, std::string preamble) -> void {
    if(ast.forest[id] == ASTTag::Assignment) {
      const ASTID ident = *ast.forest.first_child(id);
      globals.emplace_back(sv(srcs, ast.srcs[ident.get()]), ast.types[ident.get()]);
    } else if(ast.forest[id] == ASTTag::Module) {
      preamble = fmt::format("{}{}::", preamble, sv(srcs, ast.srcs[id.get()]));
      for(const ASTID id : ast.forest.child_ids(id)) {
        self(self, id, preamble);
      }
    }
  };

  for(const ASTID id : ast.forest.root_ids()) {
    handle_id(handle_id, id, "");
  }

  return globals;
}

std::vector<std::tuple<std::string, Type, BindingState>> Env::bindings() const {
  return transform_to_vec(sorted(transform_to_vec(_data->bindings, Get<0>{})), [&](std::string name) {
    const Binding& b = _data->bindings.find(name)->second;
    const BindingState max_state = std::accumulate(
      b.values.begin(), b.values.end(), BindingState::Ready, [](BindingState acc, const AsyncValue& ele) {
        const BindingState ele_state = find_binding_state(ele);
        return i32(acc) > i32(ele_state) ? acc : ele_state;
      });
    return std::tuple(std::move(name), b.type, max_state);
  });
}

} // namespace ooze
