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

struct EnvData {
  std::string src;
  AST ast;
  NativeTypeInfo native_types;
  Program program;
  std::unordered_map<ASTID, Inst> fns;

  ASTID add_fn(std::string_view name, Type type, Inst fn) {
    const auto ref = SrcRef{SrcID{0}, append_src(src, name)};
    const ASTID id = add_global(ast, ref, type);
    fns.emplace(id, fn);
    return id;
  }
};

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

Type copy_type(EnvData& env, const TypeGraph& tg, Type type) {
  return tg.get<TypeTag>(type) == TypeTag::Leaf
           ? env.ast.tg.add_node(TypeTag::Leaf, tg.get<TypeID>(type))
           : env.ast.tg.add_node(
               transform_to_vec(tg.fanout(type), [&](Type fanout) { return copy_type(env, tg, fanout); }),
               tg.get<TypeTag>(type),
               tg.get<TypeID>(type));
}

std::tuple<std::vector<AsyncValue>, Map<ASTID, std::vector<AsyncValue>>>
run_function(const AST& ast,
             const std::unordered_set<TypeID>& copy_types,
             const std::unordered_map<ASTID, Inst>& functions,
             ExecutorRef ex,
             FunctionGraphData fg_data,
             Map<ASTID, std::vector<AsyncValue>> bindings) {
  const Inst graph_inst = fg_data.program.add(std::move(fg_data.graph));

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

  return std::tuple(
    transform_to_vec(
      execute(
        std::make_shared<Program>(std::move(fg_data.program)), graph_inst, ex, std::move(futures), std::move(borrowed)),
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

std::pair<Program, std::vector<std::tuple<ASTID, ASTID, Inst>>>
generate_fns(Program prog,
             const AST& ast,
             const std::unordered_map<ASTID, Inst>& existing_fns,
             const std::unordered_set<TypeID>& copy_types,
             const Map<ASTID, ASTID>& overloads,
             Span<ASTID> new_fns) {
  std::vector<std::tuple<ASTID, ASTID, Inst>> fns =
    transform_filter_to_vec(new_fns, [&](ASTID root) -> std::optional<std::tuple<ASTID, ASTID, Inst>> {
      assert(ast.forest[root] == ASTTag::Assignment || ast.forest[root] == ASTTag::Module);
      if(ast.forest[root] == ASTTag::Assignment) {
        const auto [pattern, expr] = ast.forest.child_ids(root).take<2>();
        if(ast.forest[expr] == ASTTag::Fn) {
          return std::optional(std::tuple(pattern, expr, prog.placeholder()));
        }
      }
      return std::nullopt;
    });

  prog = fold(fns, std::move(prog), flattened([&](Program p0, ASTID, ASTID expr, Inst inst) {
                auto [p, captured_values, captured_borrows, graph] =
                  create_graph(std::move(p0), ast, copy_types, overloads, expr);

                assert(captured_borrows.empty());

                if(captured_values.empty()) {
                  p.set(inst, std::move(graph));
                } else {
                  std::vector<Any> values = transform_to_vec(captured_values, [&](ASTID id) {
                    if(const auto it = existing_fns.find(id); it != existing_fns.end()) {
                      return Any(it->second);
                    } else {
                      const auto it2 = find_if(fns, flattened([&](ASTID pat, ASTID, Inst) { return pat == id; }));
                      assert(it2 != fns.end());
                      return Any(std::get<2>(*it2));
                    }
                  });

                  p.set(inst, p.add(std::move(graph)), std::move(values));
                }

                return std::move(p);
              }));

  return std::pair(std::move(prog), std::move(fns));
}

EnvData copy_generic_fns(Span<std::string_view> srcs, EnvData env, const AST& ast, Span<ASTID> generic_roots) {
  for(const ASTID root : generic_roots) {
    const auto tree_size = distance(ast.forest.post_order_ids(root));

    env.ast.types.resize(env.ast.forest.size() + tree_size, Type{});
    env.ast.srcs.resize(env.ast.forest.size() + tree_size, SrcRef{});

    const i32 original_offset = ast.srcs[root.get()].slice.begin;
    const Slice new_slice = append_src(env.src, sv(srcs, ast.srcs[root.get()]));
    const i32 new_offset = new_slice.begin;

    const ASTID copy = copy_tree(ast.forest, root, env.ast.forest, [&](ASTID old_id, ASTID new_id) {
      // TODO literals
      env.ast.types[new_id.get()] = copy_type(env, ast.tg, ast.types[old_id.get()]);
      const Slice old_slice = ast.srcs[old_id.get()].slice;
      const i32 offset = new_offset + old_slice.begin - original_offset;
      env.ast.srcs[new_id.get()] = SrcRef{SrcID(0), {offset, offset + size(old_slice)}};
    });
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

std::tuple<EnvData, Bindings> to_str_bindings(
  Span<std::string_view> srcs, const AST& ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
  Bindings str_bindings;
  for(auto& [id, values] : bindings) {
    str_bindings.emplace(std::string(sv(srcs, ast.srcs[id.get()])),
                         Binding{copy_type(env, ast.tg, ast.types[id.get()]), std::move(values)});
  }
  return {std::move(env), std::move(str_bindings)};
}

auto run_or_assign(Span<std::string_view> srcs,
                   ExecutorRef ex,
                   const AST& ast,
                   const SemaData& s,
                   EnvData env,
                   Map<ASTID, std::vector<AsyncValue>> bindings,
                   ASTID id) {
  assert(is_global(ast.forest, id));
  assert(s.generic_roots.empty());

  std::vector<std::tuple<ASTID, ASTID, Inst>> generated_fns;
  std::tie(env.program, generated_fns) = generate_fns(
    std::move(env.program),
    ast,
    env.fns,
    env.native_types.copyable,
    s.overloads,
    filter_to_vec(s.resolved_roots, [&](ASTID id) { return ast.forest.is_root(id); }));

  auto fns = env.fns;

  for(const auto [pattern, expr, inst] : generated_fns) {
    if(pattern.is_valid()) {
      fns.emplace(pattern, inst);
      env.add_fn(sv(srcs, ast.srcs[pattern.get()]), copy_type(env, ast.tg, ast.types[pattern.get()]), inst);
    }
  }

  const bool expr = is_expr(ast.forest[id]);
  const Type type = copy_type(env, ast.tg, ast.types[id.get()]);

  std::vector<AsyncValue> values;
  std::tie(values, bindings) = run_function(
    ast,
    env.native_types.copyable,
    fns,
    ex,
    create_graph(
      env.program, ast, env.native_types.copyable, s.overloads, expr ? id : ast.forest.child_ids(id).get<1>()),
    std::move(bindings));

  return expr ? std::tuple(Binding{type, std::move(values)}, std::move(env), std::move(bindings))
              : std::tuple(Binding{type},
                           std::move(env),
                           assign_values(ast, std::move(bindings), std::move(values), *ast.forest.first_child(id)));
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
      const TypeCache tc = create_type_cache(ast.tg);
      return sema(srcs, tc, env.native_types, std::move(ast), roots);
    })
    .append_state(std::move(env))
    .map([&](SemaData s, AST ast, EnvData env) {
      std::vector<std::tuple<ASTID, ASTID, Inst>> generated_fns;
      std::tie(env.program, generated_fns) =
        generate_fns(std::move(env.program), ast, env.fns, env.native_types.copyable, s.overloads, s.resolved_roots);

      for(const auto [pattern, expr, inst] : generated_fns) {
        if(pattern.is_valid()) {
          env.add_fn(sv(srcs, ast.srcs[pattern.get()]), copy_type(env, ast.tg, ast.types[pattern.get()]), inst);
        }
      }

      env = copy_generic_fns(srcs, std::move(env), ast, s.generic_roots);

      return std::tuple(std::move(ast), std::move(env));
    })
    .map_state([](AST, EnvData env) { return env; })
    .map_error([&srcs](auto errors, EnvData env) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env));
    });
}

StringResult<Binding, EnvData, Bindings>
run(ExecutorRef ex, EnvData env, Bindings str_bindings, std::string_view expr) {
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

      return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod}).map([&](SemaData s, AST ast) {
        return std::tuple(std::tuple(std::move(s), root), std::move(ast));
      });
    })
    .append_state(std::move(env), std::move(bindings))
    .map(flattened([&](SemaData s, ASTID expr, AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      Binding result;
      std::tie(result, env, bindings) = run_or_assign(srcs, ex, ast, s, std::move(env), std::move(bindings), expr);
      return std::tuple(std::move(result), std::move(ast), std::move(env), std::move(bindings));
    }))
    .map_state([&](AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map_error([&](auto errors, EnvData env, Bindings bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });
}

StringResult<Future, EnvData, Bindings>
run_to_string(ExecutorRef ex, EnvData env, Bindings str_bindings, std::string_view expr) {
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
        return sema(srcs, tc, env.native_types, std::move(ast), std::array{runtime_mod}).map([&](SemaData s, AST ast) {
          return std::tuple(std::tuple(std::move(s), root), std::move(ast));
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
            .map([&](SemaData s, AST ast) { return std::tuple(std::tuple(std::move(s), call_id), std::move(ast)); });
        });
      }
    })
    .append_state(std::move(env), std::move(bindings))
    .map(flattened([&](SemaData s, ASTID expr, AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      Binding result;
      std::tie(result, env, bindings) = run_or_assign(srcs, ex, ast, s, std::move(env), std::move(bindings), expr);
      return std::tuple(std::move(result), std::move(ast), std::move(env), std::move(bindings));
    }))
    .map_state([&](AST ast, EnvData env, Map<ASTID, std::vector<AsyncValue>> bindings) {
      return to_str_bindings(srcs, ast, std::move(env), std::move(bindings));
    })
    .map([](Binding b, EnvData env, Bindings bindings) {
      assert(b.values.size() <= 1);
      assert(b.values.empty() || env.ast.tg.get<TypeID>(b.type) == type_id(knot::Type<std::string>{}));
      return std::tuple(b.values.size() == 1 ? take(std::move(b.values[0])) : Future(Any(std::string())),
                        std::move(env),
                        std::move(bindings));
    })
    .map_error([&](auto errors, EnvData env, Bindings bindings) {
      return std::tuple(contextualize(srcs, std::move(errors)), std::move(env), std::move(bindings));
    });
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
    .add_type<bool>("bool")
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

Env::Env() : _data(EnvData{}) {}

Env::Env(NativeRegistry r) : _data(EnvData{}) {
  // TODO check invariants on type and fn names

  auto& d = *_data;
  d.ast.tg = std::move(r.tg);
  d.native_types = std::move(r.types);

  for(NativeFn& fn : r.fns) {
    const Inst fn_inst = _data->program.add(std::move(fn.fn),
                                            borrows_of(d.ast.tg, d.ast.tg.fanout(fn.type)[0]),
                                            size_of(d.ast.tg, d.ast.tg.fanout(fn.type)[1]));

    const auto ref = SrcRef{SrcID{0}, append_src(d.src, fn.name)};
    const ASTID id = add_global(d.ast, ref, fn.type);
    d.fns.emplace(id, fn_inst);
  }
}

Env& Env::operator=(const Env& env) {
  _data = Opaque(*env._data);
  return *this;
}

StringResult<void> Env::parse_scripts(Span<std::string_view> files) & {
  return ooze::parse_scripts(std::move(*_data), files).map_state([&](auto d) { *_data = std::move(d); });
}

StringResult<void, Env> Env::parse_scripts(Span<std::string_view> files) && {
  return ooze::parse_scripts(std::move(*_data), files).map_state([&](auto d) {
    *_data = std::move(d);
    return std::move(*this);
  });
}

StringResult<Binding, Bindings> Env::run(ExecutorRef ex, Bindings bindings, std::string_view expr) & {
  return ooze::run(ex, std::move(*_data), std::move(bindings), expr).map_state([&](EnvData env, Bindings bindings) {
    *_data = std::move(env);
    return bindings;
  });
}

StringResult<Binding, Env, Bindings> Env::run(ExecutorRef ex, Bindings bindings, std::string_view expr) && {
  return ooze::run(ex, std::move(*_data), std::move(bindings), expr).map_state([&](EnvData env, Bindings bindings) {
    *_data = std::move(env);
    return std::tuple(std::move(*this), std::move(bindings));
  });
}

StringResult<Future, Bindings> Env::run_to_string(ExecutorRef ex, Bindings bindings, std::string_view expr) & {
  return ooze::run_to_string(ex, std::move(*_data), std::move(bindings), expr)
    .map_state([&](EnvData env, Bindings bindings) {
      *_data = std::move(env);
      return bindings;
    });
}

StringResult<Future, Env, Bindings> Env::run_to_string(ExecutorRef ex, Bindings bindings, std::string_view expr) && {
  return ooze::run_to_string(ex, std::move(*_data), std::move(bindings), expr)
    .map_state([&](EnvData env, Bindings bindings) {
      *_data = std::move(env);
      return std::tuple(std::move(*this), std::move(bindings));
    });
}

StringResult<void> Env::type_check_expr(std::string_view expr) const {
  const auto srcs = make_sv_array(_data->src, expr);
  return frontend(parse_expr, srcs, _data->native_types, _data->ast)
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void> Env::type_check_fn(std::string_view fn) const {
  const auto srcs = make_sv_array(_data->src, fn);
  return frontend(parse_fn, srcs, _data->native_types, _data->ast)
    .map_state(nullify())
    .map(nullify())
    .map_error([&](std::vector<ContextualError> errors) { return contextualize(srcs, std::move(errors)); });
}

StringResult<void> Env::type_check_binding(std::string_view binding) const {
  const auto srcs = make_sv_array(_data->src, binding);
  return parse_and_name_resolution(parse_binding, srcs, _data->native_types.names, _data->ast, SrcID{1})
    .and_then([&](ASTID pattern_root, AST ast) {
      if(ast.forest[pattern_root] != ASTTag::PatternIdent) {
        return ContextualResult<SemaData, AST>{
          Failure{make_vector(ContextualError{ast.srcs[pattern_root.get()], "not a binding"})}, std::move(ast)};
      } else {
        // Type check it as if it were an expr with the given type
        ast.forest[pattern_root] = ASTTag::ExprIdent;
        const TypeCache tc = create_type_cache(ast.tg);
        return sema(srcs, tc, _data->native_types, std::move(ast), std::array{pattern_root});
      }
    })
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

} // namespace ooze
