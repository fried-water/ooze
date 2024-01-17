#pragma once

#include "parser.h"
#include "sema.h"

namespace ooze {

template <typename Parser>
auto parse_and_name_resolution(
  Parser p, Span<std::string_view> srcs, const TypeNames& names, AST ast, TypeGraph tg, SrcID src_id) {
  return p(std::move(ast), std::move(tg), src_id, srcs[src_id.get()])
    .and_then([&](auto type_srcs, AST ast, TypeGraph tg) {
      return type_name_resolution(srcs, names, type_srcs, std::move(tg)).map_state([&](TypeGraph tg) {
        return std::tuple(std::move(ast), std::move(tg));
      });
    });
}

inline NativeTypeInfo basic_types() {
  return {{{"bool", type_id(knot::Type<bool>{})},
           {"f32", type_id(knot::Type<f32>{})},
           {"i32", type_id(knot::Type<i32>{})},
           {"string", type_id(knot::Type<std::string>{})}},
          {type_id(knot::Type<bool>{}), type_id(knot::Type<f32>{}), type_id(knot::Type<i32>{})}};
}

using EnvValues = std::vector<std::pair<std::string, std::string>>;

struct TestEnv {
  NativeTypeInfo types;
  std::string src;
  AST ast;
  TypeGraph tg;
};

inline TestEnv create_test_env(NativeTypeInfo types = {}, EnvValues values = {}) {
  std::string src;
  AST ast;
  TypeGraph tg;

  std::vector<std::pair<Type, SrcRef>> type_srcs;

  for(const auto& [name, type] : values) {
    std::tie(type_srcs, ast, tg) = check_result(parse_type(std::move(ast), std::move(tg), SrcID{0}, type));
    tg = check_result(type_name_resolution(make_sv_array(type), types.names, type_srcs, std::move(tg)));
    add_global(ast, SrcRef{SrcID{0}, append_src(src, name)}, Type{tg.num_nodes() - 1});
  }

  return {std::move(types), std::move(src), std::move(ast), std::move(tg)};
}

inline TestEnv basic_test_env(EnvValues values = {}) { return create_test_env(basic_types(), std::move(values)); }

} // namespace ooze
