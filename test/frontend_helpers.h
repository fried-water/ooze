#pragma once

#include "parser.h"
#include "sema.h"

namespace ooze {

template <typename Parser>
auto parse_and_name_resolution(
  Parser p, Span<std::string_view> srcs, const TypeNames& names, AST ast, TypeGraph tg, SrcID src_id) {
  return p(std::move(ast), std::move(tg), src_id, srcs[src_id.get()]).and_then([&](auto pr, AST ast, TypeGraph tg) {
    return type_name_resolution(srcs, names, pr.type_srcs, std::move(tg)).map_state([&](TypeGraph tg) {
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

struct TestEnv {
  NativeTypeInfo types;
  std::string src;
  AST ast;
  TypeGraph tg;
};

inline TestEnv create_test_env(NativeTypeInfo types = {}, Span<std::string_view> globals = {}) {
  std::string src;
  AST ast;
  TypeGraph tg;
  ParserResult<ASTID> pr;

  for(std::string_view binding : globals) {
    const auto srcs = make_sv_array(binding);
    std::tie(pr, ast, tg) = check_result(parse_binding(std::move(ast), std::move(tg), SrcID{0}, binding));
    tg = check_result(type_name_resolution(srcs, types.names, pr.type_srcs, std::move(tg)));

    const ASTID pattern_id{i32(ast.forest.size() - 1)};

    BOOST_REQUIRE(ASTTag::PatternIdent == ast.forest[pattern_id]);

    ast.srcs[pattern_id.get()] = SrcRef{SrcID{0}, append_src(src, sv(srcs, ast.srcs[pattern_id.get()]))};
    const ASTID value_id = append_root(ast, ASTTag::EnvValue, ast.srcs[pattern_id.get()], ast.types[pattern_id.get()]);
    append_root(ast, ASTTag::Assignment, SrcRef{}, Type{}, std::array{pattern_id, value_id});
  }

  return {std::move(types), std::move(src), std::move(ast), std::move(tg)};
}

template <typename... Bindings>
TestEnv basic_test_env(Bindings... globals) {
  return create_test_env(basic_types(), make_sv_array(globals...));
}

template <size_t N>
TestEnv basic_test_env(std::array<std::string_view, N> globals) {
  return create_test_env(basic_types(), globals);
}

} // namespace ooze
