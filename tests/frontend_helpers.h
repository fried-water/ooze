#pragma once

#include "frontend.h"
#include "parser.h"
#include "sema.h"

namespace ooze {

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
  ASTID module;
};

inline TestEnv create_test_env(NativeTypeInfo types = {}, Span<std::string_view> globals = {}) {
  std::string src;
  AST ast;

  const Type unit = ast.tg.add_node(TypeTag::Tuple, TypeID{});

  const std::vector<ASTID> ids = transform_to_vec(globals, [&](std::string_view binding) {
    const auto srcs = make_sv_array(binding);
    ASTID pattern;
    std::tie(pattern, ast) =
      check_result(parse_and_name_resolution(parse_binding, srcs, types.names, std::move(ast), SrcID{0}));

    BOOST_REQUIRE(ASTTag::PatternIdent == ast.forest[pattern]);

    ast.srcs[pattern.get()] = SrcRef{SrcID{0}, append_src(src, sv(srcs, ast.srcs[pattern.get()]))};
    const ASTID value_id = append_root(ast, ASTTag::EnvValue, ast.srcs[pattern.get()], ast.types[pattern.get()]);
    return append_root(ast, ASTTag::Assignment, SrcRef{}, unit, std::array{pattern, value_id});
  });

  const ASTID module = append_root(ast, ASTTag::Module, SrcRef{SrcID{0}, append_src(src, "#env")}, unit, ids);

  return {std::move(types), std::move(src), std::move(ast), module};
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
