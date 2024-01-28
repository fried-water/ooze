#pragma once

#include "parser.h"
#include "sema.h"

namespace ooze {

template <typename T>
ContextualResult<T, AST>
type_name_resolution(Span<std::string_view> srcs, const TypeNames& names, ParserResult<T> pr, AST ast) {
  return type_name_resolution(srcs, names, pr.type_srcs, std::move(ast.tg))
    .map([&](TypeGraph tg) { return std::tuple(std::move(pr.parsed), std::move(tg)); })
    .map_state([&](TypeGraph tg) {
      ast.tg = std::move(tg);
      return ast;
    });
}

template <typename Parser>
auto parse_and_name_resolution(Parser p, Span<std::string_view> srcs, const TypeNames& names, AST ast, SrcID src_id) {
  return p(std::move(ast), src_id, srcs[src_id.get()]).and_then([&](auto pr, AST ast) {
    return type_name_resolution(srcs, names, std::move(pr), std::move(ast));
  });
}

template <typename Parser>
auto frontend(Parser p, Span<std::string_view> srcs, const NativeTypeInfo& native_types, AST ast) {
  assert(srcs.size() == 2);
  const TypeCache tc = create_type_cache(ast.tg);
  return parse_and_name_resolution(p, srcs, native_types.names, std::move(ast), SrcID{1})
    .and_then([&](auto roots, AST ast) { return sema(srcs, tc, native_types, std::move(ast), as_span(roots)); });
}

} // namespace ooze
