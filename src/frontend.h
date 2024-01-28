#pragma once

#include "parser.h"
#include "sema.h"

namespace ooze {

template <typename T>
ContextualResult<T, AST, TypeGraph>
type_name_resolution(Span<std::string_view> srcs, const TypeNames& names, ParserResult<T> pr, AST ast, TypeGraph tg) {
  return type_name_resolution(srcs, names, pr.type_srcs, std::move(tg))
    .map([&](TypeGraph tg) { return std::tuple(std::move(pr.parsed), std::move(tg)); })
    .map_state([&](TypeGraph tg) { return std::tuple(std::move(ast), std::move(tg)); });
}

template <typename Parser>
auto parse_and_name_resolution(
  Parser p, Span<std::string_view> srcs, const TypeNames& names, AST ast, TypeGraph tg, SrcID src_id) {
  return p(std::move(ast), std::move(tg), src_id, srcs[src_id.get()]).and_then([&](auto pr, AST ast, TypeGraph tg) {
    return type_name_resolution(srcs, names, std::move(pr), std::move(ast), std::move(tg));
  });
}

template <typename Parser>
auto frontend(Parser p, Span<std::string_view> srcs, const NativeTypeInfo& native_types, AST ast, TypeGraph tg) {
  assert(srcs.size() == 2);
  const TypeCache tc = create_type_cache(tg);
  return parse_and_name_resolution(p, srcs, native_types.names, std::move(ast), std::move(tg), SrcID{1})
    .and_then([&](auto roots, AST ast, TypeGraph tg) {
      return sema(srcs, tc, native_types, std::move(ast), std::move(tg), as_span(roots));
    });
}

} // namespace ooze
