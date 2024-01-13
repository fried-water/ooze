#pragma once

#include "parser_flat.h"
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

} // namespace ooze
