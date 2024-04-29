#pragma once

#include "ast.h"

#include "ooze/type.h"

namespace ooze {

std::string pretty_print(const TypeNames&, TypeID);

std::string pretty_print(Span<std::string_view>, const AST&, const TypeNames&, std::optional<ASTID> = {});

std::string pretty_print(const TypeGraph&, const TypeNames&, Type);

inline void dump(Span<std::string_view> srcs, const AST& ast, const TypeNames& type_names) {
  fmt::print("AST {}\n", ast.forest.size());

  for(const ASTID id : ast.forest.post_order_ids()) {
    const Type type = ast.types[id.get()];
    const auto module = owning_module(ast.forest, id);
    fmt::print("  {:3} {:20} {:3} {:20} {}\n",
               id.get(),
               knot::debug(ast.forest[id]),
               module ? knot::debug(module->get()) : "",
               type.is_valid() ? pretty_print(ast.tg, type_names, type) : "_",
               sv(srcs, ast.srcs[id.get()]));
  }
}

} // namespace ooze
