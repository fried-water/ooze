#pragma once

#include "ooze/ast_flat.h"
#include "ooze/env.h"

namespace ooze {

std::string pretty_print(const TypeNames&, TypeID);

std::string
pretty_print(Span<std::string_view>, const AST&, const TypeGraph&, const TypeNames&, std::optional<ASTID> = {});

std::string pretty_print(Span<std::string_view>, const TypeGraph&, const TypeNames&, TypeRef);

std::string pretty_print_fn_type(Span<std::string_view>, const TypeGraph&, const TypeNames&, TypeRef);

} // namespace ooze
