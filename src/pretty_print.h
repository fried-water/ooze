#pragma once

#include "ooze/ast.h"
#include "ooze/type.h"

namespace ooze {

std::string pretty_print(const TypeNames&, TypeID);

std::string pretty_print(Span<std::string_view>, const AST&, const TypeNames&, std::optional<ASTID> = {});

std::string pretty_print(const TypeGraph&, const TypeNames&, Type);

} // namespace ooze
