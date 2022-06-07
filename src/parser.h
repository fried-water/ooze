#pragma once

#include "ast.h"

namespace ooze {

std::optional<Literal> parse_literal(std::string_view);

Result<AST> parse(std::string_view);

} // namespace ooze
