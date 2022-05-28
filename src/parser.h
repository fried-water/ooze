#pragma once

#include "ast.h"

namespace ooze {

std::optional<Literal> parse_literal(std::string_view);

std::optional<AST> parse(std::string_view);

} // namespace ooze
