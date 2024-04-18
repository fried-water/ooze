#pragma once

#include "ooze/core.h"

#include <string_view>

namespace ooze {

std::tuple<Future, Env> step_repl(Executor&, Env, std::string_view line);

void run_repl(Executor&, Env);

} // namespace ooze
