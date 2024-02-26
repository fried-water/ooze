#pragma once

#include "ooze/core.h"

#include <string_view>

namespace ooze {

std::tuple<Future, Env> step_repl(ExecutorRef, Env, std::string_view line);

void run_repl(ExecutorRef, Env);

} // namespace ooze
