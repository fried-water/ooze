#pragma once

#include "ooze/core.h"
#include "ooze/env.h"

#include <string_view>

namespace ooze {

std::tuple<Future, Env, Bindings> step_repl(ExecutorRef, Env, Bindings, std::string_view line);

void run_repl(ExecutorRef, Env, Bindings);

} // namespace ooze
