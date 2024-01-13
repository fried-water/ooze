#pragma once

#include "ooze/core.h"
#include "ooze/env.h"

#include <string_view>

namespace ooze {

std::tuple<std::vector<std::string>, Env, Bindings2> step_repl(ExecutorRef, Env, Bindings2, std::string_view line);

std::tuple<Env, Bindings2> run_repl(ExecutorRef, Env, Bindings2);

} // namespace ooze
