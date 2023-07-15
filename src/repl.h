#pragma once

#include "ooze/core.h"
#include "ooze/env.h"

#include <string_view>

namespace ooze {

std::tuple<std::vector<std::string>, Env, Bindings> step_repl(anyf::ExecutorRef, Env, Bindings, std::string_view line);

std::tuple<Env, Bindings> run_repl(anyf::ExecutorRef, Env, Bindings);

} // namespace ooze
