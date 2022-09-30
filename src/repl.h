#pragma once

#include "ooze/core.h"
#include "ooze/env.h"

#include <string_view>

namespace ooze {

std::vector<std::string> step_repl(RuntimeEnv&, std::string_view line);

void run_repl(Env);

} // namespace ooze
