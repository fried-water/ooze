#pragma once

#include "ooze/core.h"
#include "ooze/env.h"

#include <string_view>

namespace ooze {

struct Repl {
  Env env;
  anyf::TaskExecutor executor;
  Map<std::string, Binding> bindings;
  std::vector<std::tuple<std::string, TypeID, anyf::Future>> outstanding_writes;
};

std::vector<std::string> step_repl(Repl&, std::string_view line);

void run_repl(Env);

} // namespace ooze
