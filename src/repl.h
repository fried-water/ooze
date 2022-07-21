#pragma once

#include "ooze/env.h"

#include <anyf/executor/task_executor.h>
#include <anyf/future.h>
#include <anyf/graph.h>

#include <string_view>

namespace ooze {

struct Repl {
  anyf::TaskExecutor executor;
  Map<std::string, anyf::FunctionGraph> graphs;
  Map<std::string, std::pair<anyf::Future, TypeID>> bindings;
  std::vector<std::tuple<std::string, TypeID, anyf::Future>> outstanding_writes;
};

std::vector<std::string> step_repl(const Env&, Repl&, std::string_view line);

void run_repl(const Env&);

} // namespace ooze
