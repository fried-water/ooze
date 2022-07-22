#pragma once

#include "ooze/env.h"

#include <anyf/borrowed_future.h>
#include <anyf/executor/task_executor.h>
#include <anyf/future.h>
#include <anyf/graph.h>

#include <string_view>

namespace ooze {

struct BindingEntry {
  TypeID type;
  anyf::Future future;
  anyf::BorrowedFuture borrowed_future;
};

struct Repl {
  anyf::TaskExecutor executor;
  Map<std::string, anyf::FunctionGraph> graphs;
  Map<std::string, BindingEntry> bindings;
  std::vector<std::tuple<std::string, TypeID, anyf::Future>> outstanding_writes;
};

std::vector<std::string> step_repl(const Env&, Repl&, std::string_view line);

void run_repl(const Env&);

} // namespace ooze
