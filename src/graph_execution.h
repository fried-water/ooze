#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/future.h"
#include "ooze/graph.h"

namespace ooze {

std::vector<Future> execute_graph(const FunctionGraph&, ExecutorRef, std::vector<Future>, std::vector<BorrowedFuture>);

} // namespace ooze
