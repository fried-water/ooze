#pragma once

#include "program.h"

#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"

#include <memory>

namespace ooze {

void execute(std::shared_ptr<const Program>,
             Inst,
             ExecutorRef,
             std::vector<Future>,
             std::vector<BorrowedFuture>,
             std::span<Future> output);

} // namespace ooze
