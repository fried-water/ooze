#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/future.h"

#include <functional>
#include <vector>

namespace ooze {

using AsyncFn = std::function<std::vector<Future>(ExecutorRef, std::vector<Future>, std::vector<BorrowedFuture>)>;

}
