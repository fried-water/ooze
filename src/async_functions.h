#pragma once

#include "ooze/any_function.h"
#include "ooze/borrowed_future.h"
#include "ooze/future.h"
#include "ooze/graph.h"

#include <functional>

namespace ooze {

using AsyncFn = std::function<std::vector<Future>(ExecutorRef, std::vector<Future>, std::vector<BorrowedFuture>)>;

AsyncFn create_async_value(Any);
AsyncFn create_async(std::shared_ptr<const AnyFunction>);
AsyncFn create_async_graph(FunctionGraph);
AsyncFn create_async_functional(int output_count);
AsyncFn create_async_if(int output_count, AsyncFn, AsyncFn);
AsyncFn create_async_select();
AsyncFn create_async_converge();

} // namespace ooze
