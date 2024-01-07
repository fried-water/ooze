#pragma once

#include "ooze/async_fn.h"
#include "ooze/function_graph.h"

namespace ooze {

AsyncFn create_async_value(Any);
AsyncFn create_async_graph(FunctionGraph);
AsyncFn create_async_functional(int output_count);
AsyncFn create_async_if(int output_count, AsyncFn, AsyncFn);
AsyncFn create_async_select();
AsyncFn create_async_converge();

AsyncFn curry(AsyncFn, std::vector<Any>);

} // namespace ooze
