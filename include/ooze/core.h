#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/env.h"
#include "ooze/executor.h"
#include "ooze/future.h"
#include "ooze/result.h"
#include "ooze/type.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T, typename... Ts>
using StringResult = Result<T, std::vector<std::string>, Ts...>;

struct Binding {
  Type<TypeID> type;
  Future future;
  BorrowedFuture borrowed_future;
};

struct Binding2 {
  TypeRef type;
  std::vector<Binding> values;
};

using Bindings2 = std::unordered_map<std::string, Binding2>;

StringResult<void, Env> parse_scripts(Env, Span<std::string_view>);
StringResult<Binding2, Env, Bindings2> run(ExecutorRef, Env, Bindings2, std::string_view);
StringResult<std::string, Env, Bindings2> run_to_string(ExecutorRef, Env, Bindings2, std::string_view);

} // namespace ooze
