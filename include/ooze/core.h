#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/env.h"
#include "ooze/executor.h"
#include "ooze/future.h"
#include "ooze/result.h"
#include "ooze/type_flat.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T, typename... Ts>
using StringResult = Result<T, std::vector<std::string>, Ts...>;

struct AsyncValue {
  Future future;
  BorrowedFuture borrowed_future;
};

struct Binding {
  TypeRef type;
  std::vector<AsyncValue> values;
};

using Bindings = std::unordered_map<std::string, Binding>;

StringResult<void, Env> parse_scripts(Env, Span<std::string_view>);

StringResult<void> type_check_expr(const Env&, std::string_view);
StringResult<void> type_check_fn(const Env&, std::string_view);

StringResult<Binding, Env, Bindings> run(ExecutorRef, Env, Bindings, std::string_view);
StringResult<std::string, Env, Bindings> run_to_string(ExecutorRef, Env, Bindings, std::string_view);

} // namespace ooze
