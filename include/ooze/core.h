#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"
#include "ooze/native_registry.h"
#include "ooze/opaque.h"
#include "ooze/result.h"
#include "ooze/type.h"

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
  Type type;
  std::vector<AsyncValue> values;
};

using Bindings = std::unordered_map<std::string, Binding>;

NativeRegistry create_primitive_registry();

struct EnvData;

class Env {
  Opaque<EnvData> _data;

public:
  Env();

  explicit Env(NativeRegistry);

  Env(const Env& e) { *this = e; }
  Env& operator=(const Env&);

  Env(Env&&) = default;
  Env& operator=(Env&&) = default;

  StringResult<void> parse_scripts(Span<std::string_view>) &;
  StringResult<void, Env> parse_scripts(Span<std::string_view>) &&;

  StringResult<Binding, Bindings> run(ExecutorRef, Bindings, std::string_view) &;
  StringResult<Binding, Env, Bindings> run(ExecutorRef, Bindings, std::string_view) &&;

  StringResult<Future, Bindings> run_to_string(ExecutorRef, Bindings, std::string_view) &;
  StringResult<Future, Env, Bindings> run_to_string(ExecutorRef, Bindings, std::string_view) &&;

  StringResult<void> type_check_expr(std::string_view) const;
  StringResult<void> type_check_fn(std::string_view) const;
  StringResult<void> type_check_binding(std::string_view) const;

  StringResult<Type> parse_type(std::string_view);
  std::string pretty_print(Type) const;

  const NativeTypeInfo& native_types() const;
  std::vector<std::pair<std::string, Type>> globals() const;
};

} // namespace ooze
