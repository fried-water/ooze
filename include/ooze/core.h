#pragma once

#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"
#include "ooze/native_registry.h"
#include "ooze/opaque.h"
#include "ooze/result.h"
#include "ooze/type.h"

#include <span>
#include <string>
#include <string_view>
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

enum class BindingState { Ready, Borrowed, NotReady };

NativeRegistry create_primitive_registry();

struct EnvData;

class Env {
  Opaque<EnvData> _data;

public:
  Env();

  explicit Env(NativeRegistry);

  Env(const Env&) = delete;
  Env& operator=(const Env&) = delete;

  Env(Env&&) = default;
  Env& operator=(Env&&) = default;

  StringResult<void> parse_scripts(std::span<const std::string_view>) &;
  StringResult<void, Env> parse_scripts(std::span<const std::string_view>) &&;

  StringResult<Binding> run(Executor&, std::string_view) &;
  StringResult<Binding, Env> run(Executor&, std::string_view) &&;

  StringResult<Future> run_to_string(Executor&, std::string_view) &;
  StringResult<Future, Env> run_to_string(Executor&, std::string_view) &&;

  bool drop(std::string_view);

  void insert(std::string_view, Binding);
  void insert(std::string_view, Future, TypeID);

  template <typename T>
  void insert(std::string_view name, T value) {
    return insert(name, Future(Any(std::move(value))), type_id<T>());
  }

  StringResult<void> type_check(std::string_view expr, std::string_view hint = "_") const;
  StringResult<void> type_check_fn(std::string_view) const;

  StringResult<Type> parse_type(std::string_view);
  std::string pretty_print(Type) const;

  const NativeTypeInfo& native_types() const;

  std::vector<std::pair<std::string, Type>> globals() const;
  std::vector<std::tuple<std::string, Type, BindingState>> bindings() const;
};

} // namespace ooze
