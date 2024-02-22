#pragma once

#include "ooze/any_fn.h"
#include "ooze/type.h"

#include <cassert>
#include <optional>
#include <string>
#include <type_traits>
#include <vector>

namespace ooze {

struct NativeFn {
  Type type;
  std::string name;
  AnyFn fn;
};

struct NativeRegistry {
  TypeGraph tg;
  NativeTypeInfo types;
  std::vector<NativeFn> fns;

  template <typename F>
  void add_fn(std::string name, F&& f) & {
    fns.push_back({add_fn_type(tg, decay(knot::Type<F>{})), std::move(name), create_any_fn(std::forward<F>(f))});
  }

  template <typename F>
  NativeRegistry&& add_fn(std::string name, F&& f) && {
    add_fn(std::move(name), std::forward<F>(f));
    return std::move(*this);
  }

  template <typename T>
  void add_type(std::string name, std::optional<bool> copy_override = {}) & {
    const TypeID type = type_id<T>();

    constexpr bool is_default_copy =
      std::is_copy_constructible_v<T> && std::is_trivially_destructible_v<T> && sizeof(T) <= 64;

    if((copy_override.has_value() && *copy_override) || (!copy_override.has_value() && is_default_copy)) {
      assert(std::is_copy_constructible_v<T>);
      types.copyable.insert(type);
    }

    if constexpr(std::is_copy_constructible_v<T>) {
      std::move(*this).add_fn("clone", [](const T& t) { return t; });
    }

    insert(types.names, std::string(name), type);
  }

  template <typename T>
  NativeRegistry&& add_type(std::string name, std::optional<bool> copy_override = {}) && {
    add_type<T>(std::move(name), copy_override);
    return std::move(*this);
  }
};

} // namespace ooze
