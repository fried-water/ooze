#pragma once

#include "ooze/ast.h"
#include "ooze/inst.h"
#include "ooze/program.h"
#include "ooze/src_map.h"
#include "ooze/traits.h"

#include <knot/core.h>

#include <algorithm>
#include <cassert>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ooze {

struct Env {
  std::string src;

  AST ast;

  NativeTypeInfo native_types;

  Program program;

  std::unordered_map<ASTID, Inst> functions;

  ASTID add_function(std::string_view name, Type type, Inst fn) {
    const auto ref = SrcRef{SrcID{0}, append_src(src, name)};
    const ASTID id = add_global(ast, ref, type);
    functions.emplace(id, fn);
    return id;
  }

  template <typename F>
  ASTID add_function(std::string_view name, F&& f) {
    return add_function(name, add_fn(ast.tg, decay(knot::Type<F>{})), program.add(std::forward<F>(f)));
  }

  template <typename T>
  void add_type(std::string_view name, std::optional<bool> copy_override = {}) {
    const TypeID type = type_id(knot::Type<T>{});

    constexpr bool is_default_copy =
      std::is_copy_constructible_v<T> && std::is_trivially_destructible_v<T> && sizeof(T) <= 64;

    if((copy_override.has_value() && *copy_override) || (!copy_override.has_value() && is_default_copy)) {
      assert(std::is_copy_constructible_v<T>);
      native_types.copyable.insert(type);
    }

    if constexpr(std::is_copy_constructible_v<T>) {
      add_function("clone", [](const T& t) { return t; });
    }

    insert(native_types.names, std::string(name), type);
  }
};

namespace details {

template <typename T, typename... Ts>
auto generate_constructor(knot::TypeList<Ts...>) {
  return
    [](std::conditional_t<std::is_trivially_destructible_v<Ts>, const Ts&, Ts>... ts) { return T{std::move(ts)...}; };
}

} // namespace details

template <typename T>
void add_tieable_type(Env& e, const std::string& name) {
  const TypeID type = type_id(knot::Type<T>{});

  e.add_type<T>(name);

  e.add_function("to_string", [](const T& t) { return knot::debug(t); });
  e.add_function("serialize", [](const T& t) { return knot::serialize(t); });
  e.add_function("deserialize",
                 [](const std::vector<std::byte>& bytes) { return *knot::deserialize<T>(bytes.begin(), bytes.end()); });

  if constexpr(std::is_aggregate_v<T>) {
    std::string create_name = "create_";
    std::transform(name.begin(), name.end(), std::back_inserter(create_name), [](char c) { return std::tolower(c); });

    using knot::as_tie;

    e.add_function(create_name, details::generate_constructor<T>(as_typelist(tie_type(knot::Type<T>{}))));
  }
}

Env create_primative_env();

} // namespace ooze
