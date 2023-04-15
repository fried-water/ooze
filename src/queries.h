#pragma once

#include "ast.h"
#include "ooze/env.h"
#include "typed_ast.h"

#include <anyf/future.h>
#include <anyf/graph.h>

namespace ooze {

inline std::string to_string(TypeID type) { return fmt::format("type 0x{:x}", type); }

inline std::string type_name_or_id(const Env& e, TypeID type) {
  const auto it = e.type_names.find(type);
  return it != e.type_names.end() ? it->second : to_string(type);
}

inline std::string type_name_or_id(const Env& e, TypeProperties type) {
  const auto it = e.type_names.find(type.id);
  return fmt::format("{}{}", type.value ? "" : "&", it != e.type_names.end() ? it->second : to_string(type.id));
}

template <typename R, typename T>
R untype(const Env& e, const T& t) {
  return knot::map<R>(t,
                      Overloaded{[&](TypeID t) { return NamedType{type_name_or_id(e, t)}; },
                                 [&](const EnvFunctionRef& r) { return NamedFunction{r.name}; }});
}

std::string type_string(const Env&, const CompoundType<TypeID>&);

std::string function_string(const Env&, std::string_view fn_name, const EnvFunction&);
std::string function_string(const Env&, std::string_view fn_name, const FunctionGraph&);
std::string function_string(const Env&, std::string_view fn_name, const AnyFunction&);

} // namespace ooze
