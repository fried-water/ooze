#pragma once

#include "ast.h"
#include "ooze/env.h"

#include <anyf/future.h>
#include <anyf/graph.h>

namespace ooze {

inline std::string type_name_or_id(const Env& e, TypeID type) {
  const auto it = e.type_names.find(type);
  return it != e.type_names.end() ? it->second : fmt::format("type 0x{:x}", type);
}

std::string type_list_string(const Env&, Span<TypeID>);
std::string type_list_string(const Env&, Span<TypeProperties>);

std::string output_type_list_string(const Env&, Span<TypeID>);

std::string function_string(const Env&, std::string_view fn_name, const EnvFunction&);
std::string function_string(const Env&, std::string_view fn_name, const FunctionGraph&);
std::string function_string(const Env&, std::string_view fn_name, const AnyFunction&);

Result<void> type_check(const Env&, const std::vector<ast::Binding>&, const std::vector<TypeID>&);

} // namespace ooze
