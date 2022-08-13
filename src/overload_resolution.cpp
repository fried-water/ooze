#include "pch.h"

#include "overload_resolution.h"
#include "queries.h"

namespace ooze {

Result<anyf::AnyFunction>
overload_resolution(const Env& e, const std::string& name, const std::vector<anyf::TypeID>& types) {
  const auto it = e.functions.find(name);

  if(it == e.functions.end()) {
    return err(fmt::format("use of undeclared function '{}'", name));
  }

  std::vector<anyf::AnyFunction> results;

  std::copy_if(it->second.begin(), it->second.end(), std::back_inserter(results), [&](const auto& fn) {
    const auto& input_types = fn.input_types();
    if(input_types.size() == types.size()) {
      for(size_t i = 0; i < types.size(); i++) {
        if(input_types[i].id != types[i]) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  });

  if(results.size() == 1) {
    return std::move(results.front());
  } else if(results.empty()) {
    std::vector<std::string> errors{
      fmt::format("no matching overload found for {}{}", name, type_list_string(e, types))};

    for(const AnyFunction& function : it->second) {
      errors.push_back(fmt::format("  candidate: {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  } else {
    std::vector<std::string> errors{fmt::format("function call is ambiguous {}{}", name, type_list_string(e, types))};

    for(const AnyFunction& function : results) {
      errors.push_back(fmt::format("  candidate: {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  }
}

} // namespace ooze
