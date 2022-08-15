#include "pch.h"

#include "overload_resolution.h"
#include "queries.h"

namespace ooze {

namespace {

bool type_check(Span<anyf::TypeProperties> expected, Span<anyf::TypeProperties> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    // Don't allow copies (take by value, given borrowed)
    if(expected[i].id != given[i].id || expected[i].value > given[i].value) {
      return false;
    }
  }

  return true;
}

bool type_check(Span<anyf::TypeID> expected, Span<anyf::TypeID> given) {
  if(expected.size() != given.size()) return false;

  for(size_t i = 0; i < given.size(); i++) {
    if(expected[i] != given[i]) {
      return false;
    }
  }

  return true;
}

} // namespace

Result<anyf::AnyFunction> overload_resolution(const Env& e,
                                              const std::string& name,
                                              Span<anyf::TypeProperties> inputs,
                                              std::optional<Span<anyf::TypeID>> outputs) {
  const auto it = e.functions.find(name);

  if(it == e.functions.end()) {
    return err(fmt::format("use of undeclared function '{}'", name));
  }

  std::vector<anyf::AnyFunction> results;

  std::copy_if(it->second.begin(), it->second.end(), std::back_inserter(results), [&](const auto& fn) {
    return type_check(fn.input_types(), inputs) && (!outputs || type_check(fn.output_types(), *outputs));
  });

  if(results.size() == 1) {
    return std::move(results.front());
  } else if(results.empty()) {
    std::vector<std::string> errors{outputs ? fmt::format("no matching overload found for {}{} -> {} [{} candidate(s)]",
                                                          name,
                                                          type_list_string(e, inputs),
                                                          output_type_list_string(e, *outputs),
                                                          it->second.size())
                                            : fmt::format("no matching overload found for {}{} [{} candidate(s)]",
                                                          name,
                                                          type_list_string(e, inputs),
                                                          it->second.size())};

    for(const AnyFunction& function : it->second) {
      errors.push_back(fmt::format("  {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  } else {
    std::vector<std::string> errors{fmt::format(
      "function call is ambiguous {}{} [{} candidate(s)]", name, type_list_string(e, inputs), results.size())};

    for(const AnyFunction& function : results) {
      errors.push_back(fmt::format("  {}", function_string(e, name, function)));
    }

    return tl::unexpected{std::move(errors)};
  }
}

} // namespace ooze
