#include "pch.h"

#include "queries.h"

namespace ooze {

namespace {

template <typename Range, typename F>
std::string join(const Range& range, F f) {
  if(range.begin() == range.end()) {
    return "()";
  } else {
    std::string r = "(" + f(*range.begin());
    std::for_each(++range.begin(), range.end(), [&](const auto& ele) { r += ", " + f(ele); });
    return r + ")";
  }
}

std::string function_string(const Env& e,
                            std::string_view fn_name,
                            const std::vector<TypeProperties>& inputs,
                            const std::vector<TypeProperties>& outputs) {
  const auto type_name = [&](auto t) { return fmt::format("{}{}", type_name_or_id(e, t.id), t.value ? "" : "&"); };

  return fmt::format("{}{} -> {}",
                     fn_name,
                     join(inputs, type_name),
                     outputs.size() == 1 ? type_name(outputs.front()) : join(outputs, type_name));
}

} // namespace

std::string function_string(const Env& e, std::string_view fn_name, const anyf::FunctionGraph& g) {
  return function_string(e, fn_name, input_types(g), output_types(g));
}

std::string function_string(const Env& e, std::string_view fn_name, const anyf::AnyFunction& f) {
  return function_string(e, fn_name, f.input_types(), f.output_types());
}

} // namespace ooze
