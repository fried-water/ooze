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
    std::for_each(range.begin() + 1, range.end(), [&](const auto& ele) { r += ", " + f(ele); });
    return r + ")";
  }
}

std::string function_string(const Env& e,
                            std::string_view fn_name,
                            const std::vector<TypeProperties>& inputs,
                            const std::vector<TypeID>& outputs) {
  const auto input_type_name = [&](auto t) {
    return fmt::format("{}{}", type_name_or_id(e, t.id), t.value ? "" : "&");
  };

  return fmt::format("{}{} -> {}", fn_name, type_list_string(e, inputs), output_type_list_string(e, outputs));
}

} // namespace

std::string type_list_string(const Env& e, Span<TypeID> types) {
  return join(types, [&](TypeID type) { return type_name_or_id(e, type); });
}

std::string type_list_string(const Env& e, Span<TypeProperties> types) {
  return join(types,
              [&](TypeProperties t) { return fmt::format("{}{}", type_name_or_id(e, t.id), t.value ? "" : "&"); });
}

std::string type_list_string(const Env& e, Span<std::optional<TypeProperties>> types) {
  return join(types, [&](std::optional<TypeProperties> t) {
    return t ? fmt::format("{}{}", type_name_or_id(e, t->id), t->value ? "" : "&") : "_";
  });
}

std::string output_type_list_string(const Env& e, Span<TypeID> types) {
  return types.size() == 1 ? type_name_or_id(e, types.front()) : type_list_string(e, types);
}

std::string function_string(const Env& e, std::string_view fn_name, const EnvFunction& f) {
  return std::visit([&](const auto& f) { return function_string(e, fn_name, input_types(f), output_types(f)); }, f);
}

std::string function_string(const Env& e, std::string_view fn_name, const FunctionGraph& g) {
  return function_string(e, fn_name, input_types(g), output_types(g));
}

std::string function_string(const Env& e, std::string_view fn_name, const AnyFunction& f) {
  return function_string(e, fn_name, f.input_types(), f.output_types());
}

} // namespace ooze
