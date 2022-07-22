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
                            const std::vector<TypeID>& outputs) {
  const auto input_type_name = [&](auto t) {
    return fmt::format("{}{}", type_name_or_id(e, t.id), t.value ? "" : "&");
  };
  const auto output_type_name = [&](auto t) { return type_name_or_id(e, t); };

  return fmt::format("{}{} -> {}",
                     fn_name,
                     join(inputs, input_type_name),
                     outputs.size() == 1 ? output_type_name(outputs.front()) : join(outputs, output_type_name));
}

} // namespace

std::string function_string(const Env& e, std::string_view fn_name, const anyf::FunctionGraph& g) {
  return function_string(e, fn_name, input_types(g), output_types(g));
}

std::string function_string(const Env& e, std::string_view fn_name, const anyf::AnyFunction& f) {
  return function_string(e, fn_name, f.input_types(), f.output_types());
}

Result<void> type_check(const Env& e, const std::vector<ast::Binding>& bindings, const std::vector<TypeID>& types) {
  if(types.size() != bindings.size()) {
    return err(fmt::format("Assignment expects {} value(s), given {}", bindings.size(), types.size()));
  }

  std::vector<std::string> errors;
  for(int i = 0; i < types.size(); i++) {
    if(bindings[i].type) {
      const std::string given_type = type_name_or_id(e, types[i]);

      if(given_type != bindings[i].type) {
        errors.push_back(fmt::format("{} expects {}, given {}", bindings[i].name, *bindings[i].type, given_type));
      }
    }
  }

  return errors.empty() ? Result<void>{} : tl::unexpected{std::move(errors)};
}

} // namespace ooze
