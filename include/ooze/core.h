#pragma once

#include "ooze/env.h"

#include <anyf/graph.h>

#include <tl/expected.hpp>

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T>
using Result = tl::expected<T, std::vector<std::string>>;

Result<std::unordered_map<std::string, anyf::FunctionGraph>>
parse_script(const Env&, std::string_view script, std::function<Result<Any>(const std::string&)> load);

Result<anyf::FunctionGraph> parse_expr(const Env&,
                                       std::string_view expr,
                                       std::function<Result<Any>(const std::string&)> load,
                                       const std::unordered_map<std::string, anyf::FunctionGraph>& = {});

Result<std::vector<Any>>
run(const Env&, std::string_view script, std::string_view expr, std::function<Result<Any>(const std::string&)>);

Result<std::vector<std::byte>> save(const Env&, const Any&);

Result<Any> load(const Env&, Span<std::byte>, const std::string&);

int main(int argc, char* argv[], const Env&);

} // namespace ooze
