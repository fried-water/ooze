#pragma once

#include "ooze/env.h"

#include <tl/expected.hpp>

#include <string>
#include <string_view>
#include <vector>

namespace ooze {

tl::expected<std::vector<Any>, std::vector<std::string>>
run(const Env&, std::string_view script, std::string_view expr, std::function<Any(const std::string&)>);

std::vector<std::byte> save(const Env&, const Any&);
std::optional<Any> load(const Env&, Span<std::byte>, const std::string&);

int main(int argc, char* argv[], const Env&);

} // namespace ooze
