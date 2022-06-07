#pragma once

#include "ooze/env.h"

#include <tl/expected.hpp>

#include <string>
#include <string_view>
#include <vector>

namespace ooze {

std::optional<Any> load(const Env&, Span<std::byte>);
std::vector<std::byte> save(const Env&, const Any&);

tl::expected<std::vector<Any>, std::vector<std::string>> run(const Env&, const std::string& fn, std::vector<Any>);

tl::expected<std::vector<Any>, std::vector<std::string>>
run(const Env&, std::string_view script, std::vector<Any>, const std::string& fn = "main");

int main(int argc, char* argv[], const Env&);

} // namespace ooze
