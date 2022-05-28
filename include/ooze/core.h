#pragma once

#include "ooze/env.h"

#include <string>
#include <vector>

namespace ooze {

std::optional<Any> load(const Env&, Span<std::byte>);
std::vector<std::byte> save(const Env&, const Any&);

std::vector<Any> run(const Env&, const std::string& fn, std::vector<Any>);
std::vector<Any> run(const Env&, std::string_view script, std::vector<Any>, const std::string& fn = "main");

int main(int argc, char* argv[], const Env&);

} // namespace ooze
