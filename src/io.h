#pragma once

namespace ooze {

void write_binary_file(const std::string&, Span<std::byte>);
std::vector<std::byte> read_binary_file(const std::string&);

std::string read_text_file(const std::string& filename);

} // namespace ooze
