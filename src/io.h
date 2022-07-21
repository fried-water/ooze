#pragma once

namespace ooze {

Result<void> write_binary_file(const std::string&, Span<std::byte>);
Result<std::vector<std::byte>> read_binary_file(const std::string&);

Result<std::string> read_text_file(const std::string& filename);

} // namespace ooze
