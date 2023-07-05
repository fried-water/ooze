#pragma once

namespace ooze {

StringResult<void> write_binary_file(const std::string&, Span<std::byte>);
StringResult<std::vector<std::byte>> read_binary_file(const std::string&);

StringResult<std::string> read_text_file(const std::string& filename);

} // namespace ooze
