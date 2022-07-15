#include "pch.h"

#include "io.h"

#include <fstream>
#include <sstream>

namespace ooze {

void write_binary_file(const std::string& filename, Span<std::byte> bytes) {
  std::basic_ofstream<char> file(filename, std::ios::binary);
  file.write((const char*)bytes.begin(), bytes.size());
}

std::vector<std::byte> read_binary_file(const std::string& filename) {
  std::basic_ifstream<char> file(filename.c_str(), std::ios::binary);
  std::vector<std::byte> bytes;
  std::transform(std::istreambuf_iterator<char>(file), {}, std::back_inserter(bytes), [](char c) {
    return static_cast<std::byte>(c);
  });

  return bytes;
}

std::string read_text_file(const std::string& filename) {
  std::ifstream file(filename);

  std::stringstream sstr;
  sstr << file.rdbuf();
  return std::move(sstr).str();
}

} // namespace ooze
