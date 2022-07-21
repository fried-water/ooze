#include "pch.h"

#include "io.h"

#include <filesystem>
#include <fstream>
#include <sstream>

namespace ooze {

namespace {

namespace fs = std::filesystem;

Result<void> check_file_exists(const std::string& filename) {
  return fs::exists(fs::path{filename}) ? Result<void>{} : err(fmt::format("File {} does not exist", filename));
}

Result<void> check_is_not_directory(const std::string& filename) {
  return !fs::is_directory(fs::path{filename}) ? Result<void>{} : err(fmt::format("{} is a directory", filename));
}

} // namespace

Result<void> write_binary_file(const std::string& filename, Span<std::byte> bytes) {
  try {
    return check_is_not_directory(filename).map([&]() {
      std::basic_ofstream<char> file(filename, std::ios::binary);
      file.write((const char*)bytes.begin(), bytes.size());
    });
  } catch(const std::exception& ex) {
    return err(ex.what());
  }
}

Result<std::vector<std::byte>> read_binary_file(const std::string& filename) {
  try {
    return check_file_exists(filename).and_then([&]() { return check_is_not_directory(filename); }).map([&]() {
      std::basic_ifstream<char> file(filename.c_str(), std::ios::binary);
      std::vector<std::byte> bytes;
      std::transform(std::istreambuf_iterator<char>(file), {}, std::back_inserter(bytes), [](char c) {
        return static_cast<std::byte>(c);
      });
      return bytes;
    });
  } catch(const std::exception& ex) {
    return err(ex.what());
  }
}

Result<std::string> read_text_file(const std::string& filename) {
  try {
    return check_file_exists(filename).and_then([&]() { return check_is_not_directory(filename); }).map([&]() {
      std::ifstream file(filename);

      std::stringstream sstr;
      sstr << file.rdbuf();
      return std::move(sstr).str();
    });
  } catch(const std::exception& ex) {
    return err(ex.what());
  }
}

} // namespace ooze
