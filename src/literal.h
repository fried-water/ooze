#pragma once

namespace ooze {

struct FileLiteral {
  std::string name;
  KNOT_COMPAREABLE(FileLiteral);
};

using Literal = std::variant<bool, std::string, FileLiteral, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64>;

inline std::string to_string(const Literal& v) {
  return std::visit(Overloaded{[](bool b) { return std::string(b ? "true" : "false"); },
                               [](const std::string& s) { return fmt::format("\"{}\"", s); },
                               [](const FileLiteral& f) { return fmt::format("@{}", f.name); },
                               [](i8 i) { return fmt::format("'{}'", i); },
                               [](i16 i) { return fmt::format("{}i16", i); },
                               [](i32 i) { return fmt::format("{}i32", i); },
                               [](i64 i) { return fmt::format("{}i64", i); },
                               [](u8 u) { return fmt::format("'{}u8'", u); },
                               [](u16 u) { return fmt::format("{}u16", u); },
                               [](u32 u) { return fmt::format("{}u32", u); },
                               [](u64 u) { return fmt::format("{}u64", u); },
                               [](f32 f) { return fmt::format("{}f", f); },
                               [](f64 f) { return fmt::format("{}", f); }},
                    v);
}

} // namespace ooze
