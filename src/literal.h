#pragma once

#include "lexer.h"

#include <anyf/type.h>

namespace ooze {

using Literal = std::variant<bool, std::string, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64>;

std::optional<Literal> to_literal(TokenType, std::string_view);

inline anyf::TypeID type_of(const Literal& v) {
  return std::visit([](const auto& ele) { return anyf::type_id(knot::decay(knot::Type<decltype(ele)>{})); }, v);
}

inline std::string to_string(const Literal& v) {
  return std::visit(Overloaded{[](bool b) { return std::string(b ? "true" : "false"); },
                               [](const std::string& s) { return fmt::format("\"{}\"", s); },
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
