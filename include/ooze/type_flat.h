#pragma once

namespace ooze {

using TypeRef = StrongID<struct TypeSpace>;

enum class TypeTag { Leaf, Floating, Borrow, Fn, Tuple };

constexpr auto names(knot::Type<TypeTag>) { return knot::Names("Type", {"Leaf", "Floating", "Borrow", "Fn", "Tuple"}); }

} // namespace ooze
