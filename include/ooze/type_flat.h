#pragma once

#include "ooze/src_map.h"
#include "ooze/type.h"

namespace ooze {

using TypeRef = StrongID<struct TypeSpace>;

enum class TypeTag { Leaf, Floating, Borrow, Fn, Tuple };

constexpr auto names(knot::Type<TypeTag>) { return knot::Names("Type", {"Leaf", "Floating", "Borrow", "Fn", "Tuple"}); }

using TypeGraph = Graph<TypeRef, TypeTag, SrcRef, TypeID>;

} // namespace ooze
