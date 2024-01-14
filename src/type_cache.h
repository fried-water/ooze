#pragma once

#include "ooze/type.h"

namespace ooze {

struct TypeCache {
  Type floating;
  Type borrow_floating;
  Type fn_floating;
  Type unit;
  Type boolean;
};

inline TypeCache create_type_cache(TypeGraph& g) {
  const Type floating = g.add_node(TypeTag::Floating, TypeID{});

  return {floating,
          g.add_node(std::array{floating}, TypeTag::Borrow, TypeID{}),
          g.add_node(std::array{floating, floating}, TypeTag::Fn, TypeID{}),
          g.add_node(TypeTag::Tuple, TypeID{}),
          g.add_node(TypeTag::Leaf, type_id(knot::Type<bool>{}))};
}

} // namespace ooze
