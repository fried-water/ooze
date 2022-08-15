#pragma once

#include "ooze/env.h"

#include <anyf/any_function.h>

namespace ooze {

Result<anyf::AnyFunction> overload_resolution(const Env&,
                                              const std::string& name,
                                              Span<anyf::TypeProperties> inputs,
                                              std::optional<Span<anyf::TypeID>> outputs = std::nullopt);

}
