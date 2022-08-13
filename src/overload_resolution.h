#pragma once

#include "ooze/env.h"

#include <anyf/any_function.h>

namespace ooze {

Result<anyf::AnyFunction> overload_resolution(const Env&, const std::string& name, const std::vector<anyf::TypeID>&);

}
