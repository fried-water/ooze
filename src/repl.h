#pragma once

#include "ooze/env.h"

#include <string_view>

namespace ooze {

void run_repl(const Env&, std::string_view script = {});

}
