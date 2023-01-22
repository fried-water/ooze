#pragma once

#include "ooze/env.h"

#include <anyf/borrowed_future.h>
#include <anyf/executor.h>
#include <anyf/future.h>

#include <tl/expected.hpp>

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T>
using Result = tl::expected<T, std::vector<std::string>>;

struct Binding {
  TypeID type;
  anyf::Future future;
  anyf::BorrowedFuture borrowed_future;
};

struct RuntimeEnv {
  Env env;
  anyf::Executor executor;
  std::unordered_map<std::string, Binding> bindings;

  ~RuntimeEnv() { executor.wait(); }
};

RuntimeEnv make_default_runtime(Env);

Result<void> parse_script(Env&, std::string_view script);

Result<std::vector<Binding>> run(RuntimeEnv&, std::string_view expr);
Result<std::vector<std::string>> run_to_string(RuntimeEnv&, std::string_view expr);

Result<std::vector<Binding>> run_assign(RuntimeEnv&, std::string_view assignment_or_expr);
Result<std::vector<std::string>> run_to_string_assign(RuntimeEnv&, std::string_view assignment_or_expr);

int main(int argc, const char** argv, Env);

} // namespace ooze
