#pragma once

#include "ooze/env.h"

#include <anyf/borrowed_future.h>
#include <anyf/executor/task_executor.h>
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

std::pair<Env, std::vector<std::string>> parse_script(Env, std::string_view script);

std::pair<std::unordered_map<std::string, Binding>, Result<std::vector<Binding>>>
run(const Env&, anyf::TaskExecutor&, std::string_view assignment_or_expr, std::unordered_map<std::string, Binding>);

std::vector<std::string> to_string(const Env&, anyf::TaskExecutor&, std::vector<Binding>);

int main(int argc, char* argv[], Env);

} // namespace ooze
