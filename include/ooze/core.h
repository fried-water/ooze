#pragma once

#include "ooze/env.h"
#include "ooze/result.h"
#include "ooze/tree.h"

#include <anyf/borrowed_future.h>
#include <anyf/executor.h>
#include <anyf/future.h>

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T>
using StringResult = Result<T, std::vector<std::string>>;

struct Binding {
  TypeID type;
  anyf::Future future;
  anyf::BorrowedFuture borrowed_future;
};

struct RuntimeEnv {
  Env env;
  anyf::Executor executor;
  std::unordered_map<std::string, Tree<Binding>> bindings;
};

Tree<Any> await(Tree<Binding>);

CompoundType<TypeID> type(const Tree<Binding>&);

RuntimeEnv make_default_runtime(Env);

StringResult<void> parse_script(Env&, std::string_view script);

StringResult<Tree<Binding>> run(RuntimeEnv&, std::string_view expr);
StringResult<std::string> run_to_string(RuntimeEnv&, std::string_view expr);

StringResult<Tree<Binding>> run_or_assign(RuntimeEnv&, std::string_view assignment_or_expr);
StringResult<std::string> run_to_string_or_assign(RuntimeEnv&, std::string_view assignment_or_expr);

int main(int argc, const char** argv, Env);

} // namespace ooze
