#pragma once

#include "ooze/env.h"
#include "ooze/result.h"
#include "ooze/tree.h"
#include "ooze/type.h"

#include <anyf/borrowed_future.h>
#include <anyf/executor.h>
#include <anyf/future.h>

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ooze {

template <typename T, typename... Ts>
using StringResult = Result<T, std::vector<std::string>, Ts...>;

struct Binding {
  CompoundType<TypeID> type;
  anyf::Future future;
  anyf::BorrowedFuture borrowed_future;
};

using Bindings = std::unordered_map<std::string, Tree<Binding>>;

Tree<Any> await(Tree<Binding>);

CompoundType<TypeID> type(const Tree<Binding>&);

StringResult<void, Env> parse_script(Env, std::string_view script);

StringResult<Tree<Binding>, Env, Bindings> run(anyf::ExecutorRef, Env, Bindings, std::string_view expr);
StringResult<std::string, Env, Bindings> run_to_string(anyf::ExecutorRef, Env, Bindings, std::string_view expr);

int main(int argc, const char** argv, Env);

} // namespace ooze
