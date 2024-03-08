#pragma once

#include "function_graph.h"
#include "inst.h"

#include "ooze/any.h"
#include "ooze/any_fn.h"
#include "ooze/traits.h"

#include <utility>
#include <vector>

namespace ooze {

using Inst = StrongID<struct InstSpace, i32>;

enum class InstOp : u8 { Value, Fn, Graph, Functional, If, Select, While, Curry, Placeholder };

constexpr auto names(knot::Type<InstOp>) {
  return knot::Names("InstOp", {"Value", "Fn", "Graph", "Functional", "If", "Select", "While", "Curry", "Placeholder"});
}

struct AnyFnInst {
  AnyFn fn;
  std::vector<bool> input_borrows;
  int output_count;
};

struct FunctionalInst {
  i32 output_count;
};

struct IfInst {
  Inst if_inst;
  Inst else_inst;
  i32 output_count = 0;

  // [common, if, else]
  std::array<i32, 2> value_offsets = {};
  std::array<i32, 2> borrow_offsets = {};
};

struct WhileInst {
  Inst cond_inst;
  Inst body_inst;
  i32 output_count = 0;

  // [ret_common, ret_body, inv_common, inv_body, inv_cond]
  std::array<i32, 4> value_offsets = {};

  // [common, body, cond]
  std::array<i32, 2> borrow_offsets = {};
};

struct SelectInst {};

struct Program {
  std::vector<InstOp> inst;
  std::vector<i32> inst_data;

  std::vector<Any> values;
  std::vector<AnyFnInst> fns;
  std::vector<FunctionGraph> graphs;
  std::vector<IfInst> ifs;
  std::vector<WhileInst> whiles;

  // TODO handle curry with Inst instead of Any?
  std::vector<std::pair<Inst, Slice>> currys;

  Inst add(Any);
  Inst add(FunctionGraph);
  Inst add(FunctionalInst);
  Inst add(IfInst);
  Inst add(SelectInst);
  Inst add(WhileInst);

  Inst add(AnyFn fn, std::vector<bool> input_borrows, int ret_size) {
    inst.push_back(InstOp::Fn);
    inst_data.push_back(int(fns.size()));
    fns.push_back({std::move(fn), std::move(input_borrows), ret_size});
    return Inst{i32(inst.size() - 1)};
  }

  Inst curry(Inst, Span<Any>);

  Inst placeholder() {
    inst.push_back(InstOp::Placeholder);
    inst_data.push_back(-1);
    return Inst{i32(inst.size() - 1)};
  }

  void set(Inst, FunctionGraph);
  void set(Inst, Inst, Span<Any>);

  template <typename F>
  Inst add_fn(F&& f) {
    constexpr auto f_type = decay(knot::Type<F>{});
    std::vector<bool> input_borrows;
    input_borrows.reserve(size(args(f_type)));
    visit(args(f_type), [&](auto type) { input_borrows.push_back(is_const_ref(type)); });
    return add(create_any_fn(std::forward<F>(f)), std::move(input_borrows), int(size(return_types(f_type))));
  }
};

} // namespace ooze
