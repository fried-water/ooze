#include "pch.h"

#include "program.h"

namespace ooze {

namespace {

Inst add_internal(Program& p, InstOp op, int output_count, int inst_data = -1) {
  p.inst.push_back(op);
  p.output_counts.push_back(output_count);
  p.inst_data.push_back(inst_data);
  return Inst{i32(p.inst.size() - 1)};
}

} // namespace

Inst Program::add(Any a) {
  values.push_back(std::move(a));
  return add_internal(*this, InstOp::Value, 1, i32(values.size() - 1));
}

Inst Program::add(AnyFn fn, int output_count) {
  fns.push_back(std::move(fn));
  return add_internal(*this, InstOp::Fn, output_count, i32(fns.size() - 1));
}

Inst Program::add(FunctionGraph g) {
  const Inst i = add_internal(*this, InstOp::Graph, g.output_count, i32(graphs.size()));
  graphs.push_back(std::move(g));
  return i;
}

Inst Program::add(FunctionalInst, int output_count) { return add_internal(*this, InstOp::Functional, output_count); }

Inst Program::add(IfInst inst, int output_count) {
  const Inst i = add_internal(*this, InstOp::If, output_count, i32(ifs.size()));
  ifs.push_back(inst);
  return i;
}

Inst Program::curry(Inst curried, Span<Any> s) {
  const Inst i = add_internal(*this, InstOp::Curry, output_counts[curried.get()], i32(currys.size()));
  currys.emplace_back(curried, Slice{i32(values.size()), i32(values.size() + s.size())});
  values.insert(values.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
  return i;
}

Inst Program::placeholder() { return add_internal(*this, InstOp::Placeholder, -1, -1); }

void Program::set(Inst i, FunctionGraph g) {
  assert(inst[i.get()] == InstOp::Placeholder);
  inst[i.get()] = InstOp::Graph;
  output_counts[i.get()] = g.output_count;
  inst_data[i.get()] = i32(graphs.size());
  graphs.push_back(std::move(g));
}

void Program::set(Inst i, Inst curried, Span<Any> s) {
  assert(inst[i.get()] == InstOp::Placeholder);
  inst[i.get()] = InstOp::Curry;
  output_counts[i.get()] = output_counts[curried.get()];
  inst_data[i.get()] = i32(currys.size());
  currys.emplace_back(curried, Slice{i32(values.size()), i32(values.size() + s.size())});
  values.insert(values.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
}

} // namespace ooze
