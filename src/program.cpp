#include "pch.h"

#include "ooze/program.h"

namespace ooze {

namespace {

Inst add_internal(Program& p, InstOp op, int inst_data = -1) {
  p.inst.push_back(op);
  p.inst_data.push_back(inst_data);
  return Inst{i32(p.inst.size() - 1)};
}

} // namespace

Inst Program::add(Any a) {
  values.push_back(std::move(a));
  return add_internal(*this, InstOp::Value, i32(values.size() - 1));
}

Inst Program::add(FunctionGraph g) {
  const Inst i = add_internal(*this, InstOp::Graph, i32(graphs.size()));
  graphs.push_back(std::move(g));
  return i;
}

Inst Program::add(FunctionalInst inst) { return add_internal(*this, InstOp::Functional, inst.output_count); }

Inst Program::add(IfInst inst) {
  const Inst i = add_internal(*this, InstOp::If, i32(ifs.size()));
  ifs.emplace_back(inst);
  return i;
}

Inst Program::add(SelectInst) { return add_internal(*this, InstOp::Select); }
Inst Program::add(ConvergeInst) { return add_internal(*this, InstOp::Converge); }

Inst Program::curry(Inst curried, Span<Any> s) {
  const Inst i = add_internal(*this, InstOp::Curry, i32(currys.size()));
  currys.emplace_back(curried, Slice{i32(values.size()), i32(values.size() + s.size())});
  values = to_vec(std::move(s), std::move(values));
  return i;
}

void Program::set(Inst i, FunctionGraph g) {
  assert(inst[i.get()] == InstOp::Placeholder);
  inst[i.get()] = InstOp::Graph;
  inst_data[i.get()] = i32(graphs.size());
  graphs.push_back(std::move(g));
}

void Program::set(Inst i, Inst curried, Span<Any> s) {
  assert(inst[i.get()] == InstOp::Placeholder);
  inst[i.get()] = InstOp::Curry;
  inst_data[i.get()] = i32(currys.size());
  currys.emplace_back(curried, Slice{i32(values.size()), i32(values.size() + s.size())});
  values = to_vec(std::move(s), std::move(values));
}

} // namespace ooze
