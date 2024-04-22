#include "pch.h"

#include "runtime.h"

#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <memory>
#include <optional>
#include <vector>

namespace ooze {

namespace {

template <typename T, std::size_t N>
class SSOBuffer : public std::span<T> {
  std::variant<std::unique_ptr<T[]>, std::array<T, N>> _data;

public:
  SSOBuffer() = default;
  explicit SSOBuffer(size_t count) {
    if(count < N) {
      _data = std::array<T, N>{};
      static_cast<std::span<T>&>(*this) = std::span(std::get<1>(_data).data(), count);
    } else {
      _data = std::make_unique<T[]>(count);
      static_cast<std::span<T>&>(*this) = std::span(std::get<0>(_data).get(), count);
    }
  }

  SSOBuffer(const SSOBuffer&) = delete;
  SSOBuffer& operator=(const SSOBuffer&) = delete;

  SSOBuffer(SSOBuffer&&) = delete;
  SSOBuffer& operator=(SSOBuffer&&) = delete;
};

struct InvocationBlock {
  std::shared_ptr<const Program> p;
  Inst inst;

  bool parallel = false;

  std::atomic<int> ref_count;

  std::vector<Any> any_buffer; // [inputs + outputs]
  std::vector<const Any*> borrowed_inputs;
  std::vector<BorrowedFuture> borrowed_futures;

  std::vector<Promise> promises;
};

void execute(const Program&,
             bool parallel,
             Inst,
             std::span<Any> inputs,
             std::span<const Any*> borrowed_inputs,
             std::span<Any> outputs);

struct ExecutionCtx {
  std::vector<std::vector<Any>> inputs;
  std::vector<std::vector<const Any*>> borrowed_inputs;
  std::vector<std::atomic<int>> ref_counts;
  std::vector<std::pair<std::atomic<int>, Any>> borrow_cleanups;
  std::optional<tbb::task_group> tg;
};

void propagate(ExecutionCtx&, const FunctionGraph&, const Program&, std::span<const ValueForward>, std::span<Any>);

void execute_node(ExecutionCtx& ctx, const FunctionGraph& g, const Program& p, int i);

void fwd_owned(ExecutionCtx& ctx, const FunctionGraph& g, const Program& p, Term t, Any a) {
  ctx.inputs[t.node_id][t.port] = std::move(a);
  if(decrement(ctx.ref_counts[t.node_id])) {
    execute_node(ctx, g, p, t.node_id);
  }
}

void fwd_borrow(ExecutionCtx& ctx, const FunctionGraph& g, const Program& p, Term t, const Any* a) {
  ctx.borrowed_inputs[t.node_id][t.port] = a;
  if(decrement(ctx.ref_counts[t.node_id])) {
    execute_node(ctx, g, p, t.node_id);
  }
}

void execute_node(ExecutionCtx& ctx, const FunctionGraph& g, const Program& p, int i) {
  const auto exec = [&ctx, &p, &g, i]() {
    auto outputs = SSOBuffer<Any, 10>(p.output_counts[g.insts[i].get()]);
    execute(p, ctx.tg.has_value(), g.insts[i], ctx.inputs[i], ctx.borrowed_inputs[i], outputs);
    propagate(ctx, g, p, g.owned_fwds[i + 1], outputs);
    for(int cleanup_idx : g.node_borrows[i]) {
      if(auto& [rc, any] = ctx.borrow_cleanups[cleanup_idx]; decrement(rc)) {
        if(const auto fwd = g.borrow_cleanups[cleanup_idx].fwd; fwd) {
          fwd_owned(ctx, g, p, *fwd, std::move(any));
        } else {
          any = {};
        }
      }
    }
  };

  if(ctx.tg) {
    ctx.tg->run(exec);
  } else {
    exec();
  }
}

void propagate(ExecutionCtx& ctx,
               const FunctionGraph& g,
               const Program& p,
               std::span<const ValueForward> fwds,
               std::span<Any> values) {
  assert(fwds.size() == values.size());

  for(int i = 0; i < int(fwds.size()); i++) {
    const auto& fwd = fwds[i];

    // Propagate to all copy dsts
    for(int u = 0; u < fwd.copy_end; u++) {
      fwd_owned(ctx, g, p, fwd.terms[u], values[i]);
    }

    if(fwd.move_end != fwd.terms.size()) {
      // Move to fixed position then propagate borrows
      ctx.borrow_cleanups[fwd.cleanup_idx].second = std::move(values[i]);
      const Any* borrow = &ctx.borrow_cleanups[fwd.cleanup_idx].second;
      for(int u = fwd.move_end; u < std::ssize(fwds[i].terms); u++) {
        fwd_borrow(ctx, g, p, fwd.terms[u], borrow);
      }
    } else if(fwd.copy_end != fwd.move_end) {
      // Propagate move if not being borrowed
      fwd_owned(ctx, g, p, fwd.terms[fwd.copy_end], std::move(values[i]));
    }
  }
}

void propagate(ExecutionCtx& ctx,
               const FunctionGraph& g,
               const Program& p,
               std::span<const ValueForward> fwds,
               std::span<const Any*> borrows) {
  assert(fwds.size() == borrows.size());
  for(int i = 0; i < std::ssize(fwds); i++) {
    assert(fwds[i].copy_end == fwds[i].move_end);

    for(int u = 0; u < fwds[i].copy_end; u++) {
      fwd_owned(ctx, g, p, fwds[i].terms[u], *borrows[i]);
    }

    for(int u = fwds[i].copy_end; u < int(fwds[i].terms.size()); u++) {
      fwd_borrow(ctx, g, p, fwds[i].terms[u], borrows[i]);
    }
  }
}

void execute_graph(const FunctionGraph& g,
                   const Program& p,
                   bool parallel,
                   std::span<Any> inputs,
                   std::span<const Any*> borrowed_inputs,
                   std::span<Any> outputs) {
  ExecutionCtx ctx = {
    std::vector<std::vector<Any>>(g.input_counts.size() + 1),
    std::vector<std::vector<const Any*>>(g.input_counts.size()),
    std::vector<std::atomic<int>>(g.input_counts.size() + 1),
    std::vector<std::pair<std::atomic<int>, Any>>(g.borrow_cleanups.size()),
  };

  if(parallel) {
    ctx.tg.emplace(); // tbb::task_group is not copyable or moveable
  }

  // Initialize inputs
  for(int i = 0; i < std::ssize(g.input_counts); i++) {
    const auto [owned, borrowed] = g.input_counts[i];
    ctx.ref_counts[i] = owned + borrowed;
    ctx.inputs[i] = std::vector<Any>(owned);
    ctx.borrowed_inputs[i] = std::vector<const Any*>(borrowed);
  }

  for(int i = 0; i < std::ssize(g.borrow_cleanups); i++) {
    ctx.borrow_cleanups[i].first = g.borrow_cleanups[i].count;
  }

  ctx.inputs.back() = std::vector<Any>(g.output_count);
  // can't return borrowed_inputs

  // Ensure output never executes
  ctx.ref_counts.back() = g.output_count + 1;

  // Start 0 input tasks
  for(int i = 0; i < std::ssize(g.input_counts); i++) {
    const auto [owned, borrowed] = g.input_counts[i];
    if(owned + borrowed == 0) {
      execute_node(ctx, g, p, i);
    }
  }

  propagate(ctx, g, p, g.owned_fwds.front(), inputs);
  propagate(ctx, g, p, g.input_borrowed_fwds, borrowed_inputs);

  if(ctx.tg) {
    ctx.tg->wait();
  }

  std::move(ctx.inputs.back().begin(), ctx.inputs.back().end(), outputs.begin());
}

void execute_fn(
  const AnyFnInst& inst, std::span<Any> inputs, std::span<const Any*> borrowed_inputs, std::span<Any> outputs) {
  constexpr std::size_t SSO_SIZE = 10;
  const std::size_t input_count = inputs.size() + borrowed_inputs.size();

  auto input_buffer = SSOBuffer<Any, SSO_SIZE>(input_count);
  auto ptr_buffer = SSOBuffer<Any*, SSO_SIZE>(input_count);

  int owned_offset = 0;
  int borrowed_offset = 0;
  for(size_t i = 0; i < inst.input_borrows.size(); i++) {
    if(inst.input_borrows[i]) {
      ptr_buffer[i] = const_cast<Any*>(borrowed_inputs[borrowed_offset++]);
    } else {
      input_buffer[i] = std::move(inputs[owned_offset++]);
      ptr_buffer[i] = &input_buffer[i];
    }
  }

  inst.fn(ptr_buffer, outputs.data());
}

void execute_if(const IfInst& inst,
                const Program& p,
                bool parallel,
                std::span<Any> inputs,
                std::span<const Any*> borrowed_inputs,
                std::span<Any> outputs) {
  assert(holds_alternative<bool>(inputs[0]));
  const bool cond = any_cast<bool>(inputs[0]);
  inputs = inputs.subspan(1);

  if(cond) {
    inputs = inputs.first(inst.value_offsets[1]);
    borrowed_inputs = borrowed_inputs.first(inst.borrow_offsets[1]);
  } else {
    const auto input_it =
      std::rotate(inputs.begin() + inst.value_offsets[0], inputs.begin() + inst.value_offsets[1], inputs.end());

    const auto borrow_it =
      std::rotate(borrowed_inputs.begin() + inst.borrow_offsets[0],
                  borrowed_inputs.begin() + inst.borrow_offsets[1],
                  borrowed_inputs.end());

    inputs = std::span(inputs.begin(), input_it);
    borrowed_inputs = std::span(borrowed_inputs.begin(), borrow_it);
  }

  execute(p, parallel, cond ? inst.if_inst : inst.else_inst, inputs, borrowed_inputs, outputs);
}

void execute_select(std::span<Any> inputs, std::span<Any> outputs) {
  assert(holds_alternative<bool>(inputs[0]));
  const bool cond = any_cast<bool>(inputs[0]);
  inputs = inputs.subspan(1);

  const auto begin = cond ? inputs.begin() : inputs.begin() + i64(inputs.size() / 2);
  const auto end = cond ? inputs.begin() + i64(inputs.size() / 2) : inputs.end();

  std::move(begin, end, outputs.begin());
}

void execute_curry(const std::pair<Inst, Slice>& curry,
                   const Program& p,
                   bool parallel,
                   std::span<Any> inputs,
                   std::span<const Any*> borrowed_inputs,
                   std::span<Any> outputs) {
  const auto [curry_inst, slice] = curry;
  auto curried_inputs = SSOBuffer<Any, 10>(inputs.size() + size(slice));
  std::move(inputs.begin(), inputs.end(), curried_inputs.begin());
  std::copy(p.values.begin() + slice.begin, p.values.begin() + slice.end, curried_inputs.begin() + inputs.size());
  return execute(p, parallel, curry_inst, curried_inputs, borrowed_inputs, outputs);
}

// TODO expose synchronous api?
void execute(const Program& p,
             bool parallel,
             Inst inst,
             std::span<Any> inputs,
             std::span<const Any*> borrowed_inputs,
             std::span<Any> outputs) {
  assert(outputs.size() == p.output_counts[inst.get()]);

  switch(p.inst[inst.get()]) {
  case InstOp::Value: outputs[0] = p.values[p.inst_data[inst.get()]]; return;
  case InstOp::Fn: return execute_fn(p.fns[p.inst_data[inst.get()]], inputs, borrowed_inputs, outputs);
  case InstOp::Graph:
    return execute_graph(p.graphs[p.inst_data[inst.get()]], p, parallel, inputs, borrowed_inputs, outputs);
  case InstOp::Functional:
    return execute(p, parallel, any_cast<Inst>(inputs[0]), inputs.subspan(1), borrowed_inputs, outputs);
  case InstOp::If: return execute_if(p.ifs[p.inst_data[inst.get()]], p, parallel, inputs, borrowed_inputs, outputs);
  case InstOp::Select: return execute_select(inputs, outputs);
  case InstOp::Curry:
    return execute_curry(p.currys[p.inst_data[inst.get()]], p, parallel, inputs, borrowed_inputs, outputs);
  case InstOp::Placeholder: assert(false); return;
  }

  assert(false);
}

} // namespace

void execute(std::shared_ptr<const Program> p,
             Inst inst,
             Executor& ex,
             std::vector<Future> inputs,
             std::vector<BorrowedFuture> borrowed_inputs,
             std::span<Future> outputs) {

  std::vector<Promise> promises(outputs.size());
  for(int i = 0; i < std::ssize(outputs); i++) {
    std::tie(promises[i], outputs[i]) = make_promise_future();
  }

  const auto total_inputs = inputs.size() + borrowed_inputs.size();

  auto* b = new InvocationBlock{
    std::move(p),
    inst,
    ex.parallel(),
    int(total_inputs),
    std::vector<Any>(inputs.size() + outputs.size()),
    std::vector<const Any*>(borrowed_inputs.size()),
    std::move(borrowed_inputs),
    std::move(promises)};

  const auto invoke = [](Executor& ex, InvocationBlock* b) {
    ex.run([b]() {
      const size_t output_count = b->promises.size();
      const size_t owned_count = b->any_buffer.size() - output_count;

      auto owned_inputs = std::span(b->any_buffer.begin(), owned_count);
      auto outputs = std::span(b->any_buffer.begin() + owned_count, output_count);

      execute(*b->p, b->parallel, b->inst, owned_inputs, b->borrowed_inputs, outputs);

      // Drop reference to all borrowed futures so they can be forwarded asap
      b->borrowed_futures.clear();

      // Forward outputs
      for(size_t i = 0; i < b->promises.size(); i++) {
        std::move(b->promises[i]).send(std::move(outputs[i]));
      }

      delete b;
    });
  };

  if(total_inputs == 0) {
    invoke(ex, b);
  } else {
    // Save this since it's possible for b to get deleted partially through
    const size_t borrowed_input_count = b->borrowed_inputs.size();

    for(size_t i = 0; i < inputs.size(); i++) {
      std::move(inputs[i]).then([&ex, i, b, invoke](Any value) {
        b->any_buffer[i] = std::move(value);
        if(decrement(b->ref_count)) {
          invoke(ex, b);
        }
      });
    }

    for(size_t i = 0; i < borrowed_input_count; i++) {
      b->borrowed_futures[i].then([&ex, i, b, invoke](const Any& value) {
        b->borrowed_inputs[i] = &value;
        if(decrement(b->ref_count)) {
          invoke(ex, b);
        }
      });
    }
  }
}

} // namespace ooze
