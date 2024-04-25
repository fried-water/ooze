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

struct TailCall {
  Inst inst;
  std::vector<Any> inputs;
  std::vector<const Any*> borrowed_inputs;
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

std::optional<TailCall>
execute_graph(const FunctionGraph& g,
              const Program& p,
              bool parallel,
              std::span<Any> inputs,
              std::span<const Any*> borrowed_inputs,
              std::span<Any> outputs) {
  ExecutionCtx ctx = {
    std::vector<std::vector<Any>>(g.input_counts.size() + (g.tailcall ? 0 : 1)),
    std::vector<std::vector<const Any*>>(g.input_counts.size()),
    std::vector<std::atomic<int>>(g.input_counts.size() + (g.tailcall ? 0 : 1)),
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

  if(g.tailcall) {
    // Ensure tailcall never executes
    ctx.ref_counts[*g.tailcall]++;
  } else {
    // Ensure output never executes
    ctx.ref_counts.back() = g.output_count + 1;
    ctx.inputs.back() = std::vector<Any>(g.output_count);
  }

  // Start 0 input tasks
  for(int i = 0; i < std::ssize(g.input_counts); i++) {
    const auto [owned, borrowed] = g.input_counts[i];
    if(owned + borrowed == 0 && g.tailcall != i) {
      execute_node(ctx, g, p, i);
    }
  }

  propagate(ctx, g, p, g.owned_fwds.front(), inputs);
  propagate(ctx, g, p, g.input_borrowed_fwds, borrowed_inputs);

  if(ctx.tg) {
    ctx.tg->wait();
  }

  if(g.tailcall) {
    return TailCall{
      g.insts[*g.tailcall], std::move(ctx.inputs[*g.tailcall]), std::move(ctx.borrowed_inputs[*g.tailcall])};
  } else {
    std::move(ctx.inputs.back().begin(), ctx.inputs.back().end(), outputs.begin());
    return std::nullopt;
  }
}

TailCall execute_if(const IfInst& inst, std::span<Any> inputs, std::span<const Any*> borrowed_inputs) {
  assert(holds_alternative<bool>(inputs[0]));
  const bool cond = any_cast<bool>(inputs[0]);
  inputs = inputs.subspan(1);

  if(cond) {
    return TailCall{inst.if_inst,
                    std::vector<Any>(std::make_move_iterator(inputs.begin()),
                                     std::make_move_iterator(inputs.begin() + inst.value_offsets[1])),
                    std::vector<const Any*>(borrowed_inputs.begin(), borrowed_inputs.begin() + inst.borrow_offsets[1])};
  } else {
    const auto input_it =
      std::move(inputs.begin() + inst.value_offsets[1], inputs.end(), inputs.begin() + inst.value_offsets[0]);

    const auto borrow_it =
      std::move(borrowed_inputs.begin() + inst.borrow_offsets[1],
                borrowed_inputs.end(),
                borrowed_inputs.begin() + inst.borrow_offsets[0]);

    return TailCall{inst.else_inst,
                    std::vector<Any>(std::make_move_iterator(inputs.begin()), std::make_move_iterator(input_it)),
                    std::vector<const Any*>(borrowed_inputs.begin(), borrow_it)};
  }
}

TailCall execute_curry(
  const std::pair<Inst, Slice>& curry, const Program& p, std::span<Any> inputs, std::span<const Any*> borrowed_inputs) {
  const auto [curry_inst, slice] = curry;

  std::vector<Any> curried_inputs;
  curried_inputs.reserve(inputs.size() + size(slice));

  std::move(inputs.begin(), inputs.end(), std::back_inserter(curried_inputs));
  std::copy(p.values.begin() + slice.begin, p.values.begin() + slice.end, std::back_inserter(curried_inputs));

  return TailCall{
    curry_inst, std::move(curried_inputs), std::vector<const Any*>(borrowed_inputs.begin(), borrowed_inputs.end())};
}

// TODO expose synchronous api?
std::optional<TailCall> execute_tailcall(
  const Program& p,
  bool parallel,
  Inst inst,
  std::span<Any> inputs,
  std::span<const Any*> borrowed_inputs,
  std::span<Any> outputs) {
  assert(outputs.size() == p.output_counts[inst.get()]);

  switch(p.inst[inst.get()]) {
  case InstOp::Value: outputs[0] = p.values[p.inst_data[inst.get()]]; return std::nullopt;
  case InstOp::Fn: p.fns[p.inst_data[inst.get()]](inputs, borrowed_inputs, outputs.data()); return std::nullopt;
  case InstOp::Graph:
    return execute_graph(p.graphs[p.inst_data[inst.get()]], p, parallel, inputs, borrowed_inputs, outputs);
  case InstOp::Functional:
    return execute_tailcall(p, parallel, any_cast<Inst>(inputs[0]), inputs.subspan(1), borrowed_inputs, outputs);
  case InstOp::If: return execute_if(p.ifs[p.inst_data[inst.get()]], inputs, borrowed_inputs);
  case InstOp::Curry: return execute_curry(p.currys[p.inst_data[inst.get()]], p, inputs, borrowed_inputs);
  case InstOp::Placeholder: assert(false); return std::nullopt;
  }

  assert(false);
  return std::nullopt;
}

void execute(const Program& p,
             bool parallel,
             Inst inst,
             std::span<Any> inputs,
             std::span<const Any*> borrowed_inputs,
             std::span<Any> outputs) {

  std::optional<TailCall> tailcall = execute_tailcall(p, parallel, inst, inputs, borrowed_inputs, outputs);

  while(tailcall) {
    tailcall = execute_tailcall(p, parallel, tailcall->inst, tailcall->inputs, tailcall->borrowed_inputs, outputs);
  }
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
