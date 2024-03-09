#include "pch.h"

#include "runtime.h"

#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"

#include <algorithm>
#include <atomic>
#include <memory>
#include <optional>

namespace ooze {

namespace {

template <typename Futures, typename Promises>
void connect_multi(Futures&& futures, Promises&& promises) {
  assert(futures.size() == promises.size());
  for(int i = 0; i < futures.size(); i++) {
    connect(std::move(futures[i]), std::move(promises[i]));
  }
}

std::vector<Promise> make_multi_promise_future(std::span<Future> futures) {
  std::vector<Promise> promises(futures.size());
  for(int i = 0; i < std::ssize(futures); i++) {
    std::tie(promises[i], futures[i]) = make_promise_future();
  }
  return promises;
}

struct InvocationBlock {
  AnyFn f;

  std::atomic<int> ref_count;

  // [inputs, outputs]
  std::vector<Any> any_buffer;
  std::vector<Any*> input_ptrs;
  std::vector<BorrowedFuture> borrowed_inputs;

  std::vector<Promise> promises;

  InvocationBlock(
    AnyFn f_, std::vector<BorrowedFuture> borrowed_inputs, std::vector<Promise> promises_, size_t input_count)
      : f(std::move(f_))
      , ref_count(int(input_count))
      , any_buffer(input_count + promises_.size())
      , input_ptrs(input_count)
      , borrowed_inputs(std::move(borrowed_inputs))
      , promises(std::move(promises_)) {}
};

struct Block {
  std::shared_ptr<const Program> p;
  ExecutorRef e;
  std::vector<Future> owned_inputs;
  std::vector<BorrowedFuture> borrowed_inputs;
  std::vector<Promise> promises;
};

struct IfBlock : Block {
  IfInst inst;
};

struct WhileBlock {
  std::shared_ptr<const Program> p;
  ExecutorRef e;

  Inst cond;
  Inst body;
  i32 shared_acc = 0;

  std::vector<Promise> promises;
  std::vector<Future> accumulator;

  std::vector<Future> cond_copy;
  std::vector<Future> body_copy;

  std::vector<BorrowedFuture> cond_borrowed;
  std::vector<BorrowedFuture> body_borrowed;
};

struct SelectBlock {
  std::vector<Future> futures;
  std::vector<Promise> promises;
};

struct InputState {
  std::vector<std::vector<Future>> inputs;
  std::vector<std::vector<BorrowedFuture>> borrowed_inputs;
};

InputState propagate(InputState s, std::span<const ValueForward> fwds, std::span<Future> outputs) {
  assert(fwds.size() == outputs.size());

  for(int i = 0; i < int(fwds.size()); i++) {
    const auto& fwd = fwds[i];

    std::vector<Promise> copy_promises;
    copy_promises.reserve(fwd.copy_end);

    for(int i = 0; i < fwd.copy_end; i++) {
      auto [p, f] = make_promise_future();
      copy_promises.push_back(std::move(p));
      s.inputs[fwd.terms[i].node_id][fwd.terms[i].port] = std::move(f);
    }

    if(fwd.move_end == fwd.terms.size()) {
      std::optional<Promise> move_promise;
      if(fwd.copy_end != fwd.move_end) {
        const Term t = fwd.terms[fwd.copy_end];
        std::tie(move_promise, s.inputs[t.node_id][t.port]) = make_promise_future();
      }

      // move_only_function pls :(
      auto* lambda_state =
        new std::pair<std::vector<Promise>, std::optional<Promise>>(std::move(copy_promises), std::move(move_promise));
      std::move(outputs[i]).then([=](Any a) mutable {
        for(Promise& p : lambda_state->first) std::move(p).send(a);
        if(lambda_state->second) {
          std::move(*lambda_state->second).send(std::move(a));
        }
        delete lambda_state;
      });
    } else {
      auto [borrowed_promise, f2] = make_promise_future();
      auto [borrowed_future, move_future] = borrow(std::move(f2));

      // move_only_function pls :(
      auto* lambda_state =
        new std::pair<std::vector<Promise>, Promise>(std::move(copy_promises), std::move(borrowed_promise));
      std::move(outputs[i]).then([=](Any a) mutable {
        for(Promise& p : lambda_state->first) std::move(p).send(a);
        std::move(lambda_state->second).send(std::move(a));
        delete lambda_state;
      });

      for(int i = fwd.move_end; i < fwd.terms.size(); i++) {
        s.borrowed_inputs[fwd.terms[i].node_id][fwd.terms[i].port] = borrowed_future;
      }

      if(fwd.copy_end != fwd.move_end) {
        const Term t = fwd.terms[fwd.copy_end];
        s.inputs[t.node_id][t.port] = std::move(move_future);
      }
    }
  }

  return s;
}

InputState propagate(InputState s, std::span<const ValueForward> fwds, std::vector<BorrowedFuture> outputs) {
  assert(fwds.size() == outputs.size());
  for(int i = 0; i < int(fwds.size()); i++) {
    assert(fwds[i].copy_end == fwds[i].move_end);

    for(int u = 0; u < fwds[i].copy_end; u++) {
      const Term t = fwds[i].terms[u];
      auto [p, f] = make_promise_future();
      s.inputs[t.node_id][t.port] = std::move(f);
      outputs[i].then([p = std::make_shared<Promise>(std::move(p))](const Any& a) { std::move(*p).send(a); });
    }

    for(int u = fwds[i].copy_end; u < int(fwds[i].terms.size()); u++) {
      const Term t = fwds[i].terms[u];
      s.borrowed_inputs[t.node_id][t.port] = outputs[i];
    }
  }
  return s;
}

void execute(const AnyFnInst& inst,
             ExecutorRef ex,
             std::vector<Future> inputs,
             std::vector<BorrowedFuture> borrowed_inputs,
             std::vector<Promise> promises) {
  if(inst.input_borrows.empty() && promises.size() < 10) {
    std::array<Any, 10> result_buffer;
    inst.fn({}, result_buffer.data());

    for(int i = 0; i < promises.size(); i++) {
      std::move(promises[i]).send(std::move(result_buffer[i]));
    }
  } else if(inst.input_borrows.empty()) {
    std::unique_ptr<Any[]> result_buffer = std::make_unique<Any[]>(promises.size());
    inst.fn({}, result_buffer.get());

    for(int i = 0; i < promises.size(); i++) {
      std::move(promises[i]).send(std::move(result_buffer[i]));
    }
  } else {
    auto* b = new InvocationBlock(inst.fn, std::move(borrowed_inputs), std::move(promises), inst.input_borrows.size());

    auto invoke = [ex](InvocationBlock* b) mutable {
      ex.run([b]() {
        const size_t input_count = b->input_ptrs.size();
        (b->f)(b->input_ptrs, b->any_buffer.data() + input_count);

        // Drop reference to all borrowed inputs so they can be forwarded asap
        b->borrowed_inputs.clear();

        // Forward outputs
        for(size_t i = 0; i < b->promises.size(); i++) {
          std::move(b->promises[i]).send(std::move(b->any_buffer[input_count + i]));
        }

        delete b;
      });
    };

    int owned_offset = 0;
    int borrowed_offset = 0;
    for(size_t i = 0; i < inst.input_borrows.size(); i++) {
      if(!inst.input_borrows[i]) {
        std::move(inputs[owned_offset++]).then([i, b, invoke](Any value) mutable {
          b->any_buffer[i] = std::move(value);
          b->input_ptrs[i] = &b->any_buffer[i];
          if(decrement(b->ref_count)) {
            invoke(b);
          }
        });
      } else {
        b->borrowed_inputs[borrowed_offset++].then([i, b, invoke](const Any& value) mutable {
          b->input_ptrs[i] = const_cast<Any*>(&value);
          if(decrement(b->ref_count)) {
            invoke(b);
          }
        });
      }
    }
  }
}

void execute(const std::shared_ptr<const Program>& p,
             const FunctionGraph& g,
             ExecutorRef ex,
             std::vector<Future> inputs,
             std::vector<BorrowedFuture> borrowed_inputs,
             std::span<Future> outputs) {
  InputState s;
  s.inputs.reserve(g.input_counts.size() + 1);
  s.borrowed_inputs.reserve(g.input_counts.size());

  for(const auto [owned, borrowed] : g.input_counts) {
    s.inputs.emplace_back(owned);
    s.borrowed_inputs.emplace_back(borrowed);
  }

  s.inputs.emplace_back(g.output_count);
  // can't return borrowed_inputs

  s = propagate(std::move(s), g.owned_fwds.front(), inputs);
  s = propagate(std::move(s), g.input_borrowed_fwds, std::move(borrowed_inputs));

  const auto max_it =
    stdr::max_element(g.insts, [&](Inst x, Inst y) { return p->output_counts[x.get()] < p->output_counts[y.get()]; });

  std::vector<Future> results(max_it == g.insts.end() ? 0 : p->output_counts[max_it->get()]);
  for(int i = 0; i < int(g.insts.size()); i++) {
    std::span<Future> result_span(results.data(), p->output_counts[g.insts[i].get()]);
    execute(p, g.insts[i], ex, std::move(s.inputs[i]), std::move(s.borrowed_inputs[i]), result_span);
    s = propagate(std::move(s), g.owned_fwds[i + 1], result_span);
  }

  std::move(s.inputs.back().begin(), s.inputs.back().end(), outputs.begin());
}

void execute_functional(std::shared_ptr<const Program> p,
                        ExecutorRef ex,
                        std::vector<Future> inputs,
                        std::vector<BorrowedFuture> borrowed_inputs,
                        std::vector<Promise> promises) {
  Future fn = std::move(inputs[0]);
  inputs.erase(inputs.begin());

  auto* b = new Block{std::move(p), ex, std::move(inputs), std::move(borrowed_inputs), std::move(promises)};
  std::move(fn).then([b](Any fn) mutable {
    std::vector<Future> results(b->promises.size());
    execute(
      std::move(b->p), any_cast<Inst>(fn), b->e, std::move(b->owned_inputs), std::move(b->borrowed_inputs), results);
    connect_multi(std::move(results), std::move(b->promises));
    delete b;
  });
}

void execute_if(std::shared_ptr<const Program> p,
                const IfInst& inst,
                ExecutorRef ex,
                std::vector<Future> inputs,
                std::vector<BorrowedFuture> borrowed_inputs,
                std::vector<Promise> promises) {
  Future cond = std::move(inputs[0]);
  inputs.erase(inputs.begin());

  auto* b = new IfBlock{{std::move(p), ex, std::move(inputs), std::move(borrowed_inputs), std::move(promises)}, inst};

  std::move(cond).then([b](Any any_cond) {
    assert(holds_alternative<bool>(any_cond));

    const bool cond = any_cast<bool>(any_cond);
    const IfInst inst = b->inst;

    auto& inputs = b->owned_inputs;
    auto& borrows = b->borrowed_inputs;

    if(cond) {
      inputs.erase(inputs.begin() + inst.value_offsets[1], inputs.end());
      borrows.erase(borrows.begin() + inst.borrow_offsets[1], borrows.end());
    } else {
      inputs.erase(inputs.begin() + inst.value_offsets[0], inputs.begin() + inst.value_offsets[1]);
      borrows.erase(borrows.begin() + inst.borrow_offsets[0], borrows.begin() + inst.borrow_offsets[1]);
    }

    std::vector<Future> results(b->promises.size());
    execute(
      std::move(b->p), cond ? inst.if_inst : inst.else_inst, b->e, std::move(inputs), std::move(borrows), results);
    connect_multi(std::move(results), std::move(b->promises));
    delete b;
  });
}

void execute_select(std::vector<Future> inputs, std::vector<Promise> promises) {
  Future cond = std::move(inputs[0]);
  inputs.erase(inputs.begin());

  auto* b = new SelectBlock{std::move(inputs), std::move(promises)};

  std::move(cond).then([b](Any cond) mutable {
    assert(holds_alternative<bool>(cond));

    const bool c = any_cast<bool>(cond);

    const auto begin = c ? b->futures.begin() : b->futures.begin() + i64(b->futures.size() / 2);
    const auto end = c ? b->futures.begin() + i64(b->futures.size() / 2) : b->futures.end();

    connect_multi(stdr::subrange(begin, end), std::move(b->promises));
    delete b;
  });
}

WhileBlock* create_while_block(std::shared_ptr<const Program> p,
                               ExecutorRef ex,
                               const WhileInst& inst,
                               std::vector<Future> inputs,
                               std::vector<BorrowedFuture> borrowed,
                               std::vector<Promise> promises) {
  WhileBlock* b =
    new WhileBlock{std::move(p), ex, inst.cond_inst, inst.body_inst, inst.value_offsets[0], std::move(promises)};

  std::move(inputs.begin(), inputs.begin() + inst.value_offsets[1], std::back_inserter(b->accumulator));

  for(int i = inst.value_offsets[1]; i < inst.value_offsets[2]; i++) {
    auto [f1, f2] = clone(std::move(inputs[i]));
    b->cond_copy.push_back(std::move(f1));
    b->body_copy.push_back(std::move(f2));
  }

  std::move(
    inputs.begin() + inst.value_offsets[2], inputs.begin() + inst.value_offsets[3], std::back_inserter(b->body_copy));
  std::move(inputs.begin() + inst.value_offsets[3], inputs.end(), std::back_inserter(b->cond_copy));

  for(int i = 0; i < inst.borrow_offsets[0]; i++) {
    b->cond_borrowed.push_back(borrowed[i]);
    b->body_borrowed.push_back(std::move(borrowed[i]));
  }

  std::move(borrowed.begin() + inst.borrow_offsets[0],
            borrowed.begin() + inst.borrow_offsets[1],
            std::back_inserter(b->body_borrowed));
  std::move(borrowed.begin() + inst.borrow_offsets[1], borrowed.end(), std::back_inserter(b->cond_borrowed));

  return b;
}

void execute_while(WhileBlock* b) {
  std::vector<Future> cond_inputs;
  cond_inputs.reserve(b->shared_acc + b->cond_copy.size());

  for(int i = 0; i < b->shared_acc; i++) {
    auto [f1, f2] = clone(std::move(b->accumulator[i]));
    b->accumulator[i] = std::move(f1);
    cond_inputs.push_back(std::move(f2));
  }

  for(Future& f : b->cond_copy) {
    auto [f1, f2] = clone(std::move(f));
    f = std::move(f1);
    cond_inputs.push_back(std::move(f2));
  }

  Future cond_result;
  execute(b->p, b->cond, b->e, std::move(cond_inputs), b->cond_borrowed, {&cond_result, 1});

  std::move(cond_result).then([b](Any cond) {
    assert(holds_alternative<bool>(cond));

    if(any_cast<bool>(cond)) {
      for(Future& f : b->body_copy) {
        auto [f1, f2] = clone(std::move(f));
        f = std::move(f1);
        b->accumulator.push_back(std::move(f2));
      }

      std::vector<Future> results(b->p->output_counts[b->body.get()]);
      execute(b->p, b->body, b->e, std::move(b->accumulator), b->body_borrowed, results);
      b->accumulator = std::move(results);
      execute_while(b);
    } else {
      connect_multi(std::move(b->accumulator), std::move(b->promises));
      delete b;
    }
  });
}

} // namespace

void execute(std::shared_ptr<const Program> p,
             Inst inst,
             ExecutorRef ex,
             std::vector<Future> inputs,
             std::vector<BorrowedFuture> borrowed_inputs,
             std::span<Future> outputs) {
  assert(outputs.size() == p->output_counts[inst.get()]);

  switch(p->inst[inst.get()]) {
  case InstOp::Value: outputs[0] = Future(p->values[p->inst_data[inst.get()]]); return;
  case InstOp::Fn: {
    return execute(p->fns[p->inst_data[inst.get()]],
                   ex,
                   std::move(inputs),
                   std::move(borrowed_inputs),
                   make_multi_promise_future(outputs));
  }
  case InstOp::Graph:
    return execute(p, p->graphs[p->inst_data[inst.get()]], ex, std::move(inputs), std::move(borrowed_inputs), outputs);
  case InstOp::Functional:
    return execute_functional(p, ex, std::move(inputs), std::move(borrowed_inputs), make_multi_promise_future(outputs));
  case InstOp::If:
    return execute_if(
      p,
      p->ifs[p->inst_data[inst.get()]],
      ex,
      std::move(inputs),
      std::move(borrowed_inputs),
      make_multi_promise_future(outputs));
  case InstOp::Select: return execute_select(std::move(inputs), make_multi_promise_future(outputs));
  case InstOp::While: {
    execute_while(create_while_block(
      p,
      ex,
      p->whiles[p->inst_data[inst.get()]],
      std::move(inputs),
      std::move(borrowed_inputs),
      make_multi_promise_future(outputs)));
    return;
  }
  case InstOp::Curry: {
    const auto [curry_inst, slice] = p->currys[p->inst_data[inst.get()]];
    for(i32 i = slice.begin; i < slice.end; i++) {
      inputs.emplace_back(p->values[i]);
    }
    return execute(std::move(p), curry_inst, ex, std::move(inputs), std::move(borrowed_inputs), outputs);
  }
  case InstOp::Placeholder: assert(false); return;
  }

  assert(false);
}

} // namespace ooze
