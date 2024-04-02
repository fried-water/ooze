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

auto make_multi_promise_future(int count) {
  std::vector<Promise> promises;
  std::vector<Future> futures;

  promises.reserve(count);
  futures.reserve(count);

  for(int i = 0; i < count; i++) {
    auto [p, f] = make_promise_future();
    promises.push_back(std::move(p));
    futures.push_back(std::move(f));
  }

  return std::pair(std::move(promises), std::move(futures));
}

struct InvocationBlock {
  std::function<std::vector<Any>(Span<Any*>)> f;

  std::atomic<int> ref_count;

  std::vector<Any> input_vals;
  std::vector<Any*> input_ptrs;
  std::vector<BorrowedFuture> borrowed_inputs;

  std::vector<Promise> promises;

  InvocationBlock(std::function<std::vector<Any>(Span<Any*>)> f_,
                  std::vector<BorrowedFuture> borrowed_inputs,
                  std::vector<Promise> promises,
                  size_t input_count)
      : f(std::move(f_))
      , ref_count(int(input_count))
      , input_vals(input_count)
      , input_ptrs(input_count)
      , borrowed_inputs(std::move(borrowed_inputs))
      , promises(std::move(promises)) {}
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

InputState propagate(InputState s, const std::vector<ValueForward>& fwds, std::vector<Future> outputs) {
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

InputState propagate(InputState s, const std::vector<ValueForward>& fwds, std::vector<BorrowedFuture> outputs) {
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

std::vector<Future>
execute(const AnyFn& fn,
        const std::vector<bool>& input_borrows,
        int output_count,
        ExecutorRef ex,
        std::vector<Future> inputs,
        std::vector<BorrowedFuture> borrowed_inputs) {
  if(input_borrows.empty()) {
    return transform_to_vec(fn(std::array<Any*, 0>{}), Construct<Future>());
  } else {
    auto [promises, futures] = make_multi_promise_future(output_count);
    auto* b = new InvocationBlock(fn, std::move(borrowed_inputs), std::move(promises), input_borrows.size());

    auto invoke = [ex](InvocationBlock* b) mutable {
      ex.run([b]() {
        std::vector<Any> results = (b->f)(b->input_ptrs);

        // Drop reference to all borrowed inputs so they can be forwarded asap
        b->borrowed_inputs.clear();

        // Forward outputs
        for(size_t i = 0; i < results.size(); i++) {
          std::move(b->promises[i]).send(std::move(results[i]));
        }

        delete b;
      });
    };

    int owned_offset = 0;
    int borrowed_offset = 0;
    for(size_t i = 0; i < input_borrows.size(); i++) {
      if(!input_borrows[i]) {
        std::move(inputs[owned_offset++]).then([i, b, invoke](Any value) mutable {
          b->input_vals[i] = std::move(value);
          b->input_ptrs[i] = &b->input_vals[i];
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

    return std::move(futures);
  }
}

std::vector<Future> execute(const std::shared_ptr<const Program>& p,
                            const FunctionGraph& g,
                            ExecutorRef ex,
                            std::vector<Future> inputs,
                            std::vector<BorrowedFuture> borrowed_inputs) {
  InputState s;
  s.inputs.reserve(g.input_counts.size() + 1);
  s.borrowed_inputs.reserve(g.input_counts.size());

  for(const auto [owned, borrowed] : g.input_counts) {
    s.inputs.emplace_back(owned);
    s.borrowed_inputs.emplace_back(borrowed);
  }

  s.inputs.emplace_back(g.output_count);
  // can't return borrowed_inputs

  s = propagate(std::move(s), g.owned_fwds.front(), std::move(inputs));
  s = propagate(std::move(s), g.input_borrowed_fwds, std::move(borrowed_inputs));

  for(int i = 0; i < int(g.insts.size()); i++) {
    std::vector<Future> results = execute(p, g.insts[i], ex, std::move(s.inputs[i]), std::move(s.borrowed_inputs[i]));
    s = propagate(std::move(s), g.owned_fwds[i + 1], std::move(results));
  }

  return std::move(s.inputs.back());
}

std::vector<Future> execute_functional(std::shared_ptr<const Program> p,
                                       int output_count,
                                       ExecutorRef ex,
                                       std::vector<Future> inputs,
                                       std::vector<BorrowedFuture> borrowed_inputs) {
  auto [promises, futures] = make_multi_promise_future(output_count);

  Future fn = std::move(inputs[0]);
  inputs.erase(inputs.begin());

  auto* b = new Block{std::move(p), ex, std::move(inputs), std::move(borrowed_inputs), std::move(promises)};
  std::move(fn).then([b](Any fn) mutable {
    connect_multi(
      execute(std::move(b->p), any_cast<Inst>(fn), b->e, std::move(b->owned_inputs), std::move(b->borrowed_inputs)),
      std::move(b->promises));
    delete b;
  });

  return std::move(futures);
}

std::vector<Future> execute_if(std::shared_ptr<const Program> p,
                               const IfInst& inst,
                               ExecutorRef ex,
                               std::vector<Future> inputs,
                               std::vector<BorrowedFuture> borrowed_inputs) {
  auto [promises, futures] = make_multi_promise_future(inst.output_count);

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

    connect_multi(
      execute(std::move(b->p), cond ? inst.if_inst : inst.else_inst, b->e, std::move(inputs), std::move(borrows)),
      std::move(b->promises));
    delete b;
  });

  return std::move(futures);
}

std::vector<Future> execute_select(std::vector<Future> inputs) {
  auto [promises, futures] = make_multi_promise_future(int(inputs.size()) / 2);

  Future cond = std::move(inputs[0]);
  inputs.erase(inputs.begin());

  auto* b = new SelectBlock{std::move(inputs), std::move(promises)};

  std::move(cond).then([b](Any cond) mutable {
    assert(holds_alternative<bool>(cond));

    if(any_cast<bool>(cond)) {
      b->futures.erase(b->futures.begin() + i64(b->futures.size() / 2), b->futures.end());
    } else {
      b->futures.erase(b->futures.begin(), b->futures.begin() + i64(b->futures.size() / 2));
    }
    connect_multi(std::move(b->futures), std::move(b->promises));
    delete b;
  });

  return std::move(futures);
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

  auto cond_result = execute(b->p, b->cond, b->e, std::move(cond_inputs), b->cond_borrowed);

  assert(cond_result.size() == 1);

  std::move(cond_result[0]).then([b](Any cond) {
    assert(holds_alternative<bool>(cond));

    if(any_cast<bool>(cond)) {
      for(Future& f : b->body_copy) {
        auto [f1, f2] = clone(std::move(f));
        f = std::move(f1);
        b->accumulator.push_back(std::move(f2));
      }

      b->accumulator = execute(b->p, b->body, b->e, std::move(b->accumulator), b->body_borrowed);
      execute_while(b);
    } else {
      connect_multi(std::move(b->accumulator), std::move(b->promises));
      delete b;
    }
  });
}

} // namespace

std::vector<Future> execute(std::shared_ptr<const Program> p,
                            Inst inst,
                            ExecutorRef ex,
                            std::vector<Future> inputs,
                            std::vector<BorrowedFuture> borrowed_inputs) {

  switch(p->inst[inst.get()]) {
  case InstOp::Value: return make_vector(Future(p->values[p->inst_data[inst.get()]]));
  case InstOp::Fn: {
    const auto& [fn, input_borrows, output_count] = p->fns[p->inst_data[inst.get()]];
    return execute(fn, input_borrows, output_count, ex, std::move(inputs), std::move(borrowed_inputs));
  }
  case InstOp::Graph:
    return execute(p, p->graphs[p->inst_data[inst.get()]], ex, std::move(inputs), std::move(borrowed_inputs));
  case InstOp::Functional:
    return execute_functional(p, p->inst_data[inst.get()], ex, std::move(inputs), std::move(borrowed_inputs));
  case InstOp::If:
    return execute_if(p, p->ifs[p->inst_data[inst.get()]], ex, std::move(inputs), std::move(borrowed_inputs));
  case InstOp::Select: return execute_select(std::move(inputs));
  case InstOp::While: {
    const WhileInst& w = p->whiles[p->inst_data[inst.get()]];
    auto [promises, futures] = make_multi_promise_future(w.output_count);
    execute_while(create_while_block(p, ex, w, std::move(inputs), std::move(borrowed_inputs), std::move(promises)));
    return std::move(futures);
  }
  case InstOp::Curry: {
    const auto [curry_inst, slice] = p->currys[p->inst_data[inst.get()]];
    for(i32 i = slice.begin; i < slice.end; i++) {
      inputs.emplace_back(p->values[i]);
    }
    return execute(std::move(p), curry_inst, ex, std::move(inputs), std::move(borrowed_inputs));
  }
  case InstOp::Placeholder: assert(false); return {};
  }

  assert(false);
  return {};
}

} // namespace ooze
