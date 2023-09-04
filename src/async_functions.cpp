#include "pch.h"

#include "async_functions.h"
#include "graph_inner.h"
#include "ooze/borrowed_future.h"
#include "ooze/executor.h"
#include "ooze/future.h"

#include <algorithm>
#include <atomic>
#include <memory>
#include <optional>

namespace ooze {

namespace {

template <typename Block>
void forward_results_then_delete(std::vector<Future> results, Block* b) {
  if(results.empty()) {
    delete b;
  } else {
    for(int i = 0; i < int(results.size()); i++) {
      std::move(results[i]).then([i, b](Any a) mutable {
        std::move(b->promises[i]).send(std::move(a));
        if(decrement(b->ref_count) == 1) {
          delete b;
        }
      });
    }
  }
}

auto make_multi_promise_future(ExecutorRef ex, int count) {
  std::vector<Promise> promises;
  std::vector<Future> futures;

  promises.reserve(count);
  futures.reserve(count);

  for(int i = 0; i < count; i++) {
    auto [p, f] = make_promise_future(ex);
    promises.push_back(std::move(p));
    futures.push_back(std::move(f));
  }

  return std::pair(std::move(promises), std::move(futures));
}

struct InvocationBlock {
  AnyFunction f;

  std::atomic<int> ref_count;

  std::vector<Any> input_vals;
  std::vector<Any*> input_ptrs;
  std::vector<BorrowedFuture> borrowed_inputs;

  std::vector<Promise> promises;

  InvocationBlock(
    AnyFunction f_, std::vector<BorrowedFuture> borrowed_inputs, std::vector<Promise> promises, size_t input_count)
      : f(std::move(f_))
      , ref_count(input_count)
      , input_vals(input_count)
      , input_ptrs(input_count)
      , borrowed_inputs(std::move(borrowed_inputs))
      , promises(std::move(promises)) {}
};

struct Block {
  ExecutorRef e;
  std::atomic<int> ref_count;
  std::vector<Future> owned_inputs;
  std::vector<BorrowedFuture> borrowed_inputs;
  std::vector<Promise> promises;
};

struct IfBlock : Block {
  AsyncFn if_graph;
  AsyncFn else_graph;
};

struct ConvergeBlock : Block {
  std::variant<AsyncFn, Future> fn;
};

struct SelectBlock {
  std::atomic<int> ref_count;
  std::vector<Future> futures;
  std::vector<Promise> promises;
};

struct InputState {
  std::vector<std::vector<Future>> inputs;
  std::vector<std::vector<BorrowedFuture>> borrowed_inputs;
};

InputState propagate(InputState s, ExecutorRef e, const std::vector<ValueForward>& fwds, std::vector<Future> outputs) {
  assert(fwds.size() == outputs.size());

  for(int i = 0; i < int(fwds.size()); i++) {
    const auto& fwd = fwds[i];

    std::vector<Promise> copy_promises;
    copy_promises.reserve(fwd.copy_end);

    for(int i = 0; i < fwd.copy_end; i++) {
      auto [p, f] = make_promise_future(e);
      copy_promises.push_back(std::move(p));
      s.inputs[fwd.terms[i].node_id][fwd.terms[i].port] = std::move(f);
    }

    if(fwd.move_end == fwd.terms.size()) {
      std::optional<Promise> move_promise;
      if(fwd.copy_end != fwd.move_end) {
        const Term t = fwd.terms[fwd.copy_end];
        std::tie(move_promise, s.inputs[t.node_id][t.port]) = make_promise_future(e);
      }

      // move_only_function pls :(
      auto lambda_state =
        new std::pair<std::vector<Promise>, std::optional<Promise>>(std::move(copy_promises), std::move(move_promise));
      std::move(outputs[i]).then([=](Any a) mutable {
        for(Promise& p : lambda_state->first) std::move(p).send(a);
        if(lambda_state->second) {
          std::move(*lambda_state->second).send(std::move(a));
        }
        delete lambda_state;
      });
    } else {
      auto [borrowed_promise, f2] = make_promise_future(e);
      auto [borrowed_future, move_future] = borrow(std::move(f2));

      // move_only_function pls :(
      auto lambda_state =
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

InputState
propagate(InputState s, ExecutorRef e, const std::vector<ValueForward>& fwds, std::vector<BorrowedFuture> outputs) {
  assert(fwds.size() == outputs.size());
  for(int i = 0; i < int(fwds.size()); i++) {
    assert(fwds[i].copy_end == fwds[i].move_end);

    for(int u = 0; u < fwds[i].copy_end; u++) {
      const Term t = fwds[i].terms[u];
      auto [p, f] = make_promise_future(e);
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

void invoke_async_converge(Future cond, ConvergeBlock* b) {
  std::move(cond).then([b](Any cond) {
    if(any_cast<bool>(cond)) {
      forward_results_then_delete(std::move(b->owned_inputs), b);
    } else {
      std::vector<Future> res = std::get<AsyncFn>(b->fn)(b->e, std::move(b->owned_inputs), b->borrowed_inputs);
      b->owned_inputs.clear();
      b->owned_inputs.insert(
        b->owned_inputs.begin(), std::make_move_iterator(res.begin() + 1), std::make_move_iterator(res.end()));
      invoke_async_converge(std::move(res.front()), b);
    }
  });
}

void invoke_async_converge_future(Future cond, ConvergeBlock* b) {
  std::move(cond).then([b](Any cond) {
    if(any_cast<bool>(cond)) {
      forward_results_then_delete(std::move(b->owned_inputs), b);
    } else {
      Future future = std::move(std::get<Future>(b->fn));
      std::move(future).then([b](Any any) {
        b->fn = any_cast<AsyncFn>(std::move(any));
        std::vector<Future> res = std::get<AsyncFn>(b->fn)(b->e, std::move(b->owned_inputs), b->borrowed_inputs);
        b->owned_inputs.clear();
        b->owned_inputs.insert(
          b->owned_inputs.begin(), std::make_move_iterator(res.begin() + 1), std::make_move_iterator(res.end()));
        invoke_async_converge(std::move(res.front()), b);
      });
    }
  });
}

} // namespace

AsyncFn create_async_value(Any any) {
  return [any = std::move(any)](ExecutorRef ex, auto, auto) { return make_vector(Future(ex, any)); };
}

AsyncFn create_async(AnyFunction fn, std::vector<bool> input_borrows, int output_count) {
  return [fn = std::move(fn), input_borrows = std::move(input_borrows), output_count](
           ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture> borrowed_inputs) {
    auto [promises, futures] = make_multi_promise_future(ex, output_count);

    InvocationBlock* b = new InvocationBlock(fn, std::move(borrowed_inputs), std::move(promises), input_borrows.size());

    const auto invoke = [](InvocationBlock* b) {
      auto results = (b->f)(b->input_ptrs);

      // Drop reference to all borrowed inputs so they can be forwarded asap
      b->borrowed_inputs.clear();

      // Forward outputs
      for(size_t i = 0; i < results.size(); i++) {
        std::move(b->promises[i]).send(std::move(results[i]));
      }

      delete b;
    };

    if(input_borrows.size() == 0) {
      ex.run([b, invoke]() { invoke(b); });
    }

    int owned_offset = 0;
    int borrowed_offset = 0;
    for(size_t i = 0; i < input_borrows.size(); i++) {
      if(!input_borrows[i]) {
        std::move(inputs[owned_offset++]).then([i, b, invoke](Any value) mutable {
          b->input_vals[i] = std::move(value);
          b->input_ptrs[i] = &b->input_vals[i];
          if(decrement(b->ref_count) == 1) {
            invoke(b);
          }
        });
      } else {
        b->borrowed_inputs[borrowed_offset++].then([i, b, invoke](const Any& value) {
          b->input_ptrs[i] = const_cast<Any*>(&value);
          if(decrement(b->ref_count) == 1) {
            invoke(b);
          }
        });
      }
    }

    return std::move(futures);
  };
}

AsyncFn create_async_graph(FunctionGraph fg) {
  return [fg = std::move(fg)](ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture> borrowed_inputs) {
    const FunctionGraph::State& g = *fg.state;

    InputState s;
    s.inputs.reserve(g.input_counts.size() + 1);
    s.borrowed_inputs.reserve(g.input_counts.size());

    for(const auto& [owned, borrowed] : g.input_counts) {
      s.inputs.push_back(std::vector<Future>(owned));
      s.borrowed_inputs.push_back(std::vector<BorrowedFuture>(borrowed));
    }

    s.inputs.push_back(std::vector<Future>(g.output_count));
    // can't return borrowed_inputs

    s = propagate(std::move(s), ex, g.owned_fwds.front(), std::move(inputs));
    s = propagate(std::move(s), ex, g.input_borrowed_fwds, std::move(borrowed_inputs));

    for(int i = 0; i < int(g.fns.size()); i++) {
      std::vector<Future> results = g.fns[i](ex, std::move(s.inputs[i]), std::move(s.borrowed_inputs[i]));
      s = propagate(std::move(s), ex, g.owned_fwds[i + 1], std::move(results));
    }

    return std::move(s.inputs.back());
  };
}

AsyncFn create_async_functional(int output_count) {
  return [=](ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture> borrowed_inputs) {
    auto [promises, futures] = make_multi_promise_future(ex, output_count);

    Future fn = std::move(inputs[0]);
    inputs.erase(inputs.begin());

    Block* b = new Block{ex, output_count, std::move(inputs), std::move(borrowed_inputs), std::move(promises)};
    std::move(fn).then([b](Any fn) {
      forward_results_then_delete(any_cast<AsyncFn>(fn)(b->e, std::move(b->owned_inputs), b->borrowed_inputs), b);
    });

    return std::move(futures);
  };
}

AsyncFn create_async_if(int output_count, AsyncFn if_graph, AsyncFn else_graph) {
  return [=](ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture> borrowed_inputs) {
    auto [promises, futures] = make_multi_promise_future(ex, output_count);

    Future cond = std::move(inputs[0]);
    inputs.erase(inputs.begin());

    IfBlock* b = new IfBlock{
      {ex, output_count, std::move(inputs), std::move(borrowed_inputs), std::move(promises)}, if_graph, else_graph};

    std::move(cond).then([b](Any cond) {
      forward_results_then_delete(
        any_cast<bool>(cond) ? b->if_graph(b->e, std::move(b->owned_inputs), b->borrowed_inputs)
                             : b->else_graph(b->e, std::move(b->owned_inputs), b->borrowed_inputs),
        b);
    });

    return std::move(futures);
  };
}

AsyncFn create_async_select() {
  return [](ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture>) {
    const int output_count = int(inputs.size()) / 2;

    auto [promises, futures] = make_multi_promise_future(ex, output_count);

    Future cond = std::move(inputs[0]);
    inputs.erase(inputs.begin());

    SelectBlock* b = new SelectBlock{output_count, std::move(inputs), std::move(promises)};

    std::move(cond).then([b](Any cond) mutable {
      if(any_cast<bool>(cond)) {
        b->futures.erase(b->futures.begin() + b->futures.size() / 2, b->futures.end());
      } else {
        b->futures.erase(b->futures.begin(), b->futures.begin() + b->futures.size() / 2);
      }
      forward_results_then_delete(std::move(b->futures), b);
    });

    return std::move(futures);
  };
}

AsyncFn create_async_converge() {
  return [=](ExecutorRef ex, std::vector<Future> inputs, std::vector<BorrowedFuture> borrowed_inputs) {
    const int output_count = int(inputs.size()) - 2;
    auto [promises, futures] = make_multi_promise_future(ex, output_count);

    Future fn = std::move(inputs[0]);
    Future cond = std::move(inputs[1]);
    inputs.erase(inputs.begin(), inputs.begin() + 2);

    invoke_async_converge_future(
      std::move(cond),
      new ConvergeBlock{{ex, output_count, std::move(inputs), std::move(borrowed_inputs), std::move(promises)},
                        std::move(fn)});

    return std::move(futures);
  };
}

} // namespace ooze
