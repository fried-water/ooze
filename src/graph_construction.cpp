#include "pch.h"

#include "graph_construction.h"

namespace ooze {

using namespace ast;

namespace {

using anyf::Term;

std::vector<Term> add_expr(anyf::ConstructingGraph& cg,
                           const Expr& expr,
                           const Env& e,
                           const Map<std::string, Term> bindings,
                           const Map<std::string, FunctionGraph>& graphs) {

  return std::visit(
    Overloaded{[&](const Indirect<Call>& call) {
                 std::vector<Term> inputs;
                 for(const Expr& expr : call->parameters) {
                   const auto terms = add_expr(cg, expr, e, bindings, graphs);
                   check(terms.size() == 1, "arguments cannot be tuples");
                   inputs.push_back(terms.front());
                 }

                 const auto it = graphs.find(call->name);

                 check(it != graphs.end() || e.contains_function(call->name), "cannot find function {}", call->name);

                 return it != graphs.end() ? cg.add(it->second, inputs) : cg.add(e.function(call->name), inputs);
               },
               [&](const std::string& identifier) {
                 const auto it = bindings.find(identifier);
                 check(it != bindings.end(), "{} not found", identifier);
                 return std::vector<Term>{it->second};
               },
               [&](const Literal& l) {
                 // TODO support function binding
                 check(false, "TODO support function binding");
                 return std::vector<Term>{};
               }},
    expr.v);
}

FunctionGraph create_graph(const Function& f, const Env& e, const Map<std::string, FunctionGraph>& graphs) {
  std::vector<anyf::Type> input_types;

  for(const Parameter& p : f.parameters) {
    anyf::check(e.contains_type(p.type), "type {} not found", p.type);
    input_types.push_back(p.borrow ? e.type(p.type).borrowed_type : e.type(p.type).type);
  }

  auto [cg, iterms] = anyf::make_graph(input_types);

  Map<std::string, Term> bindings;

  for(int i = 0; i < iterms.size(); i++) {
    bindings.emplace(f.parameters[i].name, iterms[i]);
  }

  for(const Assignment& assignment : f.assignments) {
    const std::vector<Term> terms = add_expr(cg, assignment.expr, e, bindings, graphs);

    for(int i = 0; i < terms.size(); i++) {
      bindings.emplace(assignment.variables[i].name, terms[i]);
    }
  }

  std::vector<Term> outputs;

  if(f.ret.size() == 1) {
    outputs = add_expr(cg, f.ret.front(), e, bindings, graphs);
  } else {
    for(const Expr& expr : f.ret) {
      std::vector<Term> single_output = add_expr(cg, f.ret.front(), e, bindings, graphs);
      check(single_output.size() == 1, "cannot have nested tuples");
      outputs.push_back(single_output.front());
    }
  }
  return anyf::finalize(std::move(cg), outputs);
}

} // namespace

Map<std::string, FunctionGraph> create_graphs(const Env& e, const AST& ast) {
  Map<std::string, std::vector<std::string>> dependencies;

  for(const Function& function : ast) {
    dependencies.emplace(
      function.name,
      knot::preorder_accumulate<std::vector<std::string>>(function, [&](std::vector<std::string> v, const Call& c) {
        v.push_back(c.name);
        return v;
      }));
  }

  Map<std::string, FunctionGraph> graphs;

  const auto satisfied = [&](const std::string& dep) {
    return e.contains_function(dep) || graphs.find(dep) != graphs.end();
  };

  auto it = ast.begin();
  while(it != ast.end()) {
    it = std::find_if(ast.begin(), ast.end(), [&](const Function& f) {
      const std::vector<std::string>& deps = dependencies.at(f.name);
      return graphs.find(f.name) == graphs.end() && std::all_of(deps.begin(), deps.end(), satisfied);
    });

    if(it != ast.end()) {
      graphs.emplace(it->name, create_graph(*it, e, graphs));
    }
  }

  for(const Function& f : ast) {
    if(graphs.find(f.name) == graphs.end()) {
      fmt::print("Error generating {}()\n", f.name);
      for(const std::string& dep : dependencies.at(f.name)) {
        if(!satisfied(dep)) {
          fmt::print("  unfound dependency {}()\n", dep);
        }
      }
    }
  }

  return graphs;
}

} // namespace ooze
