#include "test.h"

#include "constructing_graph.h"

#include <algorithm>

namespace ooze {

constexpr static inline Inst fn3{3};
constexpr static inline Inst fn4{4};

BOOST_AUTO_TEST_SUITE(graph)

BOOST_AUTO_TEST_CASE(empty) {
  auto [cg, inputs] = make_graph({});
  const FunctionGraph exp{{}, 0, {{}}};
  BOOST_CHECK(exp == std::move(cg).finalize({}, {}));
}

BOOST_AUTO_TEST_CASE(empty_copy) {
  auto [cg, inputs] = make_graph({false});
  const FunctionGraph g = std::move(cg).finalize(inputs, std::array{PassBy::Copy});
  const FunctionGraph exp{{false}, 1, {{{{{{0, 0}}, 1, 1}}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_move) {
  auto [cg, inputs] = make_graph({false});
  const FunctionGraph g = std::move(cg).finalize(inputs, std::array{PassBy::Move});
  const FunctionGraph exp{{false}, 1, {{{{{{0, 0}}, 0, 1}}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_copy_ref) {
  auto [cg, inputs] = make_graph({true});
  const FunctionGraph g = std::move(cg).finalize(inputs, std::array{PassBy::Copy});
  const FunctionGraph exp{{true}, 1, {{}}, {{{{{0, 0}}, 1, 1}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_copy_multi_fwd) {
  auto [cg, inputs] = make_graph({false});
  const FunctionGraph g =
    std::move(cg).finalize(std::array{inputs[0], inputs[0]}, std::array{PassBy::Copy, PassBy::Copy});
  const FunctionGraph exp{{false}, 2, {{{{{{0, 0}, {0, 1}}, 2, 2}}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_multi_copy) {
  auto [cg, inputs] = make_graph({false, false});
  const FunctionGraph g = std::move(cg).finalize(inputs, std::array{PassBy::Copy, PassBy::Copy});
  const FunctionGraph exp{{false, false}, 2, {{{{{{0, 0}}, 1, 1}, {{{0, 1}}, 1, 1}}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_mult_copy_ref) {
  auto [cg, inputs] = make_graph({false, true});
  const FunctionGraph g = std::move(cg).finalize(inputs, std::array{PassBy::Copy, PassBy::Copy});
  const FunctionGraph exp{{false, true}, 2, {{{{{{0, 0}}, 1, 1}}}}, {{{{{0, 1}}, 1, 1}}}};
  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(empty_drop_value) {
  auto [cg, inputs] = make_graph({false});
  const FunctionGraph exp{{false}, 0, {{{}}}};
  BOOST_CHECK(exp == std::move(cg).finalize({}, {}));
}

BOOST_AUTO_TEST_CASE(empty_drop_ref) {
  auto [cg, inputs] = make_graph({true});
  const FunctionGraph exp{{true}, 0, {{}}, {{}}};
  BOOST_CHECK(exp == std::move(cg).finalize({}, {}));
}

BOOST_AUTO_TEST_CASE(single_value_fwd) {
  auto [cg, inputs] = make_graph({false});
  const auto id_outputs = cg.add(fn3, inputs, std::array{PassBy::Copy}, 1);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});

  const FunctionGraph exp{{false}, 1, {{{{{0, 0}}, 1, 1}}, {{{{1, 0}}, 1, 1}}}, {}, {{1, 0}}, {fn3}, {}, {{}}, {0}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_ref_fwd) {
  auto [cg, inputs] = make_graph({true});
  const auto id_outputs = cg.add(fn4, inputs, std::array{PassBy::Borrow}, 1);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});
  const FunctionGraph exp{{true}, 1, {{}, {{{{{1, 0}}, 1, 1}}}}, {{{{{0, 0}}, 0, 0}}}, {{0, 1}}, {fn4}, {}, {{}}, {0}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_value_alternate_fwd) {
  auto [cg, inputs] = make_graph({false});
  const Inst fn{7};
  const auto id_outputs =
    cg.add(fn,
           std::array{inputs[0], inputs[0], inputs[0], inputs[0]},
           std::array{PassBy::Copy, PassBy::Borrow, PassBy::Copy, PassBy::Borrow},
           1);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});

  const FunctionGraph exp{
    {false},
    1,
    {{{{{{0, 0}, {0, 1}, {0, 0}, {0, 1}}, 2, 2, 0}}}, {{{{{1, 0}}, 1, 1}}}},
    {},
    {{2, 2}},
    {fn},
    {{2, std::nullopt}},
    {{0, 0}}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_ref_alternate_fwd) {
  auto [cg, inputs] = make_graph({true});
  const auto id_outputs =
    cg.add(fn3,
           std::array{inputs[0], inputs[0], inputs[0], inputs[0]},
           std::array{PassBy::Copy, PassBy::Borrow, PassBy::Copy, PassBy::Borrow},
           1);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});

  const FunctionGraph exp{
    {true},
    1,
    {{}, {{{{{1, 0}}, 1, 1}}}},
    {{{{{0, 0}, {0, 1}, {0, 0}, {0, 1}}, 2, 2}}},
    {{2, 2}},
    {fn3},
    {},
    {{}},
    {0}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_ref_as_value) {
  auto [cg, inputs] = make_graph({false});
  const auto id_outputs = cg.add(fn4, inputs, std::array{PassBy::Borrow}, 1);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});
  const FunctionGraph exp{
    {false}, 1, {{{{{0, 0}}, 0, 0, 0}}, {{{{{1, 0}}, 1, 1}}}}, {}, {{0, 1}}, {fn4}, {{1, std::nullopt}}, {{0}}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_graph) {
  auto [inner_cg, inner_inputs] = make_graph({false});
  const auto id_inner_outputs = inner_cg.add(fn3, inner_inputs, std::array{PassBy::Copy}, 1);
  const FunctionGraph inner_g = std::move(inner_cg).finalize(id_inner_outputs, std::array{PassBy::Copy});

  auto [cg, inputs] = make_graph({false});
  const auto id_outputs = cg.add(inner_g, inputs);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});

  BOOST_CHECK(inner_g == g);
}

BOOST_AUTO_TEST_CASE(fwd_borrow) {
  auto [cg, inputs] = make_graph({false});
  const auto borrow_outputs = cg.add(fn3, inputs, std::array{PassBy::Borrow}, 1);
  const FunctionGraph g =
    std::move(cg).finalize(std::array{inputs[0], borrow_outputs[0]}, std::array{PassBy::Move, PassBy::Move});

  const FunctionGraph exp{
    {false}, 2, {{{{{1, 0}, {0, 0}}, 0, 1, 0}}, {{{{{1, 1}}, 0, 1}}}}, {}, {{0, 1}}, {fn3}, {{1, Term{1, 0}}}, {{0}}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_ref_as_value_graph) {
  auto [inner_cg, inner_inputs] = make_graph({true});
  const auto id_inner_outputs = inner_cg.add(fn4, inner_inputs, std::array{PassBy::Borrow}, 1);
  const FunctionGraph inner_g = std::move(inner_cg).finalize(id_inner_outputs, std::array{PassBy::Copy});

  auto [cg, inputs] = make_graph({false});
  const auto id_outputs = cg.add(inner_g, inputs);
  const FunctionGraph g = std::move(cg).finalize(id_outputs, std::array{PassBy::Copy});

  const FunctionGraph exp{
    {false}, 1, {{{{{0, 0}}, 0, 0, 0}}, {{{{{1, 0}}, 1, 1}}}}, {}, {{0, 1}}, {fn4}, {{1, std::nullopt}}, {{0}}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(single_void) {
  auto [cg, inputs] = make_graph({});
  const Inst fn{7};
  const auto id_outputs = cg.add(fn, {}, {}, 0);
  const FunctionGraph g = std::move(cg).finalize({}, {});
  const FunctionGraph exp{{}, {}, {{}, {}}, {}, {{0, 0}}, {fn}, {}, {{}}};

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_CASE(add_empty_graph) {
  auto [inner_cg, inner_inputs] = make_graph({false});
  const FunctionGraph inner_g = std::move(inner_cg).finalize(inner_inputs, std::array{PassBy::Copy});

  auto [cg, inputs] = make_graph({false});
  const FunctionGraph g = std::move(cg).finalize(cg.add(inner_g, inputs), std::array{PassBy::Copy});

  BOOST_CHECK(inner_g == g);
}

BOOST_AUTO_TEST_CASE(add_duo_graph) {
  auto [inner_cg, inner_inputs] = make_graph({false});
  const auto inner_outputs0 = inner_cg.add(fn3, inner_inputs, std::array{PassBy::Copy}, 1);
  const auto inner_outputs1 = inner_cg.add(fn3, inner_outputs0, std::array{PassBy::Copy}, 1);
  const FunctionGraph inner_g = std::move(inner_cg).finalize(inner_outputs1, std::array{PassBy::Copy});

  auto [cg, inputs] = make_graph({false});
  const FunctionGraph g = std::move(cg).finalize(cg.add(inner_g, inputs), std::array{PassBy::Copy});

  BOOST_CHECK(inner_g == g);
}

BOOST_AUTO_TEST_CASE(add_single_graph_to_single_after) {
  auto [inner_cg, inner_inputs] = make_graph({false});
  const auto inner_outputs = inner_cg.add(fn3, inner_inputs, std::array{PassBy::Copy}, 1);
  const FunctionGraph inner_g = std::move(inner_cg).finalize(inner_outputs, std::array{PassBy::Copy});

  auto [cg, inputs] = make_graph({false});
  const auto outputs = cg.add(fn3, inputs, std::array{PassBy::Copy}, 1);
  const FunctionGraph g = std::move(cg).finalize(cg.add(inner_g, outputs), std::array{PassBy::Copy});

  auto [exp_cg, exp_inputs] = make_graph({false});
  const auto exp_outputs0 = exp_cg.add(fn3, exp_inputs, std::array{PassBy::Copy}, 1);
  const auto exp_outputs1 = exp_cg.add(fn3, exp_outputs0, std::array{PassBy::Copy}, 1);
  const FunctionGraph exp = std::move(exp_cg).finalize(exp_outputs1, std::array{PassBy::Copy});

  BOOST_CHECK(exp == g);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace ooze
