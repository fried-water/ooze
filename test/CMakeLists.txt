cmake_minimum_required(VERSION 3.15)

project(ooze_test)

find_package(Boost REQUIRED COMPONENTS unit_test_framework system)
find_package(tbb REQUIRED)

add_executable(ooze_test
  test_main.cpp
  any_function_test.cpp
  any_test.cpp
  borrowed_future_test.cpp
  executor_test.cpp
  future_test.cpp
  graph_test.cpp
  lexer_test.cpp
  ooze_test.cpp
  parser_combinator_test.cpp
  parser_test.cpp
  pretty_print_test.cpp
  repl_test.cpp
  result_test.cpp
  stress_test.cpp
  type_test.cpp
  type_check_test.cpp)

target_link_libraries(ooze_test PRIVATE ooze tbb ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
target_include_directories(ooze_test PRIVATE ../src)
target_precompile_headers(ooze_test PRIVATE ../src/pch.h)

set_target_properties(ooze_test PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME OozeUnitTests COMMAND ooze_tests)
