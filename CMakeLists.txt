cmake_minimum_required(VERSION 3.15)

project(ooze)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to Release as none was specified.")
  set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(FetchContent)

FetchContent_Declare(
    cli11
    GIT_REPOSITORY https://github.com/CLIUtils/CLI11
    GIT_TAG v2.3.2
)
FetchContent_MakeAvailable(cli11)

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 10.2.1
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
    ctre
    GIT_REPOSITORY https://github.com/hanickadot/compile-time-regular-expressions
    GIT_TAG v2.10
)
FetchContent_MakeAvailable(ctre)

FetchContent_Declare(
    knot
    GIT_REPOSITORY https://github.com/fried-water/knot
    GIT_TAG 8bdbefe68e4d0baf4f30e41d0aed512382ad128b
)
FetchContent_MakeAvailable(knot)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_options(-fsanitize=address)
  add_link_options(-fsanitize=address)
endif()

option(USE_CLANG_TIDY "Run clang-tidy with the compiler." OFF)
if(USE_CLANG_TIDY)
  set(CMAKE_CXX_CLANG_TIDY clang-tidy)
endif()

add_library(ooze
  src/constructing_graph.cpp
  src/function_graph_construction.cpp
  src/lexer.cpp
  src/io.cpp
  src/ooze.cpp
  src/parser.cpp
  src/program.cpp
  src/pretty_print.cpp
  src/repl.cpp
  src/runtime.cpp
  src/sema.cpp
  src/type_check.cpp
  src/user_msg.cpp)

target_compile_features(ooze PUBLIC cxx_std_17)
target_include_directories(ooze PRIVATE src PUBLIC include)
target_precompile_headers(ooze PRIVATE src/pch.h)
target_link_libraries(ooze
  PUBLIC
    knot tbb
  PRIVATE
    ctre fmt::fmt CLI11::CLI11)

option(OOZE_BUILD_TESTS "Build tests" ON)
if(OOZE_BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
  add_subdirectory(regtest)
endif()
