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

find_package(fmt REQUIRED)

add_subdirectory(ext/ctre)
add_subdirectory(ext/knot)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_options(-fsanitize=address)
  add_link_options(-fsanitize=address)
endif()

add_library(ooze
  src/env.cpp
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
    knot
  PRIVATE
    ctre fmt CLI11::CLI11)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  add_subdirectory(example)
  enable_testing()
  add_subdirectory(test)
endif()
