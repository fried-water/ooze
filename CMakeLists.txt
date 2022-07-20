cmake_minimum_required(VERSION 3.15)

project(ooze)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to Release as none was specified.")
  set(CMAKE_BUILD_TYPE Debug)
endif()

find_package(fmt REQUIRED)

add_subdirectory(ext/AnyFunction)
add_subdirectory(ext/ctre)

add_library(ooze
  src/env.cpp
  src/ast.cpp
  src/lexer.cpp
  src/io.cpp
  src/parser.cpp
  src/graph_construction.cpp
  src/repl.cpp
  src/queries.cpp
  src/ooze.cpp)

target_compile_features(ooze PUBLIC cxx_std_17)
target_include_directories(ooze PRIVATE src PUBLIC include)
target_precompile_headers(ooze PRIVATE src/pch.h)
target_link_libraries(ooze
  PUBLIC
    any_function
  PRIVATE
    ctre fmt)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  add_subdirectory(example)
  enable_testing()
  add_subdirectory(test)
endif()
