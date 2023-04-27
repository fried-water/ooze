cmake_minimum_required(VERSION 3.15)

project(ooze)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to Release as none was specified.")
  set(CMAKE_BUILD_TYPE Release)
endif()

find_package(fmt REQUIRED)

add_subdirectory(ext/AnyFunction)
add_subdirectory(ext/ctre)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_options(-fsanitize=address)
  add_link_options(-fsanitize=address)
endif()

add_library(ooze
  src/env.cpp
  src/lexer.cpp
  src/literal.cpp
  src/io.cpp
  src/parser.cpp
  src/pretty_print.cpp
  src/graph_construction.cpp
  src/type_check.cpp
  src/repl.cpp
  src/user_msg.cpp
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
