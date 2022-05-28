cmake_minimum_required(VERSION 3.15)

project(ooze_example)

add_executable(ooze_example main.cpp)
target_link_libraries(ooze_example PRIVATE ooze)

set_target_properties(ooze_example PROPERTIES
  OUTPUT_NAME "ooze"
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
