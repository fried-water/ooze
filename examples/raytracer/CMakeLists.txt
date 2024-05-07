cmake_minimum_required(VERSION 3.15)
project(raytracer)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(raytracer
  src/collision.cpp
  src/image.cpp
  src/raytrace.cpp
  src/main.cpp)

target_include_directories(raytracer PRIVATE src ext/stb)
target_link_libraries(raytracer PRIVATE ooze fmt::fmt)
