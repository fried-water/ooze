cmake_minimum_required(VERSION 3.1)

project(ctre)

add_library(ctre INTERFACE)
target_compile_features(ctre INTERFACE cxx_std_17)
target_include_directories(ctre INTERFACE .)
