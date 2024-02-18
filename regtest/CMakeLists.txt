cmake_minimum_required(VERSION 3.15)

project(ooze_regtest)

add_executable(regtest regtest.cpp)
target_link_libraries(regtest PRIVATE ooze)
