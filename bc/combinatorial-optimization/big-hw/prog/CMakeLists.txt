cmake_minimum_required(VERSION 3.13)
project(LP1)

set(CMAKE_CXX_STANDARD 17)
include_directories(include/)
add_executable(LP1 include/parser.hpp include/utils.hpp include/lpgenerator.hpp include/lpgenerator1.hpp include/lpgenerator2.hpp include/parser1.hpp include/parser2.hpp
        src/main.cpp src/parser1.cpp src/parser2.cpp src/utils.cpp src/lpgenerator1.cpp src/lpgenerator2.cpp)

set(GCC_COVERAGE_COMPILE_FLAGS "-O3 -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )