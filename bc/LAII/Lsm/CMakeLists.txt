cmake_minimum_required(VERSION 3.10)
project(Lsm)

set(CMAKE_CXX_STANDARD 17)
add_executable(Lsm src/main.cpp include/Matrix.h include/InputData.h src/InputParser.cpp include/InputParser.h)