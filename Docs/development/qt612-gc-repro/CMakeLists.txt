cmake_minimum_required(VERSION 3.16)
project(qt612-identifierhash-gc-repro LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Qml)

add_executable(repro main.cpp)
target_link_libraries(repro PRIVATE Qt6::Qml)
