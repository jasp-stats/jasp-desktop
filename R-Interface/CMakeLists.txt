cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

find_package(
  Boost
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono
  REQUIRED)

file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

add_library(R-Interface ${HEADER_FILES} ${SOURCE_FILES})

message(STATUS "Building with R.framework...")
message(STATUS "Setting the headers...")

target_include_directories(
  R-Interface PUBLIC ${_R_HOME}/include ${_Rcpp_HOME}/include
                     ${_RInside_HOME}/include)

message(STATUS "Rcpp_INCLUDE_DIRS is set to ${_Rcpp_HOME}/include")
message(STATUS "RInside_INCLUDE_DIRS is set to ${_RInside_HOME}/include")

target_include_directories(
  R-Interface
  PUBLIC jaspResults
         ${PROJECT_SOURCE_DIR}/Common
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
         ${Boost_INCLUDE_DIRS})

add_dependencies(R-Interface Common)

target_link_libraries(R-Interface PUBLIC Common ${_R_Framework} ${_LIB_RInside})

target_compile_definitions(R-Interface PUBLIC JASP_R_INTERFACE_LIBRARY
                                              QT_NO_DEPRECATED_WARNINGS)

if(WIN32)
  target_compile_definitions(
    R-Interface
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)
  target_compile_options(R-Interface -Og -municode)
endif()

# Amir: There are some auxillary files here, and I guess I will take care of
# them later

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
