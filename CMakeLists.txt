cmake_minimum_required(VERSION 3.21)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(
  JASP
  VERSION 0.16.0.0
  LANGUAGES CXX C)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

include(CMakePrintHelpers)

option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  OFF)

option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)

set(R_VERSION "4.1.2")
set(R_VERSION_MAJOR_MINOR "4.1")

# If set, CMake tries to download all the necessary dependencies
#
#   - R.exe or R.framework depending on the system
#   - Boost, jsoncpp, libarchive, zlib, readstat, and jags
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  include(Dependencies.cmake)
endif()

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

# This loads all the Qt-related CMake Modules.
find_package(Qt6 REQUIRED COMPONENTS Core)

# I am not using this for now, but later on, we can use it to
# create binaries and installer for different systems
if(WIN32) # Check if we are on Windows
  set(SYSTEM_TYPE WIN32)
endif()
message(STATUS "System type is " ${SYSTEM_TYPE})

# JASP Options
include(JASP.cmake)

# R/Environment Configurations
#
#   - Assuming that R.framework exists, this set the all the R-related paths
#   - Install RInside, and Rcpp, and prepare them to be linked to the R-Interface
include(R.cmake)

add_subdirectory(Common)
add_subdirectory(R-Interface)
add_subdirectory(Engine)
add_subdirectory(Desktop)

if(INSTALL_R_MODULES)
  add_subdirectory(Modules)
endif()
