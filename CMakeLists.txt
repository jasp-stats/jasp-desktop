cmake_minimum_required(VERSION 3.22)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(
  JASP
  VERSION 0.16.0.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# When everything is done, we should be able to just activate
# this and build it for two architectures. This will also download
# the correct R.framework
# set(CMAKE_OSX_ARCHITECTURES "x86_64" "arm64")

set(CMAKE_FIND_FRAMEWORK ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

# With this, we can hit up to 90% speed up!
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM
   AND (NOT
        CMAKE_GENERATOR
        STREQUAL
        "Xcode"
       ))
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
  message(STATUS "Found ccache: ${CCACHE_PROGRAM}")
endif()

option(RUN_IWYU "Whether to run Include What You Use" OFF)
find_program(IWYU include-what-you-use)
if(IWYU)
  message(STATUS "Found iwyu: ${IWYU}")
endif()

include(CMakePrintHelpers)

option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  OFF)

option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)

# This could run/update the git-submodule for us to at least get those
# essentials
find_package(Git)

if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
  # Update submodules as needed
  option(GIT_SUBMODULE "Check submodules during build" ON)
  if(GIT_SUBMODULE)
    message(STATUS "Submodule update")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      RESULT_VARIABLE GIT_SUBMOD_RESULT)
    if(NOT
       GIT_SUBMOD_RESULT
       EQUAL
       "0")
      message(
        FATAL_ERROR
          "git submodule update --init --recursive failed with ${GIT_SUBMOD_RESULT}, please checkout submodules"
      )
    endif()
  endif()
endif()

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

# JASP Options
include(JASP.cmake)

# R/Environment Configurations
#
#   - Assuming that R.framework exists, this set the all the R-related paths
#   - Install RInside, and Rcpp, and prepare them to be linked to the R-Interface
include(R.cmake)

# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
# set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(Common)
add_subdirectory(R-Interface)
add_subdirectory(Engine)
add_subdirectory(Desktop)

# If INSTALL_R_MODULES is OFF, this will only look for already installed
# Modules/ and set their path to be used by the rest of the script.
include(Modules.cmake)

include(Install.cmake)

include(Pack.cmake)
