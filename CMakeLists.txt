cmake_minimum_required(VERSION 3.21)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(
  JASP
  VERSION 0.16.0.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

# TODO: Find this version number automatically
set(R_VERSION "4.1.2")
set(R_VERSION_MAJOR_MINOR "4.1")
set(CURRENT_R_VERSION ${R_VERSION_MAJOR_MINOR})

# This loads all the Qt-related CMake Modules.
find_package(Qt6 REQUIRED COMPONENTS Core)

include(FetchContent)
include(ExternalProject)
include(CMakePrintHelpers)
include(Tools/cmake/CPM.cmake)

find_package(Git)
find_program(MAKE NAMES gmake nmake make)
find_program(ACLOCAL NAMES aclocal)
find_program(AUTOCONF NAMES autoconf)
find_program(AUTORECONF NAMES autoreconf)
find_program(CCACHE_EXECUTABLE ccache)
find_program(IWYU_EXECUTABLE include-what-you-use)

# When everything is done, we should be able to just activate
# this and build it for two architectures. This will also download
# the correct R.framework
# set(CMAKE_OSX_ARCHITECTURES "x86_64" "arm64")

# With this, we can hit up to 90% speed up!
option(USE_CCACHE "Whether to use ccache for build" OFF)
option(RUN_IWYU "Whether to run Include What You Use" OFF)
option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)
option(GIT_SUBMODULE "Check submodules during build" ON)
option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  OFF)

if(CCACHE_EXECUTABLE
   AND USE_CCACHE
   AND (NOT
        CMAKE_GENERATOR
        STREQUAL
        "Xcode"
       ))
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_EXECUTABLE}")
  message(STATUS "Found ccache: ${CCACHE_EXECUTABLE}")
endif()

if(IWYU_EXECUTABLE)
  message(STATUS "Found iwyu: ${IWYU_EXECUTABLE}")
endif()

# Dealing with Git submodules
if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
  # Update submodules as needed
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

# I am not using this for now, but later on, we can use it to
# create binaries and installer for different systems
if(WIN32) # Check if we are on Windows
  set(SYSTEM_TYPE WIN32)
endif()

# Sets the build options
include(JASP.cmake)

# Prepares and configures everything related to R, and R.framework
include(R.cmake)

# Builds and configures all the dependencies
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  include(Dependencies.cmake)
endif()

add_subdirectory(Common)
add_subdirectory(R-Interface)
add_subdirectory(Engine)
add_subdirectory(Desktop)

# Builds, installs and configures JASP Modules
include(Modules.cmake)

# Installs the JASP and JASPEngine
include(Install.cmake)

# Packs and creates installer executable for Windows, and macOS
include(Pack.cmake)
