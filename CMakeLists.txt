cmake_minimum_required(VERSION 3.21)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(
  JASP
  VERSION 0.16.1.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/Tools/CMake")

# - [ ] This needs to be removed. We don't necessary need it, and
#       for now, it is here for testing
# set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/Install)
# message(STATUS ${CMAKE_INSTALL_PREFIX})

# When everything is done, we should be able to just activate
# this and build it for two architectures. This will also download
# the correct R.framework
# set(CMAKE_OSX_ARCHITECTURES "x86_64" "arm64")

# ------ CMake Modules

include(FetchContent)
include(ExternalProject)
include(CMakePrintHelpers)

# ------ Conan Dependency Manager

if(WIN32 OR USE_CONAN)
  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

  if(NOT EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                    COMMAND conan install ${CMAKE_SOURCE_DIR} --build=missing)
  endif()

  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
  conan_basic_setup(TARGETS)
endif()

# ------ JASP Config

include(CPM)

include(Checks)

# Builds and configures all the dependencies
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  include(Dependencies)
endif()

include(Config)

include(Libraries)

include(JASP)

include(R)

# This loads all the Qt-related CMake Modules.
find_package(Qt6 REQUIRED COMPONENTS Core)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

add_subdirectory(Common)

if(NOT WIN32)
  add_subdirectory(R-Interface)
endif()

add_subdirectory(Engine)
add_subdirectory(Desktop)

if(BUILD_TESTS)
  enable_testing()
  add_subdirectory(Tests)
endif()

# Builds, installs and configures JASP Modules
include(Modules)

# Installs the JASP and JASPEngine
include(Install)

# Packs and creates installer executable for Windows, and macOS
include(Pack)
