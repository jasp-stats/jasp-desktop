cmake_minimum_required(VERSION 3.21)

set(QT_CREATOR_SKIP_CONAN_SETUP ON)

project(
  JASP
  VERSION 0.16.1.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/Tools/CMake")

# ------ Build Toolchain

# set(CMAKE_OSX_ARCHITECTURES "x86_64" "arm64")

# ------ CMake Modules

include(FetchContent)
include(ExternalProject)
include(CMakePrintHelpers)

# ------ Conan Dependency Manager

include(Conan)

# ------ JASP Config

include(CPM)

include(Programs)

# Builds and configures all the dependencies
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  include(Dependencies)
endif()

include(Config)

include(Libraries)

include(JASP)

include(R)

# These tell Qt to do its things
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

if(WIN32)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
endif()

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
