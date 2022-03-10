cmake_minimum_required(VERSION 3.21)

# Because Qt implementation is a bit buggy,
# when this is fixed, the setup will be even nicer.
set(QT_CREATOR_SKIP_CONAN_SETUP ON)

if(APPLE)
  set(CMAKE_OSX_DEPLOYMENT_TARGET
      10.15
      CACHE STRING "Minimum macOS version the build will be able to run on"
            FORCE)
endif()

project(
  JASP
  VERSION 0.16.2.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      "Debug"
      CACHE STRING
            "OBS build type [Release, RelWithDebInfo, Debug, MinSizeRel]" FORCE)
  message(STATUS "Building for ${CMAKE_BUILD_TYPE}")
endif()

# ------ Build Toolchain

# set(CMAKE_OSX_ARCHITECTURES "arm64")
# if(CMAKE_OSX_ARCHITECTURES STREQUAL "")
#   set(CMAKE_OSX_ARCHITECTURES
#       ${CMAKE_HOST_SYSTEM_PROCESSOR}
#       CACHE STRING "" FORCE)
# endif()
# message(STATUS "Building for ${CMAKE_OSX_ARCHITECTURES} architecture")

# ------ CMake Modules

include(FetchContent)
include(ExternalProject)
include(CMakePrintHelpers)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/Tools/CMake")

# ------ Options

include(Config)

# ------ Conan Dependency Manager

include(Conan)

# ------ JASP Config

include(Programs)

include(Libraries)

include(Dependencies)

include(JASP)

include(R)

# These tell Qt to do its things
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

# Later, I would try tor remove this, for now, it's necessary because Conan
# tends to mess up the directory structure
if(WIN32)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
endif()

add_subdirectory(Common)

if(NOT WIN32)
  add_subdirectory(R-Interface)
else()
  # Building R-Interface with MinGW configuration
  # Note that the reason I'm not directly building into the main BINARY_DIR
  # is because CMake artifacts will conflicts in that case.
  add_custom_target(
    R-Interface
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/R-Interface
    BYPRODUCTS ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll
               ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll.a
    COMMAND
      ${CMAKE_COMMAND} -G "MinGW Makefiles" -S . -B
      ${CMAKE_BINARY_DIR}/R-Interface "-DMINGW_PATH:PATH=${MINGW_PATH}"
      "-DCMAKE_C_COMPILER:STRING=${MINGW_C_COMPILER}"
      "-DCMAKE_CXX_COMPILER:STRING=${MINGW_CXX_COMPILER}"
      "-DCMAKE_MAKE_PROGRAM:STRING=${MINGW_MAKE_PROGRAM}"
      "-DJASP_BINARY_DIR:STRING=${CMAKE_BINARY_DIR}"
      "-DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}"
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR}/R-Interface
    COMMENT "------ Configuring and Building the libR-Interface")

  # add_custom_target(R-Interface DEPENDS
  #   ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll
  #   ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll.a)
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
