cmake_minimum_required(VERSION 3.21)

# Because Qt implementation is a bit buggy,
# when this is fixed, the setup will be even nicer.
set(QT_CREATOR_SKIP_CONAN_SETUP ON)

# ------ Build Toolchain

if(APPLE)

  if(CMAKE_OSX_ARCHITECTURES STREQUAL "")
    set(CMAKE_OSX_ARCHITECTURES
        ${CMAKE_HOST_SYSTEM_PROCESSOR}
        CACHE STRING "" FORCE)
    set(CROSS_COMPILING 0)
    message(STATUS "Building for ${CMAKE_OSX_ARCHITECTURES} architecture")
  else()
    if(NOT (CMAKE_OSX_ARCHITECTURES STREQUAL CMAKE_HOST_SYSTEM_PROCESSOR))
      set(CROSS_COMPILING 1)
      message(
        STATUS "Cross compiling for ${CMAKE_OSX_ARCHITECTURES} architecture")
    endif()
  endif()

  if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
    set(CMAKE_OSX_DEPLOYMENT_TARGET
        11.0
        CACHE STRING "Minimum macOS version the build will be able to run on"
              FORCE)
    set(CONAN_ARCH "armv8")
  else()
    set(CMAKE_OSX_DEPLOYMENT_TARGET
        10.15
        CACHE STRING "Minimum macOS version the build will be able to run on"
              FORCE)
    set(CONAN_ARCH "x86_64")
  endif()

endif()

project(
  JASP
  VERSION 0.17.0.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

#needed for R-Interface and QMLComponents for them to build stuff porperly when *inside* JASP
add_definitions(-DBUILDING_JASP)
set(BUILDING_JASP TRUE)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

set(CMAKE_CXX_STANDARD 20)
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

# Looking for programs, e.g., git, bison, flex
include(Programs)

# Looking for libraries, e.g., Boost, Qt
include(Libraries)

# Preparing irregular dependencies, e.g., ReadStat
include(Dependencies)

# Setting JASP's parameters, e.g., JASP_VERSION
include(JASP)

# Preparing R environment, and setting R_HOME_PATH
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
add_subdirectory(CommonData)
add_subdirectory(QMLComponents)

if(NOT WIN32)
  add_subdirectory(R-Interface)
else()
  # Building R-Interface with MinGW configuration, see R-Interface/CMakeLists.txt
  # for more info. Note that the build path is set to be inside the JASP's build
  # folder, e.g. jasp-desktop-build-dir/R-Interface
  add_custom_target(
    R-Interface
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/R-Interface
    BYPRODUCTS ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll
               ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll.a
    COMMAND
      ${CMAKE_COMMAND} -G "MinGW Makefiles" -S . -B
      ${CMAKE_BINARY_DIR}/R-Interface "-DRTOOLS_PATH:PATH=${RTOOLS_PATH}"
      "-DCMAKE_C_COMPILER:PATH=${RTOOLS_C_COMPILER}"
      "-DCMAKE_CXX_COMPILER:PATH=${RTOOLS_CXX_COMPILER}"
      "-DCMAKE_MAKE_PROGRAM:PATH=${RTOOLS_MAKE_PROGRAM}"
      "-DJASP_BINARY_DIR:PATH=${CMAKE_BINARY_DIR}"
      "-DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}"
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR}/R-Interface
    USES_TERMINAL
    COMMENT "------ Configuring and Building the libR-Interface")

endif()

add_subdirectory(Engine)
add_subdirectory(Desktop)

if(BUILD_TESTS)
  enable_testing()
  add_subdirectory(Tests)
endif()

# Builds, installs and configures JASP Modules
include(Modules)

# Install logic, usually staging a deploy-able JASP in Build/Install/ folder
include(Install)

# Packs and creates installer executable for Windows, and macOS
if(NOT LINUX)
	include(Pack)
endif()

if(NOT
   CMAKE_GENERATOR
   STREQUAL
   "Ninja")
  message(
    WARNING
      "JASP is optimised to be built using the Ninja build system. You have chosen ${CMAKE_GENERATOR}, and this might cause issue during the build."
  )
endif()
