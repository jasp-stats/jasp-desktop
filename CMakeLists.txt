cmake_minimum_required(VERSION 3.21)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(JASP VERSION 0.16.0 LANGUAGES CXX C)
# TODO: Later on, we can use CMake to even have proper TWEAK versioning as well
# set(JASP_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
# set(JASP_VERSION_MINOR ${PROJECT_VERSION_MINOR})
# set(JASP_VERSION_PATCH ${PROJECT_VERSION_PATCH})
# set(JASP_VERSION_TWEAK ${PROJECT_VERSION_TWEAK})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

include(CMakePrintHelpers)

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
include(R.cmake)

add_subdirectory(Common)
add_subdirectory(R-Interface)
add_subdirectory(Engine)
add_subdirectory(Desktop)

if(INSTALL_R_MODULES)
  include(Modules.cmake)
endif()
