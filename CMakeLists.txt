cmake_minimum_required(VERSION 3.21)

set(CMAKE_MESSAGE_CONTEXT_SHOW ON)

project(
  JASP
  VERSION 0.16.1.0 # <major>[.<minor>[.<patch>[.<tweak>]]]
  LANGUAGES CXX C
  HOMEPAGE_URL "http://jasp-stats.org/"
  DESCRIPTION "A fresh way to do statistics")

include(FetchContent)
include(ExternalProject)
include(CMakePrintHelpers)
include(Tools/cmake/CPM.cmake)

include(Checks.cmake)

find_package(Git)
find_program(MAKE NAMES gmake nmake make)
find_program(ACLOCAL NAMES aclocal)
find_program(AUTOCONF NAMES autoconf)
find_program(AUTORECONF NAMES autoreconf)
find_program(AUTOUPDATE NAMES autoupdate)
find_program(CCACHE_EXECUTABLE ccache)
find_program(IWYU_EXECUTABLE include-what-you-use)

# I have a construct for this, and Qt often messes things up.
# I will consider turning this off, and letting Qt does it
# when everything else worked properly
option(USE_CONAN "Whether to use CONAN package manager" OFF)

if(WIN32 OR USE_CONAN)
  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

  if(NOT EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                    COMMAND conan install ${CMAKE_SOURCE_DIR} --build=missing)
  endif()

  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
  conan_basic_setup(TARGETS)
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_FIND_FRAMEWORK ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# TODO: Find this version number automatically
set(R_VERSION "4.1.2")
set(R_VERSION_MAJOR_MINOR "4.1")
set(CURRENT_R_VERSION ${R_VERSION_MAJOR_MINOR})

# - [ ] This needs to be removed. We don't necessary need it, and
#       for now, it is here for testing
# set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/Install)
# message(STATUS ${CMAKE_INSTALL_PREFIX})

if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}-arm64")
else()
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}")
endif()

if(WIN32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64 bits
    set(R_DIR_NAME "x64")
  elseif(CMAKE_SIZEOF_VOID_P EQUAL 4) # 32 bits
    set(R_DIR_NAME "i386")
  endif()
endif()

# if(UNIX)

#   file(
#     CHMOD
#     ${CMAKE_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
#     PERMISSIONS
#     OWNER_EXECUTE)

# endif()

# This looks weird but CMake doesn't like ON/OFF in the if condition,
# especially if it's passed
option(SIGN_AT_BUILD_TIME
       "Whether to sign every library during the configuration and build" ON)
if(${SIGN_AT_BUILD_TIME})
  message(STATUS "Signing everything during the configuration and building.")
  set(IS_SIGNING 1)
else()
  message(STATUS "Only signing essential libraries and binaries.")
  set(IS_SIGNING 0)
endif()

# Sets the build options
include(JASP.cmake)

# Prepares and configures everything related to R, and R.framework
include(R.cmake)

# Builds and configures all the dependencies
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  include(Dependencies.cmake)
endif()

if(WIN32)
  if(QTDIR
     OR DEFINED ENV{QTDIR}
     OR DEFINED ENV{QTDIR32}
     OR DEFINED ENV{QTDIR64})
    # Qt path set by user or env var
  else()
    set(QTDIR
        ""
        CACHE PATH "Path to Qt (e.g. C:/Qt/5.7/msvc2015_64)")
    message(
      WARNING
        "QTDIR variable is missing.  Please set this variable to specify path to Qt (e.g. C:/Qt/5.7/msvc2015_64)"
    )
  endif()
endif()

# This loads all the Qt-related CMake Modules.
find_package(Qt6 REQUIRED COMPONENTS Core)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

# When everything is done, we should be able to just activate
# this and build it for two architectures. This will also download
# the correct R.framework
# set(CMAKE_OSX_ARCHITECTURES "x86_64" "arm64")

# With this, we can hit up to 90% speed up!
option(USE_CCACHE "Whether to use ccache for build" OFF)
option(RUN_IWYU "Whether to run Include What You Use" OFF)
option(INSTALL_R_FRAMEWORK "Whether to download and prepare R.framework" ON)
option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)
option(GIT_SUBMODULE "Check submodules during build" ON)
option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  OFF)
option(BUILD_TESTS "Whether to build the test suits" OFF)
option(LINUX_LOCAL_BUILD "Whether we are building inside the Build folder" OFF)
option(FLATPAK_USED "Whether we are building for Flatpak" OFF)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  set(FLATPAK_USED ON)
endif()

if(LINUX_LOCAL_BUILD)
  set(FLATPAK_USED OFF)
  message(STATUS "JASP will be configured for local testing")
  message(
    WARNING
      "In this mode, JASP configures a local R/library; however this cannot be used for installing JASP. If you wish to install JASP (e.g., on Flatpak), you must diabled this flag."
  )
endif()

if(INSTALL_R_MODULES AND (GITHUB_PAT STREQUAL ""))
  message(
    WARNING
      "You probably need to set the GITHUB_PAT; otherwise CMAKE cannot effectively communicate with GitHub."
  )
endif()

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

if(NOT (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux"))
  set(IS_LINUX_LOCAL_BUILD FALSE)
endif()

# I am not using this for now, but later on, we can use it to
# create binaries and installer for different systems
if(WIN32) # Check if we are on Windows
  set(SYSTEM_TYPE WIN32)
  # else(APPLE)
  #   set(SYSTEM_TYPE MACOSX_BUNDLE)
endif()

# endif()

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
include(Modules.cmake)

# Installs the JASP and JASPEngine
include(Install.cmake)

# Packs and creates installer executable for Windows, and macOS
include(Pack.cmake)
