# This script in fact consists of two scripts. The first part is used for building
# R-Interface using the MSYS2 environment on Windows and the second part is a rather normal
# script that is being included by the main `CMakeLists.txt` and will build the
# R-Interface on macOS and Linux.
#
# On Windows,
#   - This will be invoked through a direct call of the CMAKE_COMMAND that is set to
#     generate a "MinGW Makefiles". In addition to the generator, more parameters will
#     be passed to this file, e.g., CMAKE_C_COMPILER. These parameters are set or deducted
#     in the Config module. JASP's CMake triggers the R-Interface build during every build
#     however, it just asks this file whether things need to be recompiled or not, if not
#     then, it won't recompile things.
#   - The output library, R-Interface.dll, and some of its artifact will be transferred
#     to the Build/ directory and linked dynamically to the JASPEngine, etc.
#
# On Linux/macOS,
#   Here we are building a static library, and it will be link statically to the rest
#   of the libraries and executable.
#
# -----------------------------------------------------------------------------------
#
# Notes
#   - There are overlap between these two scripts, however I have not got to fully
#     simplify it, that's maybe for later...
#
#
# Todos:
#   - [ ] On Windows, Qt Creator and CMake can still get confused, and
#     as a result R-Interface's build might fail with this, 
#     `The C compiler "C:/rtools42/ucrt64/bin/qcc.exe"is not able to compile a simple test program`.
#     This happens because CMake cannot navigate its way to the compiler toolset, 
#     and the solution for it is to set the PATH correctly. However, that's not
#     great, and the ultimate solution for it is to use a CMake Toolchain file, 
#     such that CMake knows that R-Interface is going to be build with set of 
#     different tools. I've tried to get the Toolchain working, but it was very
#     stubborn, if you feel working on it, you can read more about it here,
#     https://cmake.org/cmake/help/latest/manual/cmake-toolchains.7.html.

list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

file(GLOB HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.h")
list(APPEND HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.h")
list(APPEND HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp_interface.h")

if(WIN32)

  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

  cmake_minimum_required(VERSION 3.21)

  project(
    R-Interface
    VERSION 11.5.0.0
    LANGUAGES C CXX)

  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../Tools/CMake")

  # This is a variable and it's provided by the main CMakeFiles.txt.
  # It is here only if we someone wants to build the R-Interface separately.
  set(RTOOLS_PATH
      "C:/rtools42/ucrt64"
      CACHE PATH "Path to Rtools42 ucrt64 folder")
  set(JASP_SOURCE_DIR "${CMAKE_SOURCE_DIR}/..")
  if(NOT EXISTS ${JASP_BINARY_DIR})
    message(
      FATAL_ERROR
        "Please set the correct Path to the JASP build folder, e.g., 'C:/path/to/build-jasp-desktop-Desktop_Qt_6_3_0_MSVC2019_64bit-Debug'"
    )
  endif()

  set(R_HOME_PATH "${JASP_BINARY_DIR}/R")
  set(R_BIN_PATH "${R_HOME_PATH}/bin")
  set(R_LIB_PATH "${R_HOME_PATH}/bin/${R_DIR_NAME}")
  set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
  set(R_OPT_PATH "${R_HOME_PATH}/opt")
  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH "${R_HOME_PATH}/include")

  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")
  set(RENV_PATH "${R_LIBRARY_PATH}/renv")

  include(FetchContent)
  include(ExternalProject)

  message(CHECK_START "Looking for R.dll")
  set(_LIB_R_PATH ${JASP_BINARY_DIR}/R)
  find_file(
    _LIB_R
    NAMES R.dll
    PATHS ${_LIB_R_PATH}/bin
    PATH_SUFFIXES x64 x32
    NO_DEFAULT_PATH)

  if(_LIB_R)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${_LIB_R_PATH}/bin/x64")
    message(FATAL_ERROR "R.dll is required for building R-Interface.")
  endif()

  message(CHECK_START "Checking for 'libRInside'")
  find_library(
    _LIB_RINSIDE
    NAMES RInside
    PATHS ${RINSIDE_PATH}/lib
    PATH_SUFFIXES x64 x32 NO_CACHE
    NO_DEFAULT_PATH)

  if(_LIB_RINSIDE)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_RINSIDE}")
  else()
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/lib")
    message(FATAL_ERROR "libRInside is required for building ")
  endif()

  message(CHECK_START "Checking for 'libjsoncpp-24.dll'")
  find_file(
    RTOOLS_LIB_JSONCPP_DLL
    NAMES libjsoncpp-24.dll
    PATHS ${RTOOLS_PATH}/bin REQUIRED
    NO_DEFAULT_PATH)

  if(RTOOLS_LIB_JSONCPP_DLL)
    message(CHECK_PASS "found.")
    message(STATUS "  ${RTOOLS_LIB_JSONCPP_DLL}")
  else()
    message(CHECK_FAIL "not found in ${RTOOLS_PATH}/bin")
    message(FATAL_ERROR "libjsoncpp-24.dll is required for building. If it is missing, you need to install it in your rtools42 ucrt64 environment using `pacman -Syu mingw-w64-ucrt-x86_64-jsoncpp`.")
  endif()

  add_library(R-Interface SHARED ${SOURCE_FILES} ${HEADER_FILES})

  add_definitions(-DJASP_R_INTERFACE_LIBRARY)

  target_include_directories(
    R-Interface
    PUBLIC # JASP
           ${CMAKE_CURRENT_LIST_DIR}
           ${CMAKE_CURRENT_LIST_DIR}/jaspResults/src
           ${PROJECT_SOURCE_DIR}/../Common
           ${PROJECT_SOURCE_DIR}/../Common/jaspColumnEncoder
           # R
           ${R_INCLUDE_PATH}
           ${RCPP_PATH}/include
           ${RINSIDE_PATH}/include
           # Boost and jsoncpp
           ${RTOOLS_PATH}/include)

  target_link_libraries(
    R-Interface
    PUBLIC # RInside
           ${_LIB_RINSIDE}
           # R
           ${_LIB_R}
           #
           ${RTOOLS_LIB_JSONCPP_DLL}
           #
           $<$<BOOL:${JASP_USES_QT_HERE}>:Qt::Core>)

  target_compile_definitions(
    R-Interface
    PUBLIC JASP_R_INTERFACE_LIBRARY
           QT_NO_DEPRECATED_WARNINGS
           $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
           $<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>)

  target_compile_definitions(
    R-Interface
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)

else()
  # Linux and macOS


  add_library(R-Interface ${SOURCE_FILES} ${HEADER_FILES})

  target_include_directories(
    R-Interface
    PUBLIC # JASP
           jaspResults
           ${PROJECT_SOURCE_DIR}/Common
           ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
           $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_INCLUDEDIR}>
           # R
           ${R_INCLUDE_PATH}
           ${RCPP_PATH}/include
           ${RINSIDE_PATH}/include
           #
           ${Boost_INCLUDE_DIRS})

  target_link_libraries(
    R-Interface
    PUBLIC Boost::filesystem
           Boost::system
           Boost::date_time
           Boost::timer
           Boost::chrono
           # CPM/CMake
           $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LIBRARIES}>
           $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LINK_LIBRARIES}>
           $<$<BOOL:${USE_CONAN}>:jsoncpp::jsoncpp>
           # R
           ${_LIB_RINSIDE}
           $<$<PLATFORM_ID:Linux>:${_LIB_R}>
           $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
           #
           $<$<BOOL:${JASP_USES_QT_HERE}>:Qt::Core>)

  target_compile_definitions(
    R-Interface
    PUBLIC JASP_R_INTERFACE_LIBRARY
           QT_NO_DEPRECATED_WARNINGS
           $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
           $<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>)

  if(IWYU_EXECUTABLE AND RUN_IWYU)
    set_target_properties(R-Interface PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                                 ${IWYU_EXECUTABLE})
  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
