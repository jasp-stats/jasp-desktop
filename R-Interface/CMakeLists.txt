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

file(GLOB SOURCE_FILES    "${PROJECT_SOURCE_DIR}/Common/json/*.cpp")
list(APPEND SOURCE_FILES  "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")
list(APPEND HEADER_FILES  "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.h")
list(APPEND HEADER_FILES  "${CMAKE_CURRENT_LIST_DIR}/jasprcpp_interface.h")

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
if(${R_VERSION} VERSION_GREATER_EQUAL "4.5.0")
    set(RTOOLS_PATH
      "C:/rtools45/ucrt64"
       CACHE PATH "Path to Rtools45 ucrt64 folder")
  elseif(${R_VERSION} VERSION_GREATER_EQUAL "4.4.0")
    set(RTOOLS_PATH
      "C:/rtools44/ucrt64"
       CACHE PATH "Path to Rtools44 ucrt64 folder")
  elseif(${R_VERSION} VERSION_GREATER_EQUAL "4.3.0")
    set(RTOOLS_PATH
       "C:/rtools43/ucrt64"
       CACHE PATH "Path to Rtools43 ucrt64 folder")
  elseif(${R_VERSION} VERSION_GREATER_EQUAL "4.2.0")
    set(RTOOLS_PATH
      "C:/rtools42/ucrt64"
      CACHE PATH "Path to Rtools42 ucrt64 folder")
  else()
    message(
        FATAL_ERROR
        "Please use a supported R-version. If you're building JASP with a version of R older than 4.2.0 then adjust the RTOOLS_PATH in the CMake code above this error.")
  endif()
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

  set(RENV_PATH      "${JASP_BINARY_DIR}/_cache/R/renv_library/renv")

  set(R_CPP_INCLUDES_LIBRARY      "${JASP_BINARY_DIR}/Modules/Tools/R_cpp_includes_library")
  include(FindRPackagePath)

  find_package_path(RCPP_PATH       ${R_CPP_INCLUDES_LIBRARY} "Rcpp")
  find_package_path(RINSIDE_PATH    ${R_CPP_INCLUDES_LIBRARY} "RInside")

  message(STATUS "RCPP_PATH    = ${RCPP_PATH}")
  message(STATUS "RINSIDE_PATH = ${RINSIDE_PATH}")
  message(STATUS "RENV_PATH    = ${RENV_PATH}")

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

  add_library(R-Interface			SHARED ${SOURCE_FILES} ${HEADER_FILES})
  add_library(R-InterfaceNoRInside	SHARED ${SOURCE_FILES} ${HEADER_FILES})

  set(R_INTERFACE_INCLUDE_DIRS
		${CMAKE_CURRENT_LIST_DIR}
        ${PROJECT_SOURCE_DIR}/../Common
		${R_INCLUDE_PATH}
		${RCPP_PATH}/include
  )

  set(R_INTERFACE_LIBS ${_LIB_R})

  add_definitions(-DJASP_R_INTERFACE_LIBRARY)

  target_include_directories(R-Interface			PUBLIC ${R_INTERFACE_INCLUDE_DIRS} ${RINSIDE_PATH}/include)
  target_include_directories(R-InterfaceNoRInside	PUBLIC ${R_INTERFACE_INCLUDE_DIRS})

  target_link_libraries(R-Interface					PUBLIC ${R_INTERFACE_LIBS} ${_LIB_RINSIDE})
  target_link_libraries(R-InterfaceNoRInside		PUBLIC ${R_INTERFACE_LIBS})


  target_compile_definitions(R-Interface			PUBLIC JASP_R_INTERFACE_LIBRARY NOMINMAX RCPP_NO_SUGAR WIN32_LEAN_AND_MEAN
															$<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>)
  target_compile_definitions(R-InterfaceNoRInside	PUBLIC JASP_R_INTERFACE_LIBRARY NOMINMAX RCPP_NO_SUGAR WIN32_LEAN_AND_MEAN
															$<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>
															JASP_NO_RINSIDE)

else()
  # Linux and macOS


  add_library(R-Interface ${SOURCE_FILES} ${HEADER_FILES})
  add_library(R-InterfaceNoRInside ${SOURCE_FILES} ${HEADER_FILES})

  set(R_INTERFACE_INCLUDE_DIRS
        ${PROJECT_SOURCE_DIR}/Common
		${R_INCLUDE_PATH}
		${RCPP_PATH}/include
	)

  set(R_INTERFACE_LIBS
		$<$<PLATFORM_ID:Linux>:${_LIB_R}>
		$<$<PLATFORM_ID:Darwin>:${_R_Framework}>
	)


  target_include_directories(R-Interface			PUBLIC ${R_INTERFACE_INCLUDE_DIRS} ${RINSIDE_PATH}/include)
  target_include_directories(R-InterfaceNoRInside	PUBLIC ${R_INTERFACE_INCLUDE_DIRS})

  target_link_libraries(R-Interface					PUBLIC ${R_INTERFACE_LIBS} ${_LIB_RINSIDE})
  target_link_libraries(R-InterfaceNoRInside		PUBLIC ${R_INTERFACE_LIBS})

  target_compile_definitions(R-Interface			PUBLIC	RCPP_NO_SUGAR ENABLE_LEGACY_NONAPI
															$<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>)
  target_compile_definitions(R-InterfaceNoRInside	PUBLIC	RCPP_NO_SUGAR ENABLE_LEGACY_NONAPI
															  $<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>
															  JASP_NO_RINSIDE)

  if(IWYU_EXECUTABLE AND RUN_IWYU)
	set_target_properties(R-Interface			PROPERTIES CXX_INCLUDE_WHAT_YOU_USE ${IWYU_EXECUTABLE})
	set_target_properties(R-InterfaceNoRInside	PROPERTIES CXX_INCLUDE_WHAT_YOU_USE ${IWYU_EXECUTABLE})
  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
