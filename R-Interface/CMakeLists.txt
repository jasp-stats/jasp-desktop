list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

if(WIN32)

  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

  cmake_minimum_required(VERSION 3.21)

  project(
    R-Interface
    VERSION 11.5.0.0
    LANGUAGES C CXX)

  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../Tools/CMake")

  set(MINGW_PATH "C:/msys64/mingw64")
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

  include(FetchContent)
  include(ExternalProject)

  message(CHECK_START "Looking for R.dll")
  set(_LIB_R_PATH ${JASP_BINARY_DIR}/R)
  find_file(
    _LIB_R
    NAMES R.dll
    PATHS ${_LIB_R_PATH}/bin
    PATH_SUFFIXES x64 x32)

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
    PATH_SUFFIXES x64 x32 NO_CACHE)

  if(_LIB_RINSIDE)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_RINSIDE}")
  else()
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/lib")
    message(FATAL_ERROR "libRInside is required for building ")
  endif()

  find_file(
    _LIB_JSONCPP
    NAMES libjsoncpp-24.dll
    PATHS ${MINGW_PATH}/bin REQUIRED)

  file(GLOB HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.h")
  list(APPEND HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.h")
  list(APPEND HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp_interface.h")

  file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
  list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

  add_library(R-Interface SHARED ${HEADER_FILES} ${SOURCE_FILES})

  add_definitions(-DJASP_R_INTERFACE_LIBRARY)

  target_include_directories(
    R-Interface
    PUBLIC # JASP
           jaspResults/src
           ${CMAKE_CURRENT_LIST_DIR}
           ${PROJECT_SOURCE_DIR}/../Common
           ${PROJECT_SOURCE_DIR}/../Common/jaspColumnEncoder
           ${jsoncpp_INCLUDE_DIRS}
           # R
           ${R_INCLUDE_PATH}
           ${RCPP_PATH}/include
           ${RINSIDE_PATH}/include
           # Boost
           ${MINGW_PATH}/include)

  target_link_libraries(
    R-Interface
    PUBLIC # Boost::nowide
           # Boost::filesystem
           # Boost::system
           # Boost::date_time
           # Boost::timer
           # Boost::chrono
           # CPM/CMake
           # ${LIBJSONCPP_LINK_LIBRARIES}
           ${_LIB_JSONCPP}
           # ${jsoncpp_LIBRARIES}
           # jsoncpp::jsoncpp
           # jsoncpp::jsoncpp_lib
           # jsoncpp::jsoncpp_object
           # Conan
           $<$<BOOL:${USE_CONAN}>:CONAN_PKG::jsoncpp>
           $<$<BOOL:${USE_CONAN}>:CONAN_PKG::boost>
           # R
           ${_LIB_RINSIDE}
           $<$<NOT:$<PLATFORM_ID:Darwin>>:${_LIB_R}>
           $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
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

  add_custom_target(
    Transfer-R-Interface-To-JASP ALL
    WORKING_DIRECTORY ${JASP_BINARY_DIR}/R-Interface
    COMMAND ${CMAKE_COMMAND} -E copy libR-Interface.dll ${JASP_BINARY_DIR}/
    COMMAND ${CMAKE_COMMAND} -E copy libR-Interface.dll.a ${JASP_BINARY_DIR}/
    COMMENT "Copying the R-Interface to the jasp-desktop build folder\n          ${JASP_BINARY_DIR}")

  add_dependencies(Transfer-R-Interface-To-JASP R-Interface)

else()

  file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
  list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

  add_library(R-Interface ${HEADER_FILES} ${SOURCE_FILES})

  target_include_directories(
    R-Interface
    PUBLIC # JASP
           jaspResults
           ${PROJECT_SOURCE_DIR}/Common
           ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
           ${LIBJSONCPP_INCLUDE_DIRS}
           # R
           ${R_INCLUDE_PATH}
           ${RCPP_PATH}/include
           ${RINSIDE_PATH}/include)

  target_link_libraries(
    R-Interface
    PUBLIC $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::nowide>
           $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::filesystem>
           $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::system>
           $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::date_time>
           $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::timer>
           $<$<NOT:$<BOOL:${USE_CONAN}>>:Boost::chrono>
           # Conan
           $<$<BOOL:${USE_CONAN}>:CONAN_PKG::boost>
           $<$<BOOL:${USE_CONAN}>:CONAN_PKG::jsoncpp>
           # CPM/CMake
           ${LIBJSONCPP_LINK_LIBRARIES}
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

  if(WIN32)
    target_compile_definitions(
      R-Interface
      PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
             BOOST_USE_WINDOWS_H
             NOMINMAX
             WIN32_LEAN_AND_MEAN)
    # target_compile_options(R-Interface -Og -municode)
  elseif(LINUX)

  endif()

endif()

# Amir: There are some auxillary files here, and I guess I will take care of
# them later

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
