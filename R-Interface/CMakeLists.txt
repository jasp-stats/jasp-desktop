list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

if (WIN32)

  cmake_minimum_required(VERSION 3.21)

  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../Tools/CMake")

  project(R-Interface
    VERSION 11.5.0.0
    LANGUAGES CXX C)

  set(JASP_SOURCE_DIR "${CMAKE_SOURCE_DIR}/..")
  set(JASP_BINARY_DIR "${CMAKE_SOURCE_DIR}/../build")

  set(R_HOME_PATH "${JASP_BINARY_DIR}/R/")
  set(R_BIN_PATH "${R_HOME_PATH}/bin")
  set(R_LIB_PATH "${R_HOME_PATH}/bin/${R_DIR_NAME}")
  set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
  set(R_OPT_PATH "${R_HOME_PATH}/opt")
  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH "${R_HOME_PATH}/include")

  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")

  include(Conan)

  include(Programs)

  include(Libraries)

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
    PUBLIC Boost::nowide
           Boost::filesystem
           Boost::system
           Boost::date_time
           Boost::timer
           Boost::chrono
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
  elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  endif()

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
  elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  endif()

endif()

# Amir: There are some auxillary files here, and I guess I will take care of
# them later

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
