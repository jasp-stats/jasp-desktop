list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

set(Boost_USE_STATIC_LIBS ON)
find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

add_library(R-Interface ${HEADER_FILES} ${SOURCE_FILES})

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)

else()

  # find_library(
  #   LIBJSONCPP REQUIRED
  #   NAMES jsoncpp
  #   HINTS ${jsoncpp_LIBRARY_DIRS})

  # WARNING: This is not the way to do it, and it does not work with Ninja :\

  set(LIBJSONCPP_INCLUDE_DIRS ${jsoncpp_INCLUDE_DIRS})
  set(LIBJSONCPP_LIBRARY_DIRS ${jsoncpp_LIBRARY_DIRS})
  set(LIBJSONCPP_LINK_LIBRARIES ${jsoncpp_LIBRARY_DIRS}/libjsoncpp.a)

endif()

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
         ${LIBJSONCPP_LINK_LIBRARIES}
         $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
         $<$<PLATFORM_ID:Linux>:${_LIB_R}>
         ${_LIB_RINSIDE}
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

# Amir: There are some auxillary files here, and I guess I will take care of
# them later

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
