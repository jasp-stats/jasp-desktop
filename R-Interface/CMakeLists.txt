cmake_minimum_required(VERSION 3.21)

message(STATUS "[JASP]: Configuring the R-Interface...")

find_package(Boost COMPONENTS nowide filesystem system date_time timer chrono
             REQUIRED)

pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)

if(BUILD_WITH_SYSTEM_R)
  message(STATUS "[R-Interface]: Building with system R...")
  pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

  include_directories(${LIBR_INCLUDE_DIRS})
  include_directories(${_Rcpp_HOME}/include)
  include_directories(${_RInside_HOME}/include)

else()
  message(STATUS "[R-Interface]: Building with R.framework...")
  message(STATUS "[R-Interface]: Setting the headers...")

  include_directories(${_R_HOME}/include)
  include_directories(${_Rcpp_HOME}/include)
  include_directories(${_RInside_HOME}/include)

  message(
    STATUS "[R-Interface]: Rcpp_INCLUDE_DIRS is set to ${_Rcpp_HOME}/include")
  message(
    STATUS
      "[R-Interface]: RInside_INCLUDE_DIRS is set to ${_RInside_HOME}/include")
endif()

include_directories(jaspResults)
include_directories(${PROJECT_SOURCE_DIR}/Common)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})

add_definitions(-DJASP_R_INTERFACE_LIBRARY)
add_definitions(-DQT_NO_DEPRECATED_WARNINGS)

file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

add_library(R-Interface ${HEADER_FILES} ${SOURCE_FILES})

# Making sure that Xcode could find libR-Interface.a during the archiving
# process
if(CMAKE_GENERATOR STREQUAL "Xcode")
  set_target_properties(R-Interface PROPERTIES ARCHIVE_OUTPUT_DIRECTORY
                                               ${PROJECT_BINARY_DIR}/)
endif()

add_dependencies(R-Interface Common)

target_link_libraries(R-Interface PUBLIC Common ${_R_Framework} ${_LIB_RInside})

if(WIN32)
  target_compile_definitions(
    R-Interface PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
                       BOOST_USE_WINDOWS_H NOMINMAX WIN32_LEAN_AND_MEAN)
  target_compile_options(R-Interface -Og -municode)
endif()

# Amir: There are some auxillary files here, and I guess I will take care of
# them later
