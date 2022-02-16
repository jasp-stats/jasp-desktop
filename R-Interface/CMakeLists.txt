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


if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    _LIB_R_PKG_CONFIG
    REQUIRED
    IMPORTED_TARGET
    libR)

  # I think we need to deduct most information about R using the
  # PkgConfig but for now, I'm doing it as minimal as possible 
  # to keep it consistent to other platforms
  target_include_directories(R-Interface
    PUBLIC ${_LIB_R_PKG_CONFIG_INCLUDE_DIRS})

endif()

target_include_directories(
  R-Interface
  PUBLIC # JASP
         jaspResults
         ${PROJECT_SOURCE_DIR}/Common
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
         ${LIBJSONCPP_INCLUDE_DIRS}
         # R
         ${R_HOME_PATH}/include
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
