cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Common)

find_package(PkgConfig REQUIRED)
find_package(LibArchive REQUIRED)
# pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(
  LIBJSON
  REQUIRED
  IMPORTED_TARGET
  jsoncpp)

find_package(
  Boost
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono
  REQUIRED)

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp)

add_library(Common ${HEADER_FILES} ${SOURCE_FILES})

target_include_directories(
  Common
  PUBLIC ${Boost_INCLUDE_DIRS}
         ${LIBJSON_INCLUDE_DIRS}
         ${LibArchive_INCLUDE_DIRS}
         # ${LIBARCHIVE_INCLUDE_DIRS}
         jaspColumnEncoder)

if(BUILD_WITH_SYSTEM_R)
  message(STATUS "Building with system R..")
  pkg_check_modules(
    LIBR
    REQUIRED
    IMPORTED_TARGET
    libR)

  target_include_directories(
    Common PUBLIC ${LIBR_INCLUDE_DIRS} ${_Rcpp_HOME}/include
                  ${_RInside_HOME}/include)

else()

  target_include_directories(Common PUBLIC ${_R_HOME}/include
                                           ${_R_HOME}/library/Rcpp/include)
endif()

find_package(Qt6 REQUIRED COMPONENTS Core)

target_link_libraries(
  Common
  PUBLIC ${Boost_LIBRARIES}
         ${LibArchive_LIBRARIES}
         PkgConfig::LIBJSON
         Qt::Core)

if(USE_JASP_TIMER)
  target_compile_definitions(Common PUBLIC PROFILE_JASP)
endif()

if(APPLE)
  target_compile_definitions(Common PUBLIC BOOST_INTERPROCESS_SHARED_DIR_FUNC)
elseif(WIN32)
  target_compile_definitions(
    Common
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)

  # TODO: I have no idea what these are. For now, for Windows build, I'm just
  # translating the Qmake
  #
  target_link_libraries(Common PUBLIC ole32 oleaut32)
endif()

# I'm leaving the flatpak config behind, as it should be pretty straighforward,
# and I think CMake can take care of most of it

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
