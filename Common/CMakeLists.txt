
list(APPEND CMAKE_MESSAGE_CONTEXT Common)

find_package(PkgConfig REQUIRED)
find_package(LibArchive REQUIRED)

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)
  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)
else()
  find_library(
    LIBJSONCPP
    NAMES jsoncpp
    HINTS ${jsoncpp_LIBRARY_DIRS})

  set(LIBJSONCPP_INCLUDE_DIRS ${jsoncpp_INCLUDE_DIRS})
  set(LIBJSONCPP_LIBRARY_DIRS ${jsoncpp_LIBRARY_DIRS})
  set(LIBJSONCPP_LINK_LIBRARIES ${LIBJSONCPP})
endif()

find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp)

add_library(Common ${HEADER_FILES} ${SOURCE_FILES})

target_include_directories(
  Common
  PUBLIC ${Boost_INCLUDE_DIRS}
         ${LIBJSONCPP_INCLUDE_DIRS}
         ${LibArchive_INCLUDE_DIRS}
         jaspColumnEncoder)

target_include_directories(Common PUBLIC ${_R_HOME}/include
                                         ${_Rcpp_HOME}/include)

find_package(Qt6 REQUIRED COMPONENTS Core)

target_link_libraries(
  Common
  PUBLIC Boost::nowide
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         ${LibArchive_LIBRARIES}
         ${LIBJSONCPP_LINK_LIBRARIES}
         Qt::Core)

# target_link_options(Common PUBLIC -lboost_filesystem)

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
