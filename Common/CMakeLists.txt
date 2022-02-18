list(APPEND CMAKE_MESSAGE_CONTEXT Common)

find_package(LibArchive)
if(NOT LibArchive_FOUND)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    LibArchive
    REQUIRED
    IMPORTED_TARGET
    libarchive)
endif()

set(Boost_USE_STATIC_LIBS ON)
find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

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

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp)

# Note:
# I might need to use `qt_add_library` instead only because we
# are depending on `Qt::Core`. Maybe later, we can free the Common
# from it.
add_library(Common ${HEADER_FILES} ${SOURCE_FILES})

if(INSTALL_JASP_REQUIRED_LIBRARIES)
  add_dependencies(Common jsoncpp-install)
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    _LIB_R_PKG_CONFIG
    # REQUIRED
    IMPORTED_TARGET libR)

  # I think we need to deduct most information about R using the
  # PkgConfig but for now, I'm doing it as minimal as possible
  # to keep it consistent to other platforms

  # This is needed because we are including the Rcpp, and otherwise
  # Rcpp cannot find R's headers
  target_include_directories(Common PUBLIC ${_LIB_R_PKG_CONFIG_INCLUDE_DIRS}
                                           /usr/include /usr/include/R)

  find_library(
    _LIB_RT
    NAMES rt
    PATHS /usr/lib /usr/lib32 /usr/lib64
    NO_DEFAULT_PATH NO_CACHE)

  target_link_libraries(Common PUBLIC ${_LIB_RT})
  target_link_options(
    Common
    PUBLIC
    -lrt)

endif()

target_include_directories(
  Common
  PUBLIC ${LIBJSONCPP_INCLUDE_DIRS}
         ${CONAN_INCLUDE_DIRS_libjson}
         # JASP
         jaspColumnEncoder
         # R
         ${R_HOME_PATH}/include
         ${RCPP_PATH}/include
         # Conan
         ${CONAN_INCLUDE_DIRS_JSONCPP}
         ${CONAN_INCLUDE_DIRS_LIBARCHIVE}
         #
         ${Boost_INCLUDE_DIRS})

target_link_libraries(
  Common
  PUBLIC Boost::nowide
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         # Conan
         # LibArchive::LibArchive
         # /usr/lib64/librt.so
         # ${LIBJSONCPP_LINK_LIBRARIES}
         # ${CONAN_LIBS_libarchive}
         # ${CONAN_LIBS_jsoncpp}
         $<$<BOOL:${USE_CONAN}>:CONAN_PKG::jsoncpp>
         $<$<BOOL:${USE_CONAN}>:CONAN_PKG::libarchive>
         LibArchive::LibArchive
         ${LIBJSONCPP_LINK_LIBRARIES}
         # Only if JASP_USES_QT_HERE is ON
         $<$<BOOL:${JASP_USES_QT_HERE}>:Qt::Core>)

target_compile_definitions(
  Common PUBLIC $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
                $<$<BOOL:${JASP_TIMER_USED}>:PROFILE_JASP>)

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(Common PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
endif()

# TODOs:
#
# - [ ] After confirming these, they can all be rewritten as generator-expressions
# - [ ] I don't think most of the Boost stuffs have effect here, and they should
#       probably move into `Dependencies.cmake` where I actually build Boost.
#
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
  # target_link_libraries(Common PUBLIC ole32 oleaut32)
endif()

# I'm leaving the flatpak config behind, as it should be pretty straighforward,
# and I think CMake can take care of most of it

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
