list(APPEND CMAKE_MESSAGE_CONTEXT Libraries)

find_package(PkgConfig REQUIRED)

find_package(ZLIB REQUIRED)
find_package(Iconv REQUIRED)
# find_package(OpenSSL REQUIRED COMPONENTS SSL Crypto)

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

find_package(
  Qt6 REQUIRED
  COMPONENTS Core
             Gui
             OpenGL
             Widgets
             Qml
             WebEngineQuick
             WebChannel
             Svg
             Network
             PrintSupport
             Xml
             DBus
             QuickTemplates2
             Quick
             QuickLayouts
             QuickControls2
             QuickControls2Impl
             QmlWorkerScript
             QuickWidgets
             Core5Compat)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  find_library(
    _LIB_RT
    NAMES rt
    PATHS /usr/lib64
          /usr/lib
          /usr/lib32
          NO_CACHE)

  target_link_libraries(Common PUBLIC ${_LIB_RT})
  target_link_options(Common PUBLIC -lrt)

endif()

if(APPLE)

  message(CHECK_START "Looking for 'libbrotlicommon'")

  find_library(_LIB_BROTLICOMMON NAMES brotlicommon REQUIRED)

  message(CHECK_PASS "found")
  message(STATUS "  ${_LIB_BROTLICOMMON}")

endif()

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)

  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)

  pkg_check_modules(
    LIBREADSTAT
    REQUIRED
    IMPORTED_TARGET
    readstat)

else()

  # WARNING: This is not the way to do it, and it does not work with Ninja :\

  # jsoncpp
  set(LIBJSONCPP_INCLUDE_DIRS ${jsoncpp_INCLUDE_DIRS})
  set(LIBJSONCPP_LIBRARY_DIRS ${jsoncpp_LIBRARY_DIRS})
  set(LIBJSONCPP_LINK_LIBRARIES ${jsoncpp_LIBRARY_DIRS}/libjsoncpp.a)

  # readstat
  set(LIBREADSTAT_INCLUDE_DIRS ${readstat_INCLUDE_DIRS})
  set(LIBREADSTAT_LIBRARY_DIRS ${readstat_LIBRARY_DIRS})
  set(LIBREADSTAT_LINK_LIBRARIES ${LIBREADSTAT_LIBRARY_DIRS}/libreadstat.a)

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
