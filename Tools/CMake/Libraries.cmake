list(APPEND CMAKE_MESSAGE_CONTEXT Libraries)

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)
endif()

find_package(ZLIB REQUIRED)
find_package(Iconv REQUIRED)
# find_package(OpenSSL REQUIRED COMPONENTS SSL Crypto)

find_package(LibArchive)
if((NOT LibArchive_FOUND) AND (NOT WIN32))
  pkg_check_modules(
    LibArchive
    REQUIRED
    IMPORTED_TARGET
    libarchive)
endif()

set(Boost_USE_STATIC_LIBS ON)
find_package(
  Boost 1.78.0 REQUIRED
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

  message(CHECK_START "Looking for `librt`")
  find_library(
    _LIB_RT
    NAMES rt
    PATHS /usr/lib64
          /usr/lib
          /usr/lib32
          NO_CACHE)

  if(_LIB_RT)
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_RT}")
  else()
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "librt is required for building libCommon on Linux")
  endif()

endif()

if(APPLE)

  message(CHECK_START "Looking for 'libbrotlicommon'")

  find_library(_LIB_BROTLICOMMON NAMES brotlicommon)

  if(_LIB_BROTLICOMMON)
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_BROTLICOMMON}")
  else()
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "libbrotli is required for creating the DMG file. ")
  endif()

endif()

if(NOT WIN32)
  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)
endif()

if((NOT INSTALL_JASP_REQUIRED_LIBRARIES) AND (NOT WIN32))

  # This most likely doesn't work because readstat doesn't have a proper
  # pkgconfig.pc, but I leave it here fore when they do!
  pkg_check_modules(
    LIBREADSTAT
    REQUIRED
    IMPORTED_TARGET
    readstat)

endif()

if(WIN32)
  # ReadStat

  message(CHECK_START "Looking for libreadstat.dll.a")
  find_file(
    MINGW_LIBREADSTAT
    NAMES libreadstat.dll.a
    PATHS ${MINGW_PATH}/lib)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBREADSTAT}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for readstat.h")
  find_file(
    MINGW_LIBREADSTAT_H
    NAMES readstat.h
    PATHS ${MINGW_PATH}/include)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBREADSTAT_H}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  # MinGW Libraries

  message(CHECK_START "Looking for libgcc_s_seh-1.dll")
  find_file(
    MINGW_LIBGCC_S_SEH
    NAMES libgcc_s_seh-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBGCC_S_SEH}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libstdc++-6.dll")
  find_file(
    MINGW_LIBSTDCPP
    NAMES libstdc++-6.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBSTDCPP}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libwinpthread-1.dll")
  find_file(
    MINGW_LIBWINPTHREAD
    NAMES libwinpthread-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBWINPTHREAD}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libjsoncpp-24.dll")
  find_file(
    MINGW_LIBJSONCPP
    NAMES libjsoncpp-24.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBJSONCPP}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
