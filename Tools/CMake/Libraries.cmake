# Libraries.cmake contains all the necessary logics for finding JASP
# required libraries. Here, everything goes through CMake.
#
#
# `find_package` uses Find*.cmake files that Conan has generated, and use
# them to create targets and variables that are pointing to artifacts of a
# library. CMake targets, e.g., Boost::filesystem, can be used to both include
# the headers and link libraries, if a library doesn't have a Target, then some
# helper variables, e.g., LibArchive_INCLUDE_DIRS can be used for including the
# header files.
#
# On Linux,
#   - For some reason, CMake has trouble finding the `librt.so`, so I had to force
#     it a bit; other than that, everything is the same.
#   - Since we can have everything build properly on Linux, all these find_packages
#     reply on their libraries to have a proper CMake helper file, otherwise they'll
#     fail, and that's why I'm building the ReadStat for instance.
#     - `jsoncpp` has a decent PkgConfig config file, so, I'm getting everything from there
#     - `readstat` doesn't provide anything, so, I try to manually find it,
#       and if I cannot, the configuration fails
#     - `jags` has a PkgConfig, but I don't necessary use it since I already have
#       all its path, etc working for other system, I only change the `jags_HOME` to
#       point to the right place, and then I have everything...
#
# On macOS,
#   - I had to look for the `libbrotlicommon.dylib` and provide it to the JASP.app
#     because `macdeployqt` cannot deal with it
#
# On Windows,
#   - Conan does the most work,
#   - In addition, I tap into the MSYS2 environment, and grab some files for later
#     use in R-Interface build. If you ran into a problem, then you probably need
#     to set your RTOOLS_PATH that CMake can navigate its way to it.

list(APPEND CMAKE_MESSAGE_CONTEXT Libraries)

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)
endif()

find_package(ZLIB 1.2 REQUIRED)
find_package(Iconv 1.16 REQUIRED)
find_package(SQLite3 3.37.0 REQUIRED)

#if(USE_CONAN)
#  find_package(jsoncpp 1.9 REQUIRED)
#endif()

find_package(OpenSSL 1.1.1 COMPONENTS SSL Crypto)
if(NOT OpenSSL_FOUND)
  message(
    FATAL_ERROR
      "CMake cannot find the OpenSSL. Set the variable 'OPENSSL_ROOT_DIR' to your OpenSSL installation directory."
  )
endif()

find_package(LibArchive 3.5)
if((NOT LibArchive_FOUND) AND (NOT WIN32))
  pkg_check_modules(LibArchive IMPORTED_TARGET libarchive)

  if(NOT LibArchive_FOUND)
    message(STATUS "LibArchive not found.")
    message(
      FATAL_ERROR
        "If you have already installed the libarchive, you can direct CMake to its path, e.g., `-DCMAKE_PREFIX_PATH=/opt/homebrew/libarchive/`"
    )
  endif()
endif()

set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.78)
find_package(Qt6 REQUIRED COMPONENTS Core)

get_target_property(QT_TARGET_TYPE Qt6::Core TYPE)
set(USE_QT_STATIC_LIBS OFF)
message(STATUS "QT_TARGET_TYPE: ${QT_TARGET_TYPE}")
if(QT_TARGET_TYPE STREQUAL STATIC_LIBRARY)
  set(USE_QT_STATIC_LIBS ON)
endif()

if(NOT FLATPAK_USED)
  find_package(Qt6 REQUIRED COMPONENTS
      Gui
      OpenGL
      Widgets
      Qml
      QuickTemplates2
      Quick
      QuickLayouts
      QuickControls2
      QuickControls2Impl
      QmlWorkerScript
      QuickWidgets
  )
  if(NOT USE_QT_STATIC_LIBS)
    find_package(
      Qt6 REQUIRED COMPONENTS
               WebEngineQuick
               WebChannel
               Svg
               Network
               HttpServer
               Xml
               Sql
               DBus
               LabsFolderListModel
    )
  endif()

else()

  message(STATUS "flatpak arch is $ENV{FLATPAK_ARCH}")

  find_package(
    Qt6 REQUIRED
    COMPONENTS Core
               Gui
               OpenGL
               Widgets
               Qml
               WebChannel
               Svg
               Sql
               Network
               HttpServer
               Xml
               DBus
               QuickTemplates2
               LabsFolderListModel
               Quick
               QuickLayouts
               QuickControls2
               QuickControls2Impl
               QmlWorkerScript
               QuickWidgets)

  find_package(
    Qt6WebEngineQuick
    REQUIRED
    PATHS
    "/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/cmake/Qt6WebEngineQuick/"
       ${Qt6WebEngineQuick_DIR}
    NO_DEFAULT_PATH)

endif()

if(LINUX)

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

  if(NOT JASP_SYNTAX_INTERFACE_ONLY)

    if(FLATPAK_USED)
      set(LIBREADSTAT_INCLUDE_DIRS /app/include)
      set(LIBREADSTAT_LIBRARY_DIRS /app/lib)
    else()
      set(LIBREADSTAT_INCLUDE_DIRS /usr/local/include /usr/include)
      # The last two library paths handle the two most common multiarch cases.
      # Other multiarch-compliant paths may come up but should be rare.
      set(LIBREADSTAT_LIBRARY_DIRS /usr/local/lib /usr/lib /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu)
    endif()

    message(CHECK_START "Looking for libreadstat.so")
    find_library(LIBREADSTAT_LIBRARIES libreadstat.so
              HINTS ${LIBREADSTAT_LIBRARY_DIRS} REQUIRED)

    if(EXISTS ${LIBREADSTAT_LIBRARIES})
      message(CHECK_PASS "found")
      message(STATUS "  ${LIBREADSTAT_LIBRARIES}")
    else()
      message(CHECK_FAIL "not found")
      message(
        FATAL_ERROR
          "ReadStat is required for building on Linux, please follow the build instruction before you continue."
      )
    endif()

    # ---- libsodium ----
    message(CHECK_START "Looking for `libsodium`")
    set(libsodium_INCLUDE_DIR /usr/include /app/lib64/)
    set(LIBSODIUM_LIBRARY_DIRS /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu /app/include/)

    message(CHECK_START "Looking for libsodium.so")
    find_library(libsodium_LIBRARIES libsodium.so
              HINTS ${LIBSODIUM_LIBRARY_DIRS} REQUIRED)

    if(EXISTS ${libsodium_LIBRARIES})
      message(CHECK_PASS "found")
      message(STATUS "  ${LIBSODIUM_LIBRARIES}")
    else()
      message(CHECK_FAIL "not found")
      message(
        FATAL_ERROR
          "libsodium is required for building on Linux, please follow the build instruction before you continue."
      )
    endif()

    # ---- FreeXL ----
    message(CHECK_START "Looking for `libfreexl`")
    set(LIBFREEXL_INCLUDE_DIRS /usr/include /app/lib64/)
    set(LIBFREEXL_LIBRARY_DIRS /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu /app/include/)

    message(CHECK_START "Looking for libfreexl.so")
    find_library(LIBFREEXL_LIBRARIES libfreexl.so
              HINTS ${LIBFREEXL_LIBRARY_DIRS} REQUIRED)

    if(EXISTS ${LIBFREEXL_LIBRARIES})
      message(CHECK_PASS "found")
      message(STATUS "  ${LIBFREEXL_LIBRARIES}")
    else()
      message(CHECK_FAIL "not found")
      message(
        FATAL_ERROR
          "FreeXL is required for building on Linux, please follow the build instruction before you continue."
      )
    endif()

    # ---- librdata ----
    message(CHECK_START "Looking for `librdata`")
    set(LIBRDATA_INCLUDE_DIRS /usr/include/ /usr/include/rdata /usr/local/include /usr/local/include/rdata /app/include)
    set(LIBRDATA_LIBRARY_DIRS /usr/local/lib /usr/lib /app/lib64 /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu)

    message(CHECK_START "Looking for librdata.so")
    find_library(LIBRDATA_LIBRARIES librdata.so
              HINTS ${LIBRDATA_LIBRARY_DIRS} REQUIRED)

    if(EXISTS ${LIBRDATA_LIBRARIES})
      message(CHECK_PASS "found")
      message(STATUS "  ${LIBRDATA_LIBRARIES}")
    else()
      message(CHECK_FAIL "not found")
      message(
        FATAL_ERROR
          "librdata is required for building on Linux(e.g. librdata-dev on debian/ubuntu), please follow the build instruction before you continue."
      )
    endif()

    find_package(PkgConfig)
    #pkg_check_modules(_PKGCONFIG_LIB_JSONCPP REQUIRED jsoncpp>=1.9)

  endif() # NOT JASP_SYNTAX_INTERFACE_ONLY

endif()

if(APPLE AND NOT JASP_SYNTAX_INTERFACE_ONLY)

  message(CHECK_START "Looking for 'libbrotlicommon'")

  find_package(Brotli 1.0.9 REQUIRED)
  find_package(freexl 2.0.99 REQUIRED)
  find_package(librdata REQUIRED)
  find_package(libsodium 1.0.20 REQUIRED)

endif()

if(WIN32 AND NOT JASP_SYNTAX_INTERFACE_ONLY)

  include(FindRToolsDLLPath)

  find_package(freexl 2.0.99 REQUIRED)
  find_package(libsodium 1.0.20 REQUIRED)

  copy_rtools_header(RTOOLS_LIBREADSTAT_H	readstat.h		${CMAKE_SOURCE_DIR}/Desktop/data/importers/readstat/readstat.h)
  copy_rtools_header(RTOOLS_LIBRDATA_H		rdata.h			${CMAKE_SOURCE_DIR}/Desktop/data/importers/rdata/rdata.h)

  find_rtools_dll_path(RTOOLS_ZLIB_DLL            "zlib1.dll")
  find_rtools_dll_path(RTOOLS_MSYS_DLL            "msys-2.0.dll")
  find_rtools_dll_path(RTOOLS_LIBBZ2_DLL          "libbz2-1.dll")
  find_rtools_dll_path(RTOOLS_LIBLZMA_DLL         "liblzma-5.dll")
  find_rtools_dll_path(RTOOLS_LIBICONV_DLL        "libiconv-2.dll")
  find_rtools_dll_path(RTOOLS_LIBRDATA_DLL        "librdata-0.dll")
  find_rtools_dll_path(RTOOLS_LIBSTDCPP_DLL       "libstdc++-6.dll")
  find_rtools_dll_path(RTOOLS_LIBRDATA_DLL_A      "librdata.dll.a")
  find_rtools_dll_path(RTOOLS_LIBREADSTAT_DLL     "libreadstat-1.dll")
  find_rtools_dll_path(RTOOLS_LIBGCC_S_SEH_DLL    "libgcc_s_seh-1.dll")
  find_rtools_dll_path(RTOOLS_LIBREADSTAT_DLL_A   "libreadstat.dll.a")
  find_rtools_dll_path(RTOOLS_LIBWINPTHREAD_DLL   "libwinpthread-1.dll")

  # ICU DLL: needed by Qt 6.1+ on Windows Server 2019 (build < 18362)
  find_file(SYSTEM_ICU_DLL
    NAMES icu.dll
    PATHS "C:/Windows/System32"
    DOC "Combined ICU DLL for older Windows systems"
  )
  if(NOT SYSTEM_ICU_DLL)
    message(WARNING "icu.dll not found — JASP may fail to start on Windows Server 2019")
  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
