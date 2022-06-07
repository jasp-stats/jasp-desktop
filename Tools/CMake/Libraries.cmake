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
#     to set your MINGW_PATH that CMake can navigate its way to it.
#   - Boost::nowide is a library that needs to be linked statically to the executable.
#     This is not necessary on macOS and Linux.

list(APPEND CMAKE_MESSAGE_CONTEXT Libraries)

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)
endif()

find_package(ZLIB 1.2 REQUIRED)
find_package(Iconv 1.16 REQUIRED)

if(USE_CONAN)
  find_package(jsoncpp 1.9 REQUIRED)
endif()

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
find_package(
  Boost 1.78 REQUIRED
  COMPONENTS filesystem
             system
             date_time
             timer
             chrono)

if(WINDOWS)
  find_package(
    Boost 1.78 REQUIRED
    COMPONENTS nowide
               filesystem
               system
               date_time
               timer
               chrono)
endif()

if(NOT FLATPAK_USED)
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
               LabsFolderListModel
               Quick
               QuickLayouts
               QuickControls2
               QuickControls2Impl
               QmlWorkerScript
               QuickWidgets
               Core5Compat)

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
               Network
               PrintSupport
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

  find_package(
    Qt6Core5Compat
    REQUIRED
    PATHS
	  "/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/cmake/Qt6Core5Compat/"
	  ${Qt6Core5Compat_DIR}
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

  if(FLATPAK_USED)
    set(LIBREADSTAT_INCLUDE_DIRS /app/include)
    set(LIBREADSTAT_LIBRARY_DIRS /app/lib)
  else()
    set(LIBREADSTAT_INCLUDE_DIRS /usr/local/include)
    set(LIBREADSTAT_LIBRARY_DIRS /usr/local/lib)
  endif()

  message(CHECK_START "Looking for libreadstat.so")
  find_file(LIBREADSTAT_LIBRARIES libreadstat.so
            HINTS ${LIBREADSTAT_LIBRARY_DIRS} REQUIRED)

  if(EXISTS ${LIBREADSTAT_LIBRARIES})
    message(CHECK_PASS "found")
    message(STATUS "  ${LIBREADSTAT_LIBRARIES}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  find_package(PkgConfig)
  pkg_check_modules(_PKGCONFIG_LIB_JSONCPP REQUIRED jsoncpp>=1.9)

endif()

if(APPLE)

  message(CHECK_START "Looking for 'libbrotlicommon'")

  find_package(Brotli 1.0.9 REQUIRED)

  set(_LIB_BROTLICOMMON
      ${Brotli_LIB_DIRS}/libbrotlicommon.1.0.9.dylib
  )

  if(EXISTS "${_LIB_BROTLICOMMON}")
    message(CHECK_PASS "found")
    message(STATUS "  Copying the 'libbrotlicommon' to the local build folder")
    execute_process(
      WORKING_DIRECTORY ${Brotli_LIB_DIRS}
      COMMAND ${CMAKE_COMMAND} -E copy ${_LIB_BROTLICOMMON}
              ${CMAKE_BINARY_DIR}/libbrotlicommon.1.dylib)
    set(_LIB_BROTLICOMMON ${CMAKE_BINARY_DIR}/libbrotlicommon.1.dylib)
    message(STATUS "  ${_LIB_BROTLICOMMON}")
  else()
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "libbrotli is required for creating the DMG file. ")
  endif()

endif()

if(WIN32)
  # ReadStat

  message(CHECK_START "Looking for libreadstat.dll.a")
  find_file(
    MINGW_LIBREADSTAT_DLL_A
    NAMES libreadstat.dll.a
    PATHS ${MINGW_PATH}/lib)

  if(EXISTS ${MINGW_LIBREADSTAT_DLL_A})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBREADSTAT_DLL_A}")
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

  if(EXISTS ${MINGW_LIBREADSTAT_H})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBREADSTAT_H}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libreadstat-1.dll")
  find_file(
    MINGW_LIBREADSTAT_DLL
    NAMES libreadstat-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBREADSTAT_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBREADSTAT_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for zlib1.dll")
  find_file(
    MINGW_ZLIB_DLL
    NAMES zlib1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_ZLIB_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_ZLIB_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "ReadStat is required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libiconv-2.dll")
  find_file(
    MINGW_LIBICONV_DLL
    NAMES libiconv-2.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBICONV_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBICONV_DLL}")
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
    MINGW_LIBGCC_S_SEH_DLL
    NAMES libgcc_s_seh-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBGCC_S_SEH_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBGCC_S_SEH_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libstdc++-6.dll")
  find_file(
    MINGW_LIBSTDCPP_DLL
    NAMES libstdc++-6.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBSTDCPP_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBSTDCPP_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libwinpthread-1.dll")
  find_file(
    MINGW_LIBWINPTHREAD_DLL
    NAMES libwinpthread-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBWINPTHREAD_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBWINPTHREAD_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libboost_nowide-mt.dll")
  find_file(
    MINGW_LIB_BOOST_NOWIDE_DLL
    NAMES libboost_nowide-mt.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIB_BOOST_NOWIDE_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIB_BOOST_NOWIDE_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  message(CHECK_START "Looking for libjsoncpp-24.dll")
  find_file(
    MINGW_LIBJSONCPP_DLL
    NAMES libjsoncpp-24.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_LIBJSONCPP_DLL})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIBJSONCPP_DLL}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 and some of its libraries are required for building on Windows, please follow the build instruction before you continue."
    )
  endif()

  # jags
  # This could all go into its module later, and these can
  # turn into a function, but I don't want to do it now
  # because I'm uncertain about CMake variable scopping
  # message(CHECK_START "Looking for jags files")
  # find_file(
  #   MINGW_LIBJAGS_BAT
  #   NAMES jags.bat
  #   PATHS ${MINGW_PATH}/bin REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_BAT}")
  # find_file(
  #   MINGW_LIBJAGS
  #   NAMES libjags-4.dll
  #   PATHS ${MINGW_PATH}/bin REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS}")
  # find_file(
  #   MINGW_LIBJAGS_JRMATH
  #   NAMES libjrmath-0.dll
  #   PATHS ${MINGW_PATH}/bin REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_JRMATH}")
  # find_file(
  #   MINGW_LIB_BLAS
  #   NAMES libblas.dll
  #   PATHS ${MINGW_PATH}/bin REQUIRED)
  # message(STATUS "  ${MINGW_LIB_BLAS}")
  # find_file(
  #   MINGW_LIB_LAPACK
  #   NAMES liblapack.dll
  #   PATHS ${MINGW_PATH}/bin REQUIRED)
  # message(STATUS "  ${MINGW_LIB_LAPACK}")

  # set(MINGW_LIBJAGS_HEADERS_PATH "${MINGW_PATH}/include/JAGS")
  # message(STATUS "  ${MINGW_LIBJAGS_HEADERS_PATH}")
  # set(MINGW_LIBJAGS_LIBRARIES_PATH "${MINGW_PATH}/lib/JAGS")
  # message(STATUS "  ${MINGW_LIBJAGS_LIBRARIES_PATH}")
  # set(MINGW_LIBJAGS_PKGCONFIG_PATH "${MINGW_PATH}/lib/pkgconfig")
  # message(STATUS "  ${MINGW_LIBJAGS_PKGCONFIG_PATH}")
  # set(MINGW_LIBJAGS_MODULES_PATH "${MINGW_PATH}/lib/JAGS/modules-4")
  # message(STATUS "  ${MINGW_LIBJAGS_MODULES_PATH}")

  # find_file(
  #   MINGW_LIBJAGS_LIBJAGS_A
  #   NAMES libjags.dll.a
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_LIBJAGS_A}")
  # find_file(
  #   MINGW_LIBJAGS_LIBJAGS_LA
  #   NAMES libjags.la
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_LIBJAGS_LA}")
  # find_file(
  #   MINGW_LIBJAGS_LIBJRMATH_A
  #   NAMES libjrmath.dll.a
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_LIBJRMATH_A}")
  # find_file(
  #   MINGW_LIBJAGS_LIBJRMATH_LA
  #   NAMES libjrmath.la
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_LIBJRMATH_LA}")
  # find_file(
  #   MINGW_LIB_BLAS_DLL_A
  #   NAMES libblas.dll.a
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIB_BLAS_DLL_A}")
  # find_file(
  #   MINGW_LIB_LAPACK_DLL_A
  #   NAMES liblapack.dll.a
  #   PATHS ${MINGW_PATH}/lib REQUIRED)
  # message(STATUS "  ${MINGW_LIB_LAPACK_DLL_A}")

  # find_file(
  #   MINGW_LIBJAGS_JAGS_TERMINAL_EXE
  #   NAMES jags-terminal.exe
  #   PATHS ${MINGW_PATH}/libexec REQUIRED)
  # message(STATUS "  ${MINGW_LIBJAGS_JAGS_TERMINAL_EXE}")

  # message(CHECK_PASS "found")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
