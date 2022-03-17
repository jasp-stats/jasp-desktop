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
#
# On macOS,
#   - I had to look for the `libbrotlicommon.dylib` and provide it to the JASP.app
#     because `macdeployqt` cannot deal with it
#
# On Windows,
#   - Conan does the most work, 
#   - in addition, I tap into the MSYS2 environment, and grab some files for later
#     use in R-Interface build. If you ran into a problem, then you probably need 
#     to set your MINGW_PATH that CMake can navigate its way to it.

list(APPEND CMAKE_MESSAGE_CONTEXT Libraries)

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)
endif()

find_package(ZLIB REQUIRED)
find_package(Iconv REQUIRED)

if(USE_CONAN)
  find_package(jsoncpp REQUIRED)
endif()

find_package(OpenSSL COMPONENTS SSL Crypto)
if(NOT OpenSSL_FOUND)
  message(
    FATAL_ERROR
      "CMake cannot find the OpenSSL. Set the variable 'OPENSSL_ROOT_DIR' to your OpenSSL installation directory."
  )
endif()

find_package(LibArchive)
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
  Boost 1.78.0 REQUIRED
  COMPONENTS filesystem
             system
             date_time
             timer
             chrono)

if(WINDOWS)
  find_package(
    Boost 1.78.0 REQUIRED
    COMPONENTS nowide
               filesystem
               system
               date_time
               timer
               chrono)
endif()

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
message(STATUS "Found Qt6")
message(STATUS "  ${Qt6_DIR}")

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

endif()

if(APPLE)

  message(CHECK_START "Looking for 'libbrotlicommon'")

  pkg_check_modules(_PKGCONFIG_LIB_BROTLICOMMON REQUIRED libbrotlicommon)

  set(_LIB_BROTLICOMMON
      ${_PKGCONFIG_LIB_BROTLICOMMON_LIBRARY_DIRS}/libbrotlicommon.${_PKGCONFIG_LIB_BROTLICOMMON_VERSION}.dylib
  )

  if(EXISTS "${_LIB_BROTLICOMMON}")
    message(CHECK_PASS "found")
    message(STATUS "  Copying the 'libbrotlicommon' to the local build folder")
    execute_process(
      WORKING_DIRECTORY ${_PKGCONFIG_LIB_BROTLICOMMON_LIBRARY_DIRS}
      COMMAND ${CMAKE_COMMAND} -E copy ${_LIB_BROTLICOMMON}
              ${CMAKE_BINARY_DIR}/libbrotlicommon.1.dylib)
    set(_LIB_BROTLICOMMON ${CMAKE_BINARY_DIR}/libbrotlicommon.1.dylib)
    message(STATUS "  ${_LIB_BROTLICOMMON}")
  else()
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "libbrotli is required for creating the DMG file. ")
  endif()

endif()

# if(NOT WIN32)
#   pkg_check_modules(
#     LIBJSONCPP
#     REQUIRED
#     IMPORTED_TARGET
#     jsoncpp)
# endif()

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

  message(CHECK_START "Looking for libreadstat-1.dll")
  find_file(
    MINGW_LIBREADSTAT_DLL
    NAMES libreadstat-1.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
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

  if(EXISTS ${MINGW_PATH})
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

  if(EXISTS ${MINGW_PATH})
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

  message(CHECK_START "Looking for libboost_nowide-mt.dll")
  find_file(
    MINGW_LIB_BOOST_NOWIDE
    NAMES libboost_nowide-mt.dll
    PATHS ${MINGW_PATH}/bin)

  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_LIB_BOOST_NOWIDE}")
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
