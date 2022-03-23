# Program.cmake contains all the logics needed to find all the
# necessary software and tools that are being used during the
# configuration, building, and deployment of JASP.
#
# On Linux,
#   CMake makes sure that the Compiler, etc. exists, and beyond
#   that we are looking for a few other tools, e.g., gfortran
#   that is required for building some of the R packages.
#
# On macOS,
#   Beyond the Xcode, which is being verified by CMake, we are
#   expecting two third party tools, `create-dmg` and `parallel`
#
# On Windows,
#   We need to make sure that MinGW and MSYS2 environment exists,
#   and that we can find it. In addition, we look binaries of
#   WIX Toolset.
#     - If CMake cannot find either of the heat, candle, or light
#       executable, you can use the `WIX_PATH` variable to nudge
#       it to the right directory.

list(APPEND CMAKE_MESSAGE_CONTEXT Programs)

# ------ Git

find_package(Git)

if(NOT WIN32)

  find_program(MAKE NAMES gmake nmake make)
  find_program(ACLOCAL NAMES aclocal)
  find_program(AUTOCONF NAMES autoconf)
  find_program(AUTORECONF NAMES autoreconf)
  find_program(AUTOUPDATE NAMES autoupdate)
  find_program(CCACHE_EXECUTABLE NAMES ccache)
  find_program(IWYU_EXECUTABLE NAMES include-what-you-use)

endif()

# ------ Linux Tools/Programs

if(LINUX)

  message(CHECK_START "Looking for 'gfortran'")
  find_program(
    FORTRAN_EXECUTABLE
    NAMES gfortran REQUIRED
    DOC "'gfortran' is needed for building some of the R packages")

  if(NOT FORTRAN_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "Please install 'gfortran' before continuing.")
  else()
    message(CHECK_PASS "found")
  endif()

  message(CHECK_START "Looking for 'flex'")
  find_program(
    FLEX_EXECUTABLE
    NAMES flex
    DOC "'flex' is needed for building some of the dependencies.")

  if(NOT FLEX_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "Please install 'flex' before continuing.")
  else()
    message(CHECK_PASS "found")
  endif()

  message(CHECK_START "Looking for 'bison'")
  find_program(
    BISON_EXECUTABLE
    NAMES flex
    DOC "'flex' is needed for building some of the dependencies.")

  if(NOT BISON_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "Please install 'bison' before continuing.")
  else()
    message(CHECK_PASS "found")
  endif()

endif()

# ----------------------

if(APPLE)

  # message(CHECK_START "Looking for 'gfortran'")
  # if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
  #   set(GFORTRAN_PATH "/opt/homebrew/bin")
  # else()
  #   set(GFORTRAN_PATH "/usr/local/bin")
  # endif()
  # message(STATUS "Expecting 'gfortran' in ${GFORTRAN_PATH}")

  # find_program(
  #   FORTRAN_EXECUTABLE
  #   NAMES gfortran
  #         gfortran-11
  #         gfortran-12
  #         REQUIRED
  #   HINTS /usr/local/bin
  #   DOC "'gfortran' is needed for building some of the R packages")

  # set(FORTRAN_EXECUTABLE "")

  # if(NOT FORTRAN_EXECUTABLE)
  #   message(CHECK_FAIL "not found")
  #   message(FATAL_ERROR "Please install 'gfortran' before continuing.")
  # else()
  #   message(CHECK_PASS "found")
  #   message(STATUS "  ${FORTRAN_EXECUTABLE}")
  # endif()

  message(CHECK_START "Looking for 'macdeployqt'")
  find_program(DEPLOYQT_EXECUTABLE macdeployqt)
  if(NOT DEPLOYQT_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(
      WARNING
        "macdeployqt not found. If you plan to deploy the JASP.app, you will need this."
    )
  else()
    message(CHECK_PASS "found")
    message(STATUS "  ${DEPLOYQT_EXECUTABLE}")
  endif()

  message(CHECK_START "Looking for 'codesign'")
  find_program(CODESIGN_EXECUTABLE codesign)
  if(NOT CODESIGN_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(
      WARNING
        "codesign not found. If you plan to deploy the JASP.app, you will need this."
    )
  else()
    message(CHECK_PASS "found")
    message(STATUS "  ${CODESIGN_EXECUTABLE}")
  endif()

  find_program(CREATE_DMG_EXECUTABLE create-dmg)
  if(NOT CREATE_DMG_EXECUTABLE)
    message(
      WARNING
        "create-dmg not found. If you plan to make a DMG file, you will need this"
    )
  endif()

  find_program(CREATE_DMG_EXECUTABLE parallel)
  if(NOT CREATE_DMG_EXECUTABLE)
    message(
      WARNING "parallel not found. If you plan to sign and distribute JASP.")
  endif()

endif()

if(WIN32)

  find_program(
    DEPLOYQT_EXECUTABLE
    NAMES windeployqt
    PATHS ${Qt6_DIR}/bin)

  message(CHECK_START "Looking for MSYS2")
  set(MINGW_PATH
      "C:/msys64/mingw64"
      CACHE PATH "Path to MinGW x64 folder, e.g., C:/msys64/mingw64")
  if(EXISTS ${MINGW_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${MINGW_PATH}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "MSYS2 is required for building on Windows, please follow the build instruction before you continue. If you have installed the MINGW in a custom location, you can set the MINGW_PATH to your MinGW x64 path, e.g., C:/msys64/mingw64"
    )
  endif()

  message(CHECK_START "Looking for MSYS2")
  set(RTOOLS_PATH
      "C:/rtools40"
      CACHE PATH "Path to rtools40 folder, e.g., C:/rtools40")
  if(EXISTS ${RTOOLS_PATH})
    message(CHECK_PASS "found")
    message(STATUS "  ${RTOOLS_PATH}")
  else()
    message(CHECK_FAIL "not found")
    message(
      FATAL_ERROR
        "Rtool40 is required for building on some of the R packages, and JASP modules."
    )
  endif()

  set(WIX_PATH
      ""
      CACHE
        PATH
        "Path to your WIX installation, e.g., C:\\Program Files (x86)\\WiX Toolset v3.11\\bin"
  )

  find_program(
    HEAT_EXECUTABLE
    NAMES heat.exe
    HINTS ${WIX_PATH})
  cmake_path(
    NATIVE_PATH
    HEAT_EXECUTABLE
    NORMALIZE
    HEAT_EXECUTABLE_NATIVE)
  message(STATUS "  ${HEAT_EXECUTABLE_NATIVE}")

  find_program(
    CANDLE_EXECUTABLE
    NAMES candle.exe
    HINTS ${WIX_PATH})
  cmake_path(
    NATIVE_PATH
    CANDLE_EXECUTABLE
    NORMALIZE
    CANDLE_EXECUTABLE_NATIVE)
  message(STATUS "  ${CANDLE_EXECUTABLE_NATIVE}")

  find_program(
    LIGHT_EXECUTABLE
    NAMES light.exe
    HINTS ${WIX_PATH})
  cmake_path(
    NATIVE_PATH
    LIGHT_EXECUTABLE
    NORMALIZE
    LIGHT_EXECUTABLE_NATIVE)
  message(STATUS "  ${LIGHT_EXECUTABLE_NATIVE}")

  set(MINGW_C_COMPILER "${MINGW_PATH}/bin/gcc.exe")
  set(MINGW_CXX_COMPILER "${MINGW_PATH}/bin/g++.exe")
  set(MINGW_MAKE_PROGRAM "${MINGW_PATH}/bin/mingw32-make.exe")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
