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

  set(_QT_DEPLOY_BIN_HINTS)
  foreach(_QT_PREFIX IN LISTS CMAKE_PREFIX_PATH)
    if(EXISTS "${_QT_PREFIX}/bin")
      list(APPEND _QT_DEPLOY_BIN_HINTS "${_QT_PREFIX}/bin")
    endif()
  endforeach()

  find_program(
    _DEPLOYQT_EXECUTABLE_FROM_PREFIX
    NAMES windeployqt windeployqt.exe
    PATHS ${_QT_DEPLOY_BIN_HINTS}
    NO_DEFAULT_PATH)

  if(_DEPLOYQT_EXECUTABLE_FROM_PREFIX)
    set(DEPLOYQT_EXECUTABLE
        "${_DEPLOYQT_EXECUTABLE_FROM_PREFIX}"
        CACHE FILEPATH "Path to the windeployqt executable matching the configured Qt prefix"
        FORCE)
  else()
    find_program(DEPLOYQT_EXECUTABLE NAMES windeployqt windeployqt.exe)
  endif()

  message(STATUS "  ${DEPLOYQT_EXECUTABLE}")

  message(CHECK_START "Looking for Rtools $ENV{RTOOLS45_HOME}")
  if(DEFINED ENV{RTOOLS45_HOME})
    file(TO_CMAKE_PATH "$ENV{RTOOLS45_HOME}" _RTOOLS45_HOME)
    set(RTOOLS_ROOT "${_RTOOLS45_HOME}" CACHE PATH "Path to Rtools45 installation root, e.g., C:/rtools45")
	else()
    set(RTOOLS_ROOT "C:/rtools45" CACHE PATH "Path to Rtools45 installation root, e.g., C:/rtools45")
	endif()
  set(RTOOLS_PATH "${RTOOLS_ROOT}/ucrt64" CACHE PATH "Path to Rtools45 UCRT64 package folder, e.g., C:/rtools45/ucrt64")

  get_filename_component(_RTOOLS_ROOT_FROM_UCRT "${RTOOLS_PATH}" DIRECTORY)
  set(RTOOLS_STATIC_TOOLCHAIN_PATH
      "${_RTOOLS_ROOT_FROM_UCRT}/x86_64-w64-mingw32.static.posix"
      CACHE PATH
      "Path to Rtools45 static.posix compiler toolchain, e.g., C:/rtools45/x86_64-w64-mingw32.static.posix")
  set(RTOOLS_BUILD_TOOLS_PATH
      "${_RTOOLS_ROOT_FROM_UCRT}/usr"
      CACHE PATH
      "Path to Rtools45 MSYS build tools, e.g., C:/rtools45/usr")

  if(EXISTS ${RTOOLS_PATH})

    message(CHECK_PASS "found")
    message(STATUS "  UCRT package path: ${RTOOLS_PATH}")
    message(STATUS "  static toolchain:  ${RTOOLS_STATIC_TOOLCHAIN_PATH}")
    message(STATUS "  build tools:       ${RTOOLS_BUILD_TOOLS_PATH}")

    message(CHECK_START 
            "Looking for Rtools legacy and auto remove it, if not work please remove such `RTOOLS44_HOME` manually from Windows environment settings."
    )
    if(DEFINED ENV{RTOOLS42_HOME})
        unset(ENV{RTOOLS42_HOME})
    elseif(DEFINED ENV{RTOOLS43_HOME})
        unset(ENV{RTOOLS43_HOME})
    elseif(DEFINED ENV{RTOOLS44_HOME})
        unset(ENV{RTOOLS44_HOME})
    else()
        message(CHECK_START "No Rtools legacy found")
    endif()

  else()
    message(
      FATAL_ERROR
        "Rtools not found. Rtools is required for building on Windows, please follow the build instruction before you continue. If you have installed the MINGW in a custom location, you can set the RTOOLS_PATH to your UCRT64 package path, e.g., C:/rtools45/ucrt64"
    )
  endif()

  if(NOT EXISTS "${RTOOLS_STATIC_TOOLCHAIN_PATH}/bin/gcc.exe"
     OR NOT EXISTS "${RTOOLS_STATIC_TOOLCHAIN_PATH}/bin/g++.exe")
    message(
      FATAL_ERROR
        "Rtools static.posix compiler toolchain not found. Set RTOOLS_STATIC_TOOLCHAIN_PATH to the Rtools static toolchain path, e.g., C:/rtools45/x86_64-w64-mingw32.static.posix")
  endif()

  if(NOT EXISTS "${RTOOLS_BUILD_TOOLS_PATH}/bin/make.exe")
    message(
      FATAL_ERROR
        "Rtools MSYS make not found. Set RTOOLS_BUILD_TOOLS_PATH to the Rtools build tools path, e.g., C:/rtools45/usr")
  endif()

  if(DEFINED ENV{WIX})
    set(WIX_PATH "$ENV{WIX}/bin" CACHE PATH "Path to your WIX installation, e.g., C:\\Program Files (x86)\\WiX Toolset v3.11\\bin")
  else()
    set(WIX_PATH "C:/Program Files (x86)/WiX Toolset v3.11/bin" CACHE PATH "Path to your WIX installation, e.g., C:\\Program Files (x86)\\WiX Toolset v3.11\\bin")
  endif()

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

  set(RTOOLS_STATIC_TOOLCHAIN_BIN "${RTOOLS_STATIC_TOOLCHAIN_PATH}/bin")
  set(RTOOLS_BUILD_TOOLS_BIN "${RTOOLS_BUILD_TOOLS_PATH}/bin")
  set(RTOOLS_C_COMPILER "${RTOOLS_STATIC_TOOLCHAIN_BIN}/gcc.exe")
  set(RTOOLS_CXX_COMPILER "${RTOOLS_STATIC_TOOLCHAIN_BIN}/g++.exe")
  set(RTOOLS_MAKE_PROGRAM "${RTOOLS_BUILD_TOOLS_BIN}/make.exe")
  set(RTOOLS_R_INTERFACE_GENERATOR "Unix Makefiles")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
