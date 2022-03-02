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

  message(CHECK_START "Looking for 'mercurial'")
  find_program(
    MERCURIAL_EXECUTABLE
    NAMES hg
    DOC "'mercurial' is needed for downloading some of the dependencies.")

  if(NOT MERCURIAL_EXECUTABLE)
    message(CHECK_FAIL "not found")
    message(FATAL_ERROR "Please install 'mercurial' before continuing.")
  else()
    message(CHECK_PASS "found")
  endif()

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

  find_program(DEPLOYQT_EXECUTABLE macdeployqt)
  if(NOT DEPLOYQT_EXECUTABLE)
    message(
      STATUS
        "macdeployqt not found. If you plan to deploy the JASP.app, you will need this."
    )
  endif()

  find_program(CREATE_DMG_EXECUTABLE create-dmg)
  if(NOT CREATE_DMG_EXECUTABLE)
    message(
      STATUS
        "create-dmg not found. If you plan to make a DMG file, you will need this"
    )
  endif()

endif()

if(WIN32)

  find_program(
    DEPLOYQT_EXECUTABLE
    NAMES windeployqt
    PATHS ${Qt6_DIR}/bin)

  message(CHECK_START "Looking for MSYS2")
  set(MINGW_PATH "C:/msys64/mingw64" CACHE PATH "Path to MinGW x64 folder, e.g., C:/msys64/mingw64")
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

  set(MINGW_C_COMPILER "${MINGW_PATH}/bin/gcc.exe")
  set(MINGW_CXX_COMPILER "${MINGW_PATH}/bin/g++.exe")
  set(MINGW_MAKE_PROGRAM "${MINGW_PATH}/bin/mingw32-make.exe")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
