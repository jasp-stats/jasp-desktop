list(APPEND CMAKE_MESSAGE_CONTEXT Config)

# TODO: Find this version number automatically
set(R_VERSION "4.1.2")
set(R_VERSION_MAJOR_MINOR "4.1")
set(CURRENT_R_VERSION ${R_VERSION_MAJOR_MINOR})

# TODO: Replace the version with a variable
if(APPLE)

  #
  # I copy the `R.frameworks` inside the build folder as well,
  # so we will have a similar debug and bundle builds and our
  # paths are not altered that we have to take care of them later.
  # This also leaves the original `R.framework` intact. This is
  # important because, as we are starting to mess with it and installing
  # `jags`, etc., we want to have it to be build dependent when we are
  # experimenting and not always tapping into one instance of it.
  #
  # Another reason for having the Framework being copied into the build
  # folder is that it allows us to leave the CMake in charge of the
  # multi-architecture build.
  #
  if(NOT EXISTS ${CMAKE_BINARY_DIR}/Frameworks/R.framework)
    message(CHECK_START "Copying the R.framework into the build folder")
    message(STATUS "This may take a few minutes...")
    execute_process(COMMAND cp -r ${CMAKE_SOURCE_DIR}/Frameworks
                            ${CMAKE_BINARY_DIR})
    message(CHECK_PASS "done.")
  endif()

  set(_R_FRAMEWORK_PATH ${CMAKE_BINARY_DIR}/Frameworks)

  set(_R_HOME
      "${_R_FRAMEWORK_PATH}/R.framework/Versions/${R_VERSION_MAJOR_MINOR}/Resources"
  )

  set(_R_Library_HOME "${_R_HOME}/library")
  set(_R_EXE "${_R_HOME}/R")
  set(_Rscript_EXE "${_R_HOME}/bin/Rscript")
  set(_Rcpp_HOME "${_R_Library_HOME}/Rcpp")
  set(_RInside_HOME "${_R_Library_HOME}/RInside")

  message(CHECK_START "Checking for 'R.framework'")
  find_library(
    _R_Framework
    NAMES R
    PATHS ${_R_FRAMEWORK_PATH}
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_R_Framework)
    message(CHECK_PASS "found.")
  else()
    message(CHECK_FAIL "not found in ${_R_FRAMEWORK_PATH}")
  endif()

  message(CHECK_START "Checking for 'libR'")
  find_library(
    _LIB_R
    NAMES R
    PATHS ${_R_HOME}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found.")
  else()
    message(CHECK_FAIL "not found in ${_R_HOME}/lib")
  endif()

  if(NOT EXISTS ${_RInside_HOME})
    message(STATUS "RInside is not installed!")

    message(CHECK_START
            "Installing the 'RInside' and 'Rcpp' within the R.framework")

    execute_process(
      COMMAND
        ${_Rscript_EXE} -e install.packages\("RInside",repos="${R_REPOSITORY}"\)
        COMMAND_ERROR_IS_FATAL ANY COMMAND_ECHO NONE
      OUTPUT_QUIET ERROR_QUIET)

    message(CHECK_PASS "successful.")
  endif()

  message(CHECK_START "Checking for 'libRInside'")
  find_library(
    _LIB_RInside
    NAMES RInside
    PATHS ${_RInside_HOME}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_RInside)
    message(CHECK_PASS "found.")
  else()
    message(CHECK_FAIL "not found in ${_RInside_HOME}/libs")
  endif()

elseif(WIN32)
  # TODO
  #   - [ ] I can use the PATH to R/ as _R_framework and everything else should just work

endif()

# Amir: Not sure about this yet.
# GETTEXT_LOCATION = $$(GETTEXT_PATH) #The GETTEXT_PATH can be used as environment for a specific gettext location

# unix {
#   isEmpty(GETTEXT_LOCATION): GETTEXT_LOCATION=/usr/local/bin
#   EXTENDED_PATH = $$(PATH):$$GETTEXT_LOCATION:$$_R_HOME:$$dirname(QMAKE_QMAKE)
# }

# win32 {
#   isEmpty(GETTEXT_LOCATION): GETTEXT_LOCATION=$${_GIT_LOCATION}\usr\bin
#   WINQTBIN  = $$winPathFix($$QMAKE_QMAKE)
#   WINQTBIN ~= s,qmake.exe,,gs
# }

message(STATUS "R Configurations:")

cmake_print_variables(_R_Framework)
cmake_print_variables(_LIB_R)
cmake_print_variables(_LIB_RInside)
cmake_print_variables(_R_HOME)
cmake_print_variables(_Rcpp_HOME)
cmake_print_variables(_RInside_HOME)
cmake_print_variables(_R_Library_HOME)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
