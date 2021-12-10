list(APPEND CMAKE_MESSAGE_CONTEXT Config)

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
    make_directory(${CMAKE_BINARY_DIR}/Frameworks)
    execute_process(WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/Frameworks
                    COMMAND cp -Rpf R.framework ${CMAKE_BINARY_DIR}/Frameworks/)
    message(CHECK_PASS "done.")
  endif()

  set(R_FRAMEWORK_PATH ${CMAKE_BINARY_DIR}/Frameworks)

  set_property(
    DIRECTORY
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES ${R_FRAMEWORK_PATH})

  set(_R_HOME "${R_FRAMEWORK_PATH}/R.framework/Resources")

  cmake_print_variables(R_FRAMEWORK_PATH)
  cmake_print_variables(_R_HOME)

  set(R_LIBRARY_PATH "${_R_HOME}/library")
  set(R_EXECUTABLE "${_R_HOME}/R")
  set(RSCRIPT_EXECUTABLE "${_R_HOME}/Rscript")
  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")

  cmake_print_variables(_R_HOME)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)

  message(CHECK_START "Checking for 'R.framework'")
  find_library(
    _R_Framework
    NAMES R
    PATHS ${R_FRAMEWORK_PATH}
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_R_Framework)
    message(CHECK_PASS "found.")
  else()
    message(CHECK_FAIL "not found in ${R_FRAMEWORK_PATH}")
  endif()

  message(CHECK_START "Checking for 'libR'")
  find_library(
    _LIB_R
    NAMES R
    PATHS ${_R_HOME}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${_R_HOME}/lib")
  endif()

  if(NOT EXISTS ${RINSIDE_PATH})
    message(STATUS "RInside is not installed!")

    message(CHECK_START
            "Installing the 'RInside' and 'Rcpp' within the R.framework")

    execute_process(
      COMMAND ${RSCRIPT_EXECUTABLE} -e
              install.packages\("RInside",repos="${R_REPOSITORY}"\)
      #   COMMAND_ERROR_IS_FATAL ANY COMMAND_ECHO NONE
      # OUTPUT_QUIET ERROR_QUIET
    )

    message(CHECK_PASS "successful.")
  endif()

  message(CHECK_START "Checking for 'libRInside'")
  find_library(
    _LIB_RINSIDE
    NAMES RInside
    PATHS ${RINSIDE_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_RINSIDE)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_RINSIDE}")
  else()
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/libs")
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
cmake_print_variables(_LIB_RINSIDE)
cmake_print_variables(R_LIBRARY_PATH)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
