list(APPEND CMAKE_MESSAGE_CONTEXT Config)

if(BUILD_WITH_SYSTEM_R)
  message(STATUS "[JASP]: Building with system R...")

  # Trying to set the RPATH using CMake, but it's not there yet...
  # set(CMAKE_SKIP_BUILD_RPATH OFF) set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)

  # set(CMAKE_INSTALL_RPATH
  # "/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/Rcpp/libs/;/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/RInside/libs/")
  # set(CMAKE_BUILD_RPATH
  # "/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/Rcpp/libs/;/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/RInside/libs/")

  set(CMAKE_BUILD_WITH_INSTALL_RPATH)

  find_package(PkgConfig REQUIRED)

  pkg_check_modules(
    LIBR
    REQUIRED
    IMPORTED_TARGET
    libR)

  if(${LIBR_FOUND})

    set(_R_HOME "/opt/homebrew/lib/R/")
    set(_R_EXE "/opt/homebrew/bin/R")
    set(_R_Library "opt/homebrew/lib/R/library/")
    set(_Rcpp_HOME
        "/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/Rcpp/")
    set(_RInside_HOME
        "/opt/homebrew/lib/R/${R_VERSION_MAJOR_MINOR}/site-library/RInside/")

    # message(STATUS "[JASP]: Checking for 'Rcpp.so'") find_library(_LIB_RCPP
    # NAMES Rcpp.so PATHS ${_Rcpp_HOME}/libs NO_CACHE REQUIRED)

    # if (_LIB_RCPP) message(STATUS "[JASP]: Found the 'Rcpp.so' library in "
    # ${_LIB_RCPP}) else() message(FATAL_ERROR "[JASP]: Couldn't find the
    # 'Rcpp.so'") endif()

    message(STATUS "[JASP]: Checking for 'RInside.so'")
    find_library(
      _LIB_RInside
      NAMES RInside.so
      PATHS ${_RInside_HOME}/libs NO_CACHE REQUIRED)

    if(_LIB_RInside)
      message(STATUS "[JASP]: Found the 'RInside.so' library in "
                     ${_LIB_RInside})
    else()
      message(FATAL_ERROR "[JASP]: Couldn't find the 'RInside.so'")
    endif()

  endif()

else()

  message(STATUS "Building using the R.framework")

  # TODO: Replace the version with a variable
  if(APPLE)

    # I copy the `R.frameworks` inside the build folder as well,
    # so we will have a similar debug and bundle builds and our
    # paths are not altered that we have to take care of them later.
    # This also leaves the original `R.framework` intact. This is
    # important because, as we are starting to mess with it and installing
    # `jags`, etc., we want to have it to be build dependent when we are
    # experimenting and not always tapping into one instance of it.
    if(NOT EXISTS ${CMAKE_BINARY_DIR}/Frameworks/R.framework)
      message(CHECK_START "Copying the R.framework into the build folder")
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
          ${_Rscript_EXE} -e
          install.packages\("RInside",repos="${R_REPOSITORY}"\)
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
