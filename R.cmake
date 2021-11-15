cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Config)

if(BUILD_WITH_SYSTEM_R)
  message(STATUS "[JASP]: Building with system R...")

  # Trying to set the RPATH using CMake, but it's not there yet...
  # set(CMAKE_SKIP_BUILD_RPATH OFF) set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)

  # set(CMAKE_INSTALL_RPATH
  # "/opt/homebrew/lib/R/4.1/site-library/Rcpp/libs/;/opt/homebrew/lib/R/4.1/site-library/RInside/libs/")
  # set(CMAKE_BUILD_RPATH
  # "/opt/homebrew/lib/R/4.1/site-library/Rcpp/libs/;/opt/homebrew/lib/R/4.1/site-library/RInside/libs/")

  set(CMAKE_BUILD_WITH_INSTALL_RPATH)

  find_package(PkgConfig REQUIRED)

  pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

  if(${LIBR_FOUND})

    set(_R_HOME "/opt/homebrew/lib/R/")
    set(_R_EXE "/opt/homebrew/bin/R")
    set(_R_Library "opt/homebrew/lib/R/library/")
    set(_Rcpp_HOME "/opt/homebrew/lib/R/4.1/site-library/Rcpp/")
    set(_RInside_HOME "/opt/homebrew/lib/R/4.1/site-library/RInside/")

    # message(STATUS "[JASP]: Checking for 'Rcpp.so'") find_library(_LIB_RCPP
    # NAMES Rcpp.so PATHS ${_Rcpp_HOME}/libs NO_CACHE REQUIRED)

    # if (_LIB_RCPP) message(STATUS "[JASP]: Found the 'Rcpp.so' library in "
    # ${_LIB_RCPP}) else() message(FATAL_ERROR "[JASP]: Couldn't find the
    # 'Rcpp.so'") endif()

    message(STATUS "[JASP]: Checking for 'RInside.so'")
    find_library(_LIB_RInside NAMES RInside.so PATHS ${_RInside_HOME}/libs
                                                     NO_CACHE REQUIRED)

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
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(_R_HOME
        "${CMAKE_SOURCE_DIR}/Frameworks/R.framework/Versions/4.1/Resources")
    set(_R_Library_HOME "${_R_HOME}/library")
    set(_R_EXE "${_R_HOME}/R")
    set(_Rscript_EXE "${_R_HOME}/bin/Rscript")
    set(_Rcpp_HOME "${_R_Library_HOME}/Rcpp")
    set(_RInside_HOME "${_R_Library_HOME}/RInside")

    set(_R_FRAMEWORK_PATH ${CMAKE_SOURCE_DIR}/Frameworks)

    message(CHECK_START "Checking for 'R.framework'")
    find_library(_R_Framework NAMES R PATHS ${_R_FRAMEWORK_PATH}
                 NO_DEFAULT_PATH NO_CACHE REQUIRED)

    if(_R_Framework)
      message(CHECK_PASS "found.")
    else()
      message(CHECK_FAIL "not found in ${_R_FRAMEWORK_PATH}")
    endif()

    message(CHECK_START "Checking for 'libR'")
    find_library(_LIB_R NAMES R PATHS ${_R_HOME}/lib NO_DEFAULT_PATH NO_CACHE
                                                     REQUIRED)

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
          install.packages\("RInside",repos="http://cran.r-project.org"\)
          COMMAND_ERROR_IS_FATAL ANY COMMAND_ECHO NONE
        OUTPUT_QUIET ERROR_QUIET)

      message(CHECK_PASS "successful.")
    endif()

    message(CHECK_START "Checking for 'libRInside'")
    find_library(_LIB_RInside NAMES RInside PATHS ${_RInside_HOME}/lib
                 NO_DEFAULT_PATH NO_CACHE REQUIRED)

    if(_LIB_RInside)
      message(CHECK_PASS "found.")
    else()
      message(CHECK_FAIL "not found in ${_RInside_HOME}/libs")
    endif()

  endif()

endif()

message(STATUS "R Configurations:")
list(APPEND CMAKE_MESSAGE_INDENT "  ")

cmake_print_variables(_R_Framework)
cmake_print_variables(_LIB_R)
cmake_print_variables(_LIB_RInside)
cmake_print_variables(_R_HOME)
cmake_print_variables(_Rcpp_HOME)
cmake_print_variables(_RInside_HOME)
cmake_print_variables(_R_Library_HOME)

list(POP_BACK CMAKE_MESSAGE_INDENT)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
