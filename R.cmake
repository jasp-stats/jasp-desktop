# R/Environment Configurations
#
#   - Assuming that R.framework exists, this set the all the R-related paths
#   - Install RInside, and Rcpp, and prepare them to be linked to the R-Interface

# Todos:
#
# - [ ] Maybe, the entire R.framework prepration should be a target. The advantages
#       is that it can be triggered independently, however, it will only be
#       done during the build stage and not configuration

list(APPEND CMAKE_MESSAGE_CONTEXT Config)

option(INSTALL_R_FRAMEWORK "Whether to download and prepare R.framework" OFF)

# TODO: Replace the version with a variable
if(APPLE)

  # These are futher paths, and may not exist yet!
  # CMake throws if it cannot setup the R.framework properly and get to
  # these paths!

  set(R_FRAMEWORK_PATH ${CMAKE_BINARY_DIR}/Frameworks)
  set(R_HOME_PATH "${R_FRAMEWORK_PATH}/R.framework/Resources")
  set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
  set(R_OPT_PATH "${R_HOME_PATH}/opt")
  set(R_EXECUTABLE "${R_HOME_PATH}/R")
  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")

  cmake_print_variables(R_FRAMEWORK_PATH)
  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(R_EXECUTABLE)

  # This whole thing can be a module of itself but after I make sure that it
  # fully works

  # TODOs:
  #
  # - [ ] I need to check whether I should download the `.tar.gz` version of the
  #       framework instead. It seems that the `.pkg` version requires xQuartz but
  #       the former does not.
  #
  if(INSTALL_R_FRAMEWORK AND (NOT EXISTS
                              ${CMAKE_BINARY_DIR}/Frameworks/R.framework))

    if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
      set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}-arm64")
      set(R_PACKAGE_NAME "R-${R_VERSION}-${CMAKE_HOST_SYSTEM_PROCESSOR}.pkg")
      set(R_DOWNLOAD_URL
          "https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-${R_VERSION}-arm64.pkg"
      )
      set(R_PACKAGE_HASH "69e8845ffa134c822d4bdcf458220e841a9eeaa5")

    else()
      set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}")
      set(R_PACKAGE_NAME "R-${R_VERSION}.pkg")
      set(R_DOWNLOAD_URL
          "https://cran.r-project.org/bin/macosx/base/R-${R_VERSION}.pkg")
      set(R_PACKAGE_HASH "61d3909bc070f7fb86c5a2bd67209fda9408faaa")
    endif()

    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/Frameworks/R.framework)

      fetchcontent_declare(
        r_pkg
        URL ${R_DOWNLOAD_URL}
        URL_HASH SHA1=${R_PACKAGE_HASH}
        DOWNLOAD_NO_EXTRACT ON
        DOWNLOAD_NAME ${R_PACKAGE_NAME})

      message(CHECK_START "Downloading '${R_PACKAGE_NAME}'.")
      fetchcontent_populate(r_pkg)
      message(CHECK_PASS "done.")

      fetchcontent_getproperties(r_pkg)

      if(r_pkg_POPULATED)

        set(r_pkg_r_home
            ${r_pkg_SOURCE_DIR}/R.framework/Versions/${R_DIR_NAME}/Resources)

        message(CHECK_START "Unpacking '${R_PACKAGE_NAME}'.")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND xar -xf ${R_PACKAGE_NAME})
        message(CHECK_PASS "done.")

        message(CHECK_START "Unpacking the payloads.")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND tar -xf R-fw.pkg/Payload)

        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND tar -xf tcltk.pkg/Payload -C ${r_pkg_r_home}/)

        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND tar -xf texinfo.pkg/Payload -C ${r_pkg_r_home}/)

        message(CHECK_PASS "done.")

        message(CHECK_START "Patching the R.framework.")

        # These can be done using the file(COPY files DESTINATION dir)
        execute_process(
          WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/Tools/macOS
          COMMAND cp install_name_prefix_tool.sh ${r_pkg_SOURCE_DIR})

        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND chmod +x install_name_prefix_tool.sh)

        file(
          GLOB_RECURSE
          SO_LIBRARIES
          "${r_pkg_SOURCE_DIR}/R.framework/Versions/${R_DIR_NAME}/Resources/*.so"
        )
        file(
          GLOB_RECURSE
          DYLIB_LIBRARIES
          "${r_pkg_SOURCE_DIR}/R.framework/Versions/${R_DIR_NAME}/Resources/*.dylib"
        )
        list(
          FILTER
          DYLIB_LIBRARIES
          EXCLUDE
          REGEX
          ".*dSYM.*")

        message(CHECK_PASS "done.")

        message(CHECK_START
                "Patching bin/R and etc/Makeconf, and library paths")

        # Patching R -------------

        include(${CMAKE_SOURCE_DIR}/PatchR.cmake)
        cmake_print_variables(r_pkg_r_home)
        cmake_print_variables(R_HOME_PATH)
        patch_r()

        include(${CMAKE_SOURCE_DIR}/R.framework.cmake)
        cmake_print_variables(r_pkg_SOURCE_DIR)
        patch_ld_paths(${r_pkg_r_home} ${r_pkg_SOURCE_DIR})

        # ------------------------

        message(CHECK_PASS "done.")

        message(CHECK_START
                "Copying the 'R.framework' to the jasp-desktop/Frameworks.")

        make_directory(${CMAKE_BINARY_DIR}/Frameworks)
        execute_process(
          WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
          COMMAND cp -Rpf R.framework ${CMAKE_BINARY_DIR}/Frameworks)

        message(CHECK_PASS "done.")
      endif()

    endif()

  endif()

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
    message(
      FATAL_ERROR
        "CMake cannot locate 'R.framework' inside this build folder.
         You can use `cmake .. -DINSTALL_R_FRAMEWORK=ON` to ask CMake to install
         it for you.")
  endif()

  set_property(
    DIRECTORY
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES ${R_FRAMEWORK_PATH})

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
    PATHS ${R_HOME_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${R_HOME_PATH}/lib")
  endif()

  if(NOT EXISTS ${RINSIDE_PATH})
    message(STATUS "RInside is not installed!")

    message(CHECK_START
            "Installing the 'RInside' and 'Rcpp' within the R.framework")

    file(WRITE ${CMAKE_BINARY_DIR}/Modules/renv-root/install-RInside.R
         "install.packages('RInside', repos='${R_REPOSITORY}')")

    execute_process(
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND ./R --slave --no-restore --no-save
              --file=${CMAKE_BINARY_DIR}/Modules/renv-root/install-RInside.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/RInside)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'RInside' installation has failed!")
    endif()

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
#   EXTENDED_PATH = $$(PATH):$$GETTEXT_LOCATION:$$R_HOME_PATH:$$dirname(QMAKE_QMAKE)
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
