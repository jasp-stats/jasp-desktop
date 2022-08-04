# R.cmake handles the process of downloading, patching, locating and pathing the R
# instance in different platforms. There is a lot that is going on here, so, if you
# don't know what you are doing, you might very well start breaking things!
#
# The general flow of setting R is as follow:
#
#   - Downloading or locating the R instance, e.g., R.framework,
#   - Copying the R instance to the build folder, after patching and preparing it (only on Windows and macOS)
#   - Installing RInside, and Rcpp
#   - Interpolating all the necessary paths and passing them to the rest of the CMake
#
# on macOS,
#   - Because we are cross-building, I am downloading the right Fortran, place it inside the
#     R.framework, and make sure that R can find it. Most of this is being done in the
#     PatchR.cmake were I modify the `etc/Makeconf`. On ARM, R uses the Fortran 11, or so; and
#     on x86_64, it is using the Fortran 8.
#
# Notes:
#   - Be aware that at some point, R will move to use a different Fortran, and 
#     when that happens, someone needs to make sure that the right Fortran is being
#     download, unpacked, and placed in the right location. You can find the 
#     appropriate version in `etc/Makeconf` and the binary here,
#     https://github.com/fxcoudert/gfortran-for-macOS/releases
#   - On GitHub Action,
#     - You probably want to unpack the `https://static.jasp-stats.org/development/gfortran-8.2-Mojave.dmg`
#       into a `.tar.gz`. I think this might elimite some possible issues with the unpacking on
#       their environment. If you have decided to do this, make sure that the structure of the 
#       archive is similiar and things land where they are expected.
#
# Todos:
#
#   - [ ] All the code inside the if(APPLE), and if(WIN32) should be turned into
#       a CMake module. I leave this for later cleanup
#

set(R_BINARY_REPOSITORY "https://static.jasp-stats.org/development")
set(AVAILABLE_R_VERSIONS
    "R-4.1.2"
    "R-4.1.2-arm64"
    "R-4.1.2-win"
    "R-4.1.3"
    "R-4.1.3-arm64"
    "R-4.1.3-win"
	"R-4.2.1"
	"R-4.2.1-arm64"
	"R-4.2.1-win")
set(R_BINARY_HASHES
    "61d3909bc070f7fb86c5a2bd67209fda9408faaa"
    "69e8845ffa134c822d4bdcf458220e841a9eeaa5"
    "c72e68bc50e84bea68a2379073c9fedbdfaeda0c"
    "45121f2c830b0cd7d180aee3fc4cd80d0de1e582"
    "dad405d4f58349403c4976ba50e944502070b209"
    "d4068fdc75334c850d5948a0dc8356d34d3512e1"
	"f83a6c96cedd19193255f94cb01381a273073a3a"
	"05370dd000f0fded68594fc95334808ee25a8e91"
	"37cfb7702a7be00abd64bef8e2ae4252821e5cfc")


list(APPEND CMAKE_MESSAGE_CONTEXT R)

set(R_VERSION
	"4.2.1"
    CACHE STRING "R version to be used")
set(R_VERSION_MAJOR_MINOR "4.2")
set(CURRENT_R_VERSION ${R_VERSION_MAJOR_MINOR})

if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}-arm64")
else()
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}")
endif()

if(WIN32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64 bits
    set(R_DIR_NAME "x64")
  elseif(CMAKE_SIZEOF_VOID_P EQUAL 4) # 32 bits
    set(R_DIR_NAME "i386")
  endif()
endif()

# ------ Preparing Renv Paths
#
set(MODULES_SOURCE_PATH
    ${PROJECT_SOURCE_DIR}/Modules
    CACHE PATH "Location of JASP Modules")

set(MODULES_BINARY_PATH
    "${CMAKE_BINARY_DIR}/Modules"
    CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH
    "${PROJECT_BINARY_DIR}/_cache/renv-root"
    CACHE PATH "Location of renv root directories")

if(FLATPAK_USED)
  set(MODULES_RENV_CACHE_PATH "/app/lib64/renv-cache" CACHE PATH "Location of renv cache directories")
else()
  set(MODULES_RENV_CACHE_PATH "${MODULES_BINARY_PATH}/renv-cache" CACHE PATH "Location of renv cache directories")
endif()


set(JASP_ENGINE_PATH
    "${CMAKE_BINARY_DIR}/Desktop/"
    CACHE PATH "Location of the JASPEngine")

make_directory("${MODULES_BINARY_PATH}")
make_directory("${MODULES_RENV_ROOT_PATH}")
make_directory("${MODULES_RENV_CACHE_PATH}")

cmake_print_variables(MODULES_BINARY_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

# ------

if(APPLE)

  set(R_FRAMEWORK_PATH "${CMAKE_BINARY_DIR}/Frameworks")
  set(R_HOME_PATH
      "${R_FRAMEWORK_PATH}/R.framework/Versions/${R_DIR_NAME}/Resources")
  set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
  set(R_OPT_PATH "${R_HOME_PATH}/opt")
  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH "${R_HOME_PATH}/include")
  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")
  set(RENV_PATH "${R_LIBRARY_PATH}/renv")
  set(ENV{JASP_R_HOME} ${R_HOME_PATH})
  set(ENV{R_HOME} ${R_HOME_PATH})

  cmake_print_variables(R_FRAMEWORK_PATH)
  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(RENV_PATH)

  if(INSTALL_R_FRAMEWORK AND (NOT EXISTS
                              ${CMAKE_BINARY_DIR}/Frameworks/R.framework))

    if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")

      set(R_VERSION_NAME "R-${R_VERSION}-${CMAKE_OSX_ARCHITECTURES}")
      set(R_PACKAGE_NAME "${R_VERSION_NAME}.pkg")
      set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")

      list(
        FIND
        AVAILABLE_R_VERSIONS
        "${R_VERSION_NAME}"
        HASH_INDEX)
      list(
        GET
        R_BINARY_HASHES
        ${HASH_INDEX}
        R_PACKAGE_HASH)

    else()

      set(R_VERSION_NAME "R-${R_VERSION}")
      set(R_PACKAGE_NAME "${R_VERSION_NAME}.pkg")
      set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")

      list(
        FIND
        AVAILABLE_R_VERSIONS
        ${R_VERSION_NAME}
        HASH_INDEX)
      list(
        GET
        R_BINARY_HASHES
        ${HASH_INDEX}
        R_PACKAGE_HASH)

    endif()

    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/Frameworks/R.framework)

      fetchcontent_declare(
        r_pkg
        URL ${R_DOWNLOAD_URL}
        URL_HASH SHA1=${R_PACKAGE_HASH}
        DOWNLOAD_NO_EXTRACT ON
        DOWNLOAD_NAME ${R_PACKAGE_NAME})

      message(CHECK_START "Downloading '${R_PACKAGE_NAME}'")

      fetchcontent_makeavailable(r_pkg)

      if(r_pkg_POPULATED)

        message(CHECK_PASS "done.")

        set(r_pkg_r_home
            ${r_pkg_SOURCE_DIR}/R.framework/Versions/${R_DIR_NAME}/Resources)

        message(CHECK_START "Unpacking '${R_PACKAGE_NAME}'")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND xar -xf ${R_PACKAGE_NAME})
        message(CHECK_PASS "done.")

        message(CHECK_START "Unpacking the payloads")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND tar -xf R-fw.pkg/Payload)

        if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")

          execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                          COMMAND tar -xf tcltk.pkg/Payload -C ${r_pkg_r_home}/)

          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf texinfo.pkg/Payload -C ${r_pkg_r_home}/)

          # Downloading the gfortran

          message(CHECK_START "Downloading gfortran")

          fetchcontent_declare(
            gfortran_tar_gz
            URL "https://static.jasp-stats.org/development/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz"
            URL_HASH
              SHA256=a2ab8be30a7d92a24f53e1509c8c0804f8502f0bc35469750e3f1e233d1c64b8
            DOWNLOAD_NO_EXTRACT ON
            DOWNLOAD_NAME gfortran.tar.gz)

          fetchcontent_makeavailable(gfortran_tar_gz)

          if(gfortran_tar_gz_POPULATED)

            message(CHECK_PASS "done.")

            execute_process(WORKING_DIRECTORY ${gfortran_tar_gz_SOURCE_DIR}
                            COMMAND tar xzf gfortran.tar.gz -C ${r_pkg_r_home}/)

            set(GFORTRAN_PATH ${R_OPT_PATH}/R/arm64/bin)

          else()

            message(CHECK_FAIL "unsuccessful")

          endif()

        else()

          make_directory(${r_pkg_r_home}/opt)
          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf tcltk.pkg/Payload --strip-components=2 -C
                    ${r_pkg_r_home}/opt)

          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf texinfo.pkg/Payload --strip-components=2 -C
                    ${r_pkg_r_home}/opt)

          # Downloading the gfortran
          message(CHECK_START "Downloading gfortran")

          # @todo, it's probably a good idea to unpack this and provide a tar.gz like the other version
          fetchcontent_declare(
            gfortran_dmg
            URL "https://static.jasp-stats.org/development/gfortran-8.2-Mojave.dmg"
            URL_HASH
              SHA256=81d379231ba5671a5ef1b7832531f53be5a1c651701a61d87e1d877c4f06d369
            DOWNLOAD_NO_EXTRACT ON
            DOWNLOAD_NAME gfortran.dmg)

          fetchcontent_makeavailable(gfortran_dmg)

          if(gfortran_dmg_POPULATED)

            message(CHECK_PASS "done.")

            # message(CHECK_START "Unpacking the payloads.")
            execute_process(WORKING_DIRECTORY ${gfortran_dmg_SOURCE_DIR}
                            COMMAND hdiutil attach gfortran.dmg)

            execute_process(
              WORKING_DIRECTORY /Volumes/gfortran-8.2-Mojave/gfortran-8.2-Mojave
              COMMAND ${CMAKE_COMMAND} -E copy gfortran.pkg
                      ${gfortran_dmg_SOURCE_DIR}/)

            execute_process(WORKING_DIRECTORY ${gfortran_dmg_SOURCE_DIR}
                            COMMAND xar -xf gfortran.pkg)

            execute_process(WORKING_DIRECTORY ${gfortran_dmg_SOURCE_DIR}
                            COMMAND tar -xf Payload)

            execute_process(
              WORKING_DIRECTORY ${gfortran_dmg_SOURCE_DIR}
              COMMAND ${CMAKE_COMMAND} -E copy_directory usr/local
                      ${r_pkg_r_home}/opt/local/)

            execute_process(COMMAND hdiutil detach /Volumes/gfortran-8.2-Mojave)

            set(GFORTRAN_PATH ${R_OPT_PATH}/local/gfortran/bin)

          else()

            message(CHECK_FAIL "unsuccessful")

          endif()

        endif()

        message(CHECK_START
                "Copying the 'R.framework' to the jasp-desktop/Frameworks.")

        make_directory(${CMAKE_BINARY_DIR}/Frameworks)
        execute_process(
          WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
          COMMAND cp -Rpf R.framework ${CMAKE_BINARY_DIR}/Frameworks)

        message(CHECK_PASS "done.")
      else()
        message(CHECK_FAIL "failed.")
      endif()

      message(CHECK_START "Locating the 'gfortran'")

      find_program(
        FORTRAN_EXECUTABLE
        NAMES gfortran
        PATHS ${GFORTRAN_PATH}
        NO_DEFAULT_PATH
        DOC "'gfortran' is needed for building some of the R packages")

      if(NOT FORTRAN_EXECUTABLE)
        message(CHECK_FAIL "not found")
        message(FATAL_ERROR "Please install 'gfortran' before continuing.")
      else()
        message(CHECK_PASS "found")
        message(STATUS "  ${FORTRAN_EXECUTABLE}")
      endif()

      # --------------------------------------------------------
      # Patching R.framework and everything related to it ------
      #
      # A this point, R.framework is unpacked, and prepared and
      # has been copied into the build directory.
      # --------------------------------------------------------

      # Patching R's pathing variables, R_HOME, etc. -----------
      message(CHECK_START "Patching bin/R and etc/Makeconf, and library paths")

      include(${CMAKE_SOURCE_DIR}/Tools/CMake/PatchR.cmake)
      cmake_print_variables(r_pkg_r_home)
      cmake_print_variables(R_HOME_PATH)
      patch_r()

      message(CHECK_PASS "done.")

      message(CHECK_START "Patching and signing all the first-party libraries")

      # Patch and sign all first party libraries
	  execute_process(
		COMMAND_ECHO STDOUT
		ERROR_QUIET OUTPUT_QUIET
		WORKING_DIRECTORY ${R_HOME_PATH}
		COMMAND
		  ${CMAKE_COMMAND} -D
		  NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
		  -D PATH=${R_HOME_PATH} -D R_HOME_PATH=${R_HOME_PATH} -D
		  R_DIR_NAME=${R_DIR_NAME} -D
		  SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY} -D SIGNING=1 -D
		  CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
		  ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake)

      # R binary should be patched as well
      message(CHECK_START "Patching /bin/exec/R")
      execute_process(
        COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${R_HOME_PATH}
        COMMAND
          bash ${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
          "${R_HOME_PATH}/bin/exec/R"
          "/Library/Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
      )
      message(CHECK_PASS "successful")

      message(CHECK_START "Signing '${R_HOME_PATH}/bin/exec/R'")

      set(SIGNING_RESULT "timeout")
      while((${SIGNING_RESULT} MATCHES "timeout") OR (${SIGNING_RESULT} STREQUAL
                                                      "1"))
        execute_process(
          COMMAND_ECHO STDOUT
          #ERROR_QUIET OUTPUT_QUIET
          TIMEOUT 30
          WORKING_DIRECTORY ${R_HOME_PATH}
          COMMAND
            codesign --force --verbose --deep ${CODESIGN_TIMESTAMP_FLAG} --sign
            ${APPLE_CODESIGN_IDENTITY} --options runtime
            "${R_HOME_PATH}/bin/exec/R"
          RESULT_VARIABLE SIGNING_RESULT
          OUTPUT_VARIABLE SIGNING_OUTPUT
          ERROR_VARIABLE SIGNING_ERROR)
      endwhile()

      if(NOT (SIGNING_RESULT MATCHES "timeout"))
        message(CHECK_PASS "successful")
      else()
        message(CHECK_FAIL "unsuccessful")
      endif()

      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${R_HOME_PATH}/bin
        COMMAND ln -s ../../../../../../Frameworks Frameworks)

      execute_process(WORKING_DIRECTORY ${R_OPT_PATH}/R/arm64/gfortran
                      COMMAND ln -sfn ${CMAKE_OSX_SYSROOT} SDK)

      # ------------------------

      message(CHECK_PASS "done.")

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

  if(NOT EXISTS ${RENV_PATH})
    message(STATUS "renv is not installed!")
    message(CHECK_START "Installing 'renv'")

    configure_file(${MODULES_SOURCE_PATH}/install-renv.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-renv.R @ONLY)

    set(ENV{JASP_R_HOME} ${R_HOME_PATH})

    execute_process(
	  COMMAND_ECHO STDERR
	  #ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
	  COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save --file=${MODULES_RENV_ROOT_PATH}/install-renv.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/renv)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'renv' installation has failed!")
    endif()

    message(CHECK_PASS "successful.")

    message(CHECK_START "Patching Frameworks/.../library")
    execute_process(
	  COMMAND_ECHO STDOUT
	  #ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND
        ${CMAKE_COMMAND} -D
        NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
        -D PATH=${R_HOME_PATH}/library -D R_HOME_PATH=${R_HOME_PATH} -D
        R_DIR_NAME=${R_DIR_NAME} -D SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY}
        -D SIGNING=1 -D CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
        ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake)
  endif()

  if(NOT EXISTS ${RINSIDE_PATH})
    message(STATUS "RInside is not installed!")

    message(CHECK_START "Installing the 'RInside' and 'Rcpp'")

    configure_file(${MODULES_SOURCE_PATH}/install-RInside.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-RInside.R @ONLY)

    execute_process(
      COMMAND_ECHO STDOUT
      #ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-RInside.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/RInside)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'RInside' installation has failed!")
    endif()

    message(CHECK_PASS "successful.")

    # Patching RInside and RCpp
    message(CHECK_START "Patching Frameworks/.../library")
    execute_process(
	  COMMAND_ECHO STDOUT
	  #ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND
        ${CMAKE_COMMAND} -D
        NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
        -D PATH=${R_HOME_PATH}/library -D R_HOME_PATH=${R_HOME_PATH} -D
        R_DIR_NAME=${R_DIR_NAME} -D SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY}
        -D SIGNING=1 -D CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
        ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake)

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
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/lib")
  endif()

elseif(WIN32)

  set(R_HOME_PATH "${CMAKE_BINARY_DIR}/R")
  set(R_BIN_PATH "${R_HOME_PATH}/bin")
  set(R_LIB_PATH "${R_HOME_PATH}/bin/${R_DIR_NAME}")
  set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
  set(R_OPT_PATH "${R_HOME_PATH}/opt")
  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH "${R_HOME_PATH}/include")
  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")
  set(RENV_PATH "${R_LIBRARY_PATH}/renv")
  

  # This will be added to the install.packages calls
  set(USE_LOCAL_R_LIBS_PATH ", lib='${R_LIBRARY_PATH}'")

  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIB_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)

  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(RENV_PATH)

  message(CHECK_START "Checking for R/")

  if(NOT EXISTS ${CMAKE_BINARY_DIR}/R)

    message(CHECK_FAIL "not found.")

    message(CHECK_START "Downloading R-${R_VERSION}-win.exe")

    set(R_VERSION_NAME "R-${R_VERSION}-win")
    set(R_PACKAGE_NAME "${R_VERSION_NAME}.exe")
    set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")

    list(
      FIND
      AVAILABLE_R_VERSIONS
      "${R_VERSION_NAME}"
      HASH_INDEX)
    list(
      GET
      R_BINARY_HASHES
      ${HASH_INDEX}
      R_PACKAGE_HASH)

    fetchcontent_declare(
      r_win_exe
      URL ${R_DOWNLOAD_URL}
      URL_HASH SHA1=${R_PACKAGE_HASH}
      DOWNLOAD_NO_EXTRACT ON
      DOWNLOAD_NAME ${R_PACKAGE_NAME})

    fetchcontent_makeavailable(r_win_exe)

    if(r_win_exe_POPULATED)

      message(CHECK_PASS "successful.")

      message(CHECK_START "Unpacking and preparing the R instance")

      execute_process(
        WORKING_DIRECTORY ${r_win_exe_SOURCE_DIR}
        COMMAND ${R_PACKAGE_NAME} /CURRENTUSER /verysilent /sp
                /DIR=${r_win_exe_BINARY_DIR}/R)

      file(COPY ${r_win_exe_BINARY_DIR}/R DESTINATION ${CMAKE_BINARY_DIR})

      if(EXISTS ${CMAKE_BINARY_DIR}/R)
        message(CHECK_PASS "successful")
      else()
        message(CHECK_FAIL "failed")
        message(
          FATAL_ERROR
            "CMake has failed to prepare the R environment in the build folder."
        )
      endif()

      # TODOs:
      #   - [ ] I think we should probably remove a few auxiliary files, e.g. uninstall stuff

    else()

      message(CHECK_FAIL "failed.")

    endif()

  else()

    message(CHECK_PASS "found.")

  endif()

  if(NOT EXISTS ${RENV_PATH})
    message(STATUS "renv is not installed!")
    message(CHECK_START "Installing 'renv'")

    configure_file(${MODULES_SOURCE_PATH}/install-renv.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-renv.R @ONLY)

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-renv.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/renv)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'renv' installation has failed!")
    endif()

    message(CHECK_PASS "successful.")
  endif()

  if(NOT EXISTS ${RINSIDE_PATH})
    message(STATUS "RInside is not installed!")

    message(CHECK_START "Installing the 'RInside' and 'Rcpp'")

    configure_file(${MODULES_SOURCE_PATH}/install-RInside.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-RInside.R @ONLY)

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_BIN_PATH}
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-RInside.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/RInside)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'RInside' installation has failed!")
    endif()

    message(CHECK_PASS "successful.")

  endif()

elseif(LINUX)

  message(CHECK_START "Looking for R")

  # If not custom path is not defined, we are looking if R_HOME is set
  if(CUSTOM_R_PATH STREQUAL "")

    set(R_HOME_PATH $ENV{R_HOME})

    if(R_HOME_PATH STREQUAL "")

      message(CHECK_FAIL "unsuccessful")
      message(
        FATAL_ERROR
          "R is not installed in your system. Please install R and try again.")

    else()

      message(CHECK_PASS "successful")
      message(STATUS "R_HOME is ${R_HOME_PATH}")

    endif()

  else()

    if(EXISTS ${CUSTOM_R_PATH})

      set(R_HOME_PATH ${CUSTOM_R_PATH})
      message(CHECK_PASS "successful")
      message(STATUS "Using a custom R installation, ${R_HOME_PATH}")

    else()

      message(CHECK_FAIL "unsuccessful")
      message(FATAL_ERROR "${CUSTOM_R_PATH} does not exist.")

    endif()

  endif()

  if(LINUX_LOCAL_BUILD)
    message(
      STATUS
        "JASP is configured to install all its R depdendencies inside the build folder. If this is not what you want, make sure that 'LINUX_LOCAL_BUILD' parametere is set to OFF, e.g., 'cmake .. -DLINUX_LOCAL_BUILD=OFF'"
    )

    set(R_LIBRARY_PATH "${CMAKE_BINARY_DIR}/R/library")
    set(R_OPT_PATH "${CMAKE_BINARY_DIR}/R/opt")
    make_directory(${R_LIBRARY_PATH})
    make_directory(${R_OPT_PATH})
  else() # Flatpak
    message(
      WARNING
        "JASP is configured to install all its depdendencies into the ${R_HOME_PATH}/library. CMake may not be able to continue if the user does not have the right permission to right into ${R_HOME_PATH}/library folder."
    )

    set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
    set(R_OPT_PATH "${R_HOME_PATH}/opt")
  endif()

  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")
  set(RCPP_PATH "${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH "${R_LIBRARY_PATH}/RInside")
  set(RENV_PATH "${R_LIBRARY_PATH}/renv")

  set(USE_LOCAL_R_LIBS_PATH ", lib='${R_LIBRARY_PATH}'")

  message(CHECK_START "Looking for R.h")
  set(R_INCLUDE_PATH "${R_HOME_PATH}/include")
  if(NOT EXISTS ${R_INCLUDE_PATH})
    find_file(
      _R_H
      NAMES R.h
      PATHS /usr/include /usr/include/R)

    if(_R_H)
      get_filename_component(R_INCLUDE_PATH ${_R_H} DIRECTORY)
      message(CHECK_PASS "found")
      message(STATUS "  ${_R_H}")
    else()
      message(CHECK_FAIL "not found")
      message(FATAL_ERROR "R.h is necessary for building R-Interface library.")
    endif()
  endif()

  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(RENV_PATH)

  message(CHECK_START "Checking for 'libR'")
  find_library(
    _LIB_R
    NAMES R
    PATHS ${R_HOME_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${R_HOME_PATH}/lib")
  endif()

  if(NOT EXISTS ${RENV_PATH})
    message(STATUS "renv is not installed!")
    message(CHECK_START "Installing 'renv'")

    configure_file(${MODULES_SOURCE_PATH}/install-renv.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-renv.R @ONLY)

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${R_HOME_PATH}
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-renv.R)

    if(NOT EXISTS ${R_LIBRARY_PATH}/renv)
      message(CHECK_FAIL "unsuccessful.")
      message(FATAL_ERROR "'renv' installation has failed!")
    endif()

    message(CHECK_PASS "successful.")
  endif()

  if(NOT EXISTS ${RINSIDE_PATH})
    message(STATUS "RInside is not installed!")

    message(CHECK_START "Installing the 'RInside' and 'Rcpp'")

    configure_file(${MODULES_SOURCE_PATH}/install-RInside.R.in
                   ${MODULES_RENV_ROOT_PATH}/install-RInside.R @ONLY)

    execute_process(
      ERROR_QUIET OUTPUT_QUIET
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-RInside.R)

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
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_RINSIDE}")
  else()
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/lib")
  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
