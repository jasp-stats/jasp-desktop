# JASP.cmake sets a lot of JASP parameters, e.g., version. In addition to adding
# some of the CMake variables, it also add some _global_ definition to the compiler
# based on the value of those variables, e.g., `-DJASP_DEBUG`.
#
#
# TODOs:
#   - [ ] Most of these add_definitions should turn into `set_target_definitions`
#       and link to their appropriate targets later on.

list(APPEND CMAKE_MESSAGE_CONTEXT JASP)

if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/.git")

  message(CHECK_START "Retrieving the git-branch information")

  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse --verify HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_COMMIT
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  message(CHECK_PASS "done.")

  set(GIT_CURRENT_BRANCH ${GIT_BRANCH})
  set(GIT_CURRENT_COMMIT ${GIT_COMMIT})

  cmake_print_variables(GIT_CURRENT_BRANCH)
  cmake_print_variables(GIT_CURRENT_COMMIT)
endif()

set(JASP_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(JASP_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(JASP_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(JASP_VERSION_TWEAK ${PROJECT_VERSION_TWEAK})

set(JASP_VERSION ${CMAKE_PROJECT_VERSION})
set(JASP_SHORT_VERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR})

message(STATUS "Version: ${CMAKE_PROJECT_VERSION}")

set(JASP_VERSION_MSIX_PATCH_POSTFIX
    ""
    CACHE STRING "Microsoft does not allow packages of same version to be uploaded to store and forces TWEAK 0, so we add a large version postfix to PATCH in the appmanifest in case we publish a broken version and wish to switch it out. Nobody sees it anyway")

set(MSIX_STORE_PUBLISHER
    "CN=044465FF-CD1D-4EC4-B82B-C244199F66F9"
    CACHE STRING "Publisher set for store msix package")

set(MSIX_STORE_BETA_PUBLISHER
    "CN=044465FF-CD1D-4EC4-B82B-C244199F66F9"
    CACHE STRING "Publisher set for store beta msix package")

set(MSIX_SIDELOAD_PUBLISHER
    "CN=Universiteit van Amsterdam, O=Universiteit van Amsterdam, L=Amsterdam, S=Noord-Holland, C=NL, OID.2.5.4.15=Government Entity, OID.1.3.6.1.4.1.311.60.2.1.3=NL, SERIALNUMBER=34370207"
    CACHE STRING "Publisher set for sideloaded msix package")

set(MSIX_NIGHTLY_PUBLISHER
    "CN=JASP, O=JASP, L=Amsterdam, S=Noord-Holland, C=NL"
    CACHE STRING "Publisher set for nightly msix package")

set(MSIX_SIGN_CERT_PATH
  "C:\\JASPSelfSign.pfx"
  CACHE STRING "Path to selfsign cert for Nightlies")

set(MSIX_SIGN_CERT_PASSWORD
  "0000"
  CACHE STRING "Password selfsign cert for Nightlies")

set(R_PKG_CELLAR_PATH
  "${CMAKE_BINARY_DIR}/_cache/cellar/"
  CACHE STRING "Set the path for an renv package cellar to be used during build phase. Changing this disables the use of a remote cellar")

set(R_PKG_CELLAR_REMOTE
  ""
  CACHE STRING "URL to downloand a remote package cellar from (Make it https). If not set a platform default will be set by cmake")

set(R_PKG_CELLAR_DOWNLOAD_REMOTE
  ON
  CACHE BOOL "Disable Downloading off a remote renv package cellar to use")

set(RPKG_DOWNLOAD_ONLY
  OFF
  CACHE BOOL "If enabled renv will not install JASP module deps but just download them. Usefull to make a cellar for Flatpak")

set(REGENERATE_LOCKFILE
  OFF
  CACHE BOOL "If enabled jaspModuleInstaller will generate a fresh lockfile")

set(MODULE_INSTALL_MODE
  "localizeAll"
  CACHE STRING "identicalToLockfile or localizeModuleOnly or localizeAll. Sets how much is pulled remote or from source folder")

# TODO:
# - [ ] Rename all JASP related variables to `JASP_*`. This way,
#       Qt Creator can categorize them nicely in its CMake configurator
option(JASP_PRINT_ENGINE_MESSAGES
       "Whether or not JASPEngine prints log messages" ON)

if(NOT R_REPOSITORY)
  set(R_REPOSITORY
      "http://cloud.r-project.org"
      CACHE STRING "The CRAN mirror used by 'renv' and 'install.packages'")
endif()
if(FLATPAK_USED AND LINUX)
  set(R_PKG_CELLAR_PATH "/app/lib64/cellar")
  set(R_REPOSITORY "file:///app/lib64/local-cran")
endif()

message(STATUS "CRAN mirror: ${R_REPOSITORY}")

#this handles the cellar download.
if(R_PKG_CELLAR_DOWNLOAD_REMOTE AND R_PKG_CELLAR_PATH STREQUAL "${CMAKE_BINARY_DIR}/_cache/cellar/" AND NOT EXISTS "${CMAKE_BINARY_DIR}/_cache/cellar/")
  #set appropriate default remote if needed
  if(R_PKG_CELLAR_REMOTE STREQUAL "")
    if(APPLE)
      SET(R_PKG_CELLAR_REMOTE "https://static.jasp-stats.org/development/cellars/cellar_macOS_x86_64_latest.tar.gz")
      if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
        SET(R_PKG_CELLAR_REMOTE "https://static.jasp-stats.org/development/cellars/cellar_macOS_arm64_latest.tar.gz")
      endif()
    elseif(WIN32)
      SET(R_PKG_CELLAR_REMOTE "https://static.jasp-stats.org/development/cellars/cellar_Windows_x86_64_latest.tar.gz")
    endif()
  endif()

  #download and extract the cellar
  if(NOT R_PKG_CELLAR_REMOTE STREQUAL "")
    message(STATUS "Remote cellar: ${R_PKG_CELLAR_REMOTE}")
    file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/cellar/") 
    file(DOWNLOAD "${R_PKG_CELLAR_REMOTE}" "${CMAKE_BINARY_DIR}/_cache/cellar.tar.gz" TLS_VERIFY ON)
    file(ARCHIVE_EXTRACT INPUT "${CMAKE_BINARY_DIR}/_cache/cellar.tar.gz" DESTINATION "${CMAKE_BINARY_DIR}/_cache/")
  endif()
endif()

# This one is GLOBAL
# should be off for flatpak though because it is always build in debug mode (but the symbols are split off)
if(CMAKE_BUILD_TYPE STREQUAL "Debug" AND NOT FLATPAK_USED)
  add_definitions(-DJASP_DEBUG)
endif()

option(PRINT_ENGINE_MESSAGES
       "Indicates whether the log contains JASPEngine messages" ON)

option(JASP_USES_QT_HERE "Indicates whether some projects are using Qt" ON)

# add_definitions(-DJASP_RESULTS_DEBUG_TRACES)

option(JASP_TIMER_USED "Use JASP timer for profiling" OFF)
if(JASP_TIMER_USED)
  add_definitions(-DPROFILE_JASP)
endif()

option(UPDATE_JASP_SUBMODULES
       "Whether to automatically initialize and update the submodules" OFF)

message(CHECK_START "Checking for CRYPT_KEY")
set(CRYPT_KEY
    ""
    CACHE STRING "")

if(CRYPT_KEY STREQUAL "")
  # Let's see if the user has set something in the environment
  set(CRYPT_KEY $ENV{CRYPTKEY})

  if(CRYPT_KEY STREQUAL "")

    message(CHECK_PASS "set to default.")

    set(CRYPT_KEY "0x0c2ad4a4acb9f023")

  else()

    message(CHECK_PASS "found in environment.")

  endif()

  message(STATUS "  ${CRYPT_KEY}")

else()
  message(CHECK_PASS "set by user in cmake config.")
endif()

# Dealing with Git submodules

if(UPDATE_JASP_SUBMODULES)
  if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
    # Update submodules as needed
    message(STATUS "Submodule update")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      RESULT_VARIABLE GIT_SUBMOD_RESULT)
    if(NOT
       GIT_SUBMOD_RESULT
       EQUAL
       "0")
      message(
        FATAL_ERROR
          "git submodule update --init --recursive failed with ${GIT_SUBMOD_RESULT}, please checkout submodules"
      )
    endif()
  endif()
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
