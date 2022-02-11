# If set, CMake tries to download all the necessary dependencies
#
#   - Boost, jsoncpp, and readstat

# Notes:
#
# - FetchContent variables will be lower cased, that's why we have
#   weird variable names like r_win_exe_POPULATED.
# - "CMake 3.22 updated the FindPkgConfig module to allow passing
#   arbitrary arguments to the pkgconfig executable." This could come handy
#   later on when dealing with some of the more annoying dependencies

# TODOs:
#
# - [ ] CMake caches the build and everything, but it does it
#       after running the `./configure`. This produces a lot of
#       unnecessary outputs during the build. I think I can simply
#       and if they exist, ignore the process but that is not perfect!
#       However, since we are not changing these packages so often,
#       we should be fine. Either way, for now I leave it like this.
#
# - [ ] I would love to add these to the ExternalPackage,
#       but CMake proved to be not so good at dealing with a
#       tiniest anomaly/warning that the build process throw
#       at it, and this could cause it to stop the build!
#         - LOG_CONFIGURE ON
#         - LOG_BUILD ON
#         - LOG_INSTALL ON
#         - LOG_OUTPUT_ON_FAILURE ON
#
# - [ ] I would have liked to choose a better name for _deps but since CPM
#       uses it, I went for it that I don't have to maintain two paths

# Adding caching for CPM, this is going to be useful later that we
# want to have CI builds on GitHub, see here: https://github.com/cpm-cmake/CPM.cmake/wiki/Caching-with-CPM.cmake-and-ccache-on-GitHub-Actions
set(CPM_SOURCE_CACHE ${PROJECT_SOURCE_DIR}/.cache/CPM)

add_custom_target(Dependencies)

# The rest of dependencies are automatically being handled by the CPM
if(INSTALL_JASP_REQUIRED_LIBRARIES)
  add_dependencies(Dependencies readstat-install)
endif()

# Here,we download the R binary, extract its content and copy it into the
# right place.
#   - [ ] I still have to test the windows version
#   - On macOS, this will place the R.framework into the ${CMAKE_SOURCE_DIR}/Frameworks
#
if(WIN32)
  find_program(EXTRACT NAMES extract)

  set(R_PACKAGE_NAME "R-${R_VERSION}.pkg")
  set(R_DOWNLOAD_URL
      "https://cran.r-project.org/bin/windows/base/R-${R_VERSION}-win.exe")
  set(R_PACKAGE_HASH "776384c989ea061728e781b6b9ce5b92")

  if(NOT EXISTS ${CMAKE_SOURCE_DIR}/R)

    fetchcontent_declare(
      r_win_exe
      URL ${R_DOWNLOAD_URL}
      URL_HASH MD5=${R_PACKAGE_HASH}
      DOWNLOAD_NO_EXTRACT ON
      DOWNLOAD_NAME ${R_PACKAGE_NAME})

    fetchcontent_populate(r_win_exe)
    fetchcontent_getproperties(r_win_exe)

    if(r_win_exe_POPULATED)
      execute_process(
        WORKING_DIRECTORY ${r_win_exe_SOURCE_DIR}
        COMMAND extract /c ${R_PACKAGE_NAME} /l ${r_win_exe_BINARY_DIR})
      execute_process(
        WORKING_DIRECTORY ${r_win_exe_SOURCE_DIR}
        COMMAND cp -r ${r_win_exe_BINARY_DIR} ${CMAKE_SOURCE_DIR}/R)

    endif()

  endif()

elseif(APPLE)

  # Moved to the R.cmake

else()

endif()

set(CPM_USE_LOCAL_PACKAGES ON)

# This is rather slow because it has to download all the submodules,
# when the final version of 1.78.0 is released, we can replace it
# with the .tar.gz to have a faster download.
#   - For some reason, the .tar.gz doesn't support CMake!
#   - It's not necessary to pass a list of targets. CMake only builds want it needs
# cpmaddpackage(
#   NAME
#   Boost
#   VERSION
#   1.78.0
#   OPTIONS
#   "BUILD_TESTING:BOOL=OFF"
#   "CMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}"
#   "BOOST_INCLUDE_LIBRARIES:STRING=nowide\\\\;filesystem\\\\;system\\\\;date_time\\\\;timer\\\\;chrono\\\\;atomic"
#   GITHUB_REPOSITORY
#   "boostorg/boost"
#   GIT_TAG
#   "boost-1.78.0")

#
# Removing these, since they are part of the Xcode and MSVC's toolchain
#
if(WIN32)

  cpmaddpackage(
    NAME
    LibArchive
    VERSION
    3.5.2
    OPTIONS
    "ENABLE_TEST OFF"
    "JSONCPP_WITH_POST_BUILD_UNITTEST OFF"
    GITHUB_REPOSITORY
    "libarchive/libarchive"
    GIT_TAG
    "v3.5.2")

  cpmaddpackage(
    NAME
    ZLIB
    VERSION
    1.2.11
    GITHUB_REPOSITORY
    "madler/zlib"
    GIT_TAG
    "v1.2.11")

  cpmaddpackage(
    NAME
    ZSTD
    VERSION
    1.5.2
    GITHUB_REPOSITORY
    "facebook/zstd"
    GIT_TAG
    "v1.5.2")

endif()

# ----- jsoncpp ------
#
# So, the problem is that jsoncpp has a faulty CMake config,
# and this triggers the install stage during the build. This
# triggers an install command on all targets and messes up
# everything. For this reason, I had to use the `ExternalProject`
# and handle the build myself.
#
# cpmaddpackage(
#   NAME
#   jsoncpp
#   VERSION
#   1.9.5
#   OPTIONS
#   "JSONCPP_WITH_TESTS OFF"
#   "JSONCPP_WITH_POST_BUILD_UNITTEST OFF"
#   "JSONCPP_WITH_CMAKE_PACKAGE OFF"
#   GITHUB_REPOSITORY
#   "open-source-parsers/jsoncpp"
#   GIT_TAG
#   "1.9.5")

#
# By setting, `-DCMAKE_BINARY_DIR=<DOWNLOAD_DIR>/jsoncpp-install`,
# I am somehow manipulating the path, this should be secluded from
# everything else, but I am observing this until I make sure that
# it works.
#
externalproject_add(
  jsoncpp
  PREFIX _deps/jsoncpp
  # LOG_CONFIGURE ON
  # LOG_BUILD ON
  # LOG_INSTALL ON
  # LOG_OUTPUT_ON_FAILURE ON
  GIT_REPOSITORY "https://github.com/open-source-parsers/jsoncpp.git"
  GIT_TAG "1.9.5"
  STEP_TARGETS configure build install
  CMAKE_ARGS -DJSONCPP_WITH_TESTS=OFF
             -DJSONCPP_WITH_POST_BUILD_UNITTEST=OFF
             -DJSONCPP_WITH_CMAKE_PACKAGE=OFF
             -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_INSTALL_PREFIX=<DOWNLOAD_DIR>/jsoncpp-install
             -DCMAKE_BINARY_DIR=<DOWNLOAD_DIR>/jsoncpp-install)

externalproject_get_property(jsoncpp DOWNLOAD_DIR)
set(jsoncpp_DOWNLOAD_DIR ${DOWNLOAD_DIR})
set(jsoncpp_INCLUDE_DIRS ${jsoncpp_DOWNLOAD_DIR}/jsoncpp-install/include)
set(jsoncpp_LIBRARY_DIRS ${jsoncpp_DOWNLOAD_DIR}/jsoncpp-install/lib)

# ----- readstat -----

externalproject_add(
  readstat
  PREFIX _deps/readstat
  # LOG_CONFIGURE ON
  # LOG_BUILD ON
  # LOG_INSTALL ON
  # LOG_OUTPUT_ON_FAILURE ON
  GIT_REPOSITORY "https://github.com/WizardMac/ReadStat"
  GIT_TAG "v1.1.7"
  BUILD_IN_SOURCE ON
  STEP_TARGETS configure build install
  CONFIGURE_COMMAND ${AUTORECONF} -fi
  COMMAND ${AUTOUPDATE}
  COMMAND ./configure --enable-static --prefix=<DOWNLOAD_DIR>/readstat-install
  BUILD_COMMAND ${MAKE}
  INSTALL_COMMAND ${MAKE} install)

externalproject_get_property(readstat DOWNLOAD_DIR)
set(readstat_DOWNLOAD_DIR ${DOWNLOAD_DIR})
set(readstat_BUILD_DIR ${readstat_DOWNLOAD_DIR}/readstat-build)
set(readstat_INCLUDE_DIRS ${readstat_DOWNLOAD_DIR}/readstat-install/include)
set(readstat_LIBRARY_DIRS ${readstat_DOWNLOAD_DIR}/readstat-install/lib)
