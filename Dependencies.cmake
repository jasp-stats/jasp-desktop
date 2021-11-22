cmake_minimum_required(VERSION 3.21)

include(ExternalProject)
include(Tools/cmake/CPM.cmake)

if(WIN32)
  # That's the gist of it, but I still need to test it on Windows
  find_program(EXTRACT NAMES extract)
  externalproject_add(
    Rexe
    PREFIX Dependencies/R
    URL https://cran.r-project.org/bin/windows/base/R-4.1.2-win.exe
    # URLHASH ""
    DOWNLOAD_NO_EXTRACT ON
    STEP_TARGETS configure build install
    DOWNLOAD_NAME R-4.1.2-win.exe
    CONFIGURE_COMMAND extract /c <DOWNLOAD_DIR>/R-4.1.2-win.exe /l <BINARY_DIR>
    # BUILD_COMMAND ""
    # INSTALL_COMMAND ""
    )
elseif(APPLE)

  # Caveat:
  #   - There is a change that this overwrite the previously downloaded 
  #     and configured R.framework, but I am not sure, for now, I am just
  #     downloading it, and when we decide to use it, we can see if it 
  #     happens, then I can use `FetchContent` to download and configure it
  #     during the Configuration time.
  externalproject_add(
    Rframework
    PREFIX Dependencies/Rframework
    URL https://cran.r-project.org/bin/macosx/base/R-4.1.2.pkg
    # URLHASH ""
    DOWNLOAD_NO_EXTRACT ON
    BUILD_IN_SOURCE ON
    STEP_TARGETS configure build install
    DOWNLOAD_NAME R-4.1.2.pkg
    CONFIGURE_COMMAND xar -xf <DOWNLOAD_DIR>/R-4.1.2.pkg
    BUILD_COMMAND tar -xvf R-fw.pkg/Payload
    # BUILD_COMMAND ""
    INSTALL_COMMAND ""
    # INSTALL_COMMAND cp -r <BINARY_DIR>/R.framework ${CMAKE_SOURCE_DIR}/Frameworks
  )

else()

endif()


set(CPM_USE_LOCAL_PACKAGES ON)

# cpmaddpackage(
#   NAME
#   Boost
#   VERSION
#   1.77.0
#   OPTIONS
#   "BUILD_TESTING OFF"
#   GITHUB_REPOSITORY
#   "boostorg/boost"
#   GIT_TAG
#   "boost-1.77.0")

# cpmaddpackage(
#   NAME
#   jsoncpp
#   VERSION
#   1.9.5
#   OPTIONS
#   "JSONCPP_WITH_TESTS OFF"
#   "JSONCPP_WITH_POST_BUILD_UNITTEST OFF"
#   GITHUB_REPOSITORY
#   "open-source-parsers/jsoncpp"
#   GIT_TAG
#   "1.9.5")

# cpmaddpackage(
#   NAME
#   LibArchive
#   VERSION
#   3.5.2
#   OPTIONS
#   "ENABLE_TEST OFF"
#   "JSONCPP_WITH_POST_BUILD_UNITTEST OFF"
#   GITHUB_REPOSITORY
#   "libarchive/libarchive"
#   GIT_TAG
#   "v3.5.2")

# cpmaddpackage(
#   NAME
#   ZLIB
#   VERSION
#   1.2.11
#   GITHUB_REPOSITORY
#   "madler/zlib"
#   GIT_TAG
#   "v1.2.11")

find_program(MAKE NAMES gmake nmake make)
find_program(ACLOCAL NAMES aclocal)
find_program(AUTOCONF NAMES autoconf)
find_program(AUTORECONF NAMES autoreconf)

# Installing ReadStat
#
externalproject_add(
  readstat
  PREFIX Dependencies/readstat
  GIT_REPOSITORY "https://github.com/WizardMac/ReadStat"
  GIT_TAG "v1.1.7"
  BUILD_IN_SOURCE ON
  # LOG_CONFIGURE ON
  # LOG_BUILD ON
  # LOG_INSTALL ON
  # LOG_OUTPUT_ON_FAILURE ON
  CONFIGURE_COMMAND ./autogen.sh
  COMMAND autoupdate
  COMMAND
    ./configure
    --prefix=${CMAKE_CURRENT_BINARY_DIR}/Dependencies/readstat/src/readstat-install
  BUILD_COMMAND ${MAKE})

externalproject_get_property(readstat SOURCE_DIR)
set(readstat_SOURCE_DIR ${SOURCE_DIR})
set(readstat_BUILD_DIR ${readstat_SOURCE_DIR}/../readstat-build)
set(readstat_INCLUDES_DIR ${readstat_BUILD_DIR}/include)
set(readstat_LIBRARIES_DIR ${readstat_BUILD_DIR}/lib)

# Installing JAGS
#
# - JAGS needs GNU Bison v3, https://www.gnu.org/software/bison.
# - With this, we can build JAGS, and link it, or even place it inside the the `R.framework`
#
externalproject_add(
  jags
  PREFIX Dependencies/jags
  HG_REPOSITORY "http://hg.code.sf.net/p/mcmc-jags/code-0"
  HG_TAG "release-4_3_0"
  BUILD_IN_SOURCE ON
  LOG_CONFIGURE ON
  LOG_BUILD ON
  LOG_INSTALL ON
  LOG_OUTPUT_ON_FAILURE ON
  STEP_TARGETS configure install
  CONFIGURE_COMMAND ${ACLOCAL}
  COMMAND ${AUTORECONF} -fi
  COMMAND
    ./configure --disable-dependency-tracking
    --prefix=${CMAKE_CURRENT_BINARY_DIR}/Dependencies/jags/src/jags-install
  BUILD_COMMAND ${MAKE})

externalproject_get_property(jags SOURCE_DIR)
set(JAGS_SOURCE_DIR ${SOURCE_DIR})
set(JAGS_BUILD_DIR ${JAGS_SOURCE_DIR}/../jags-build)
set(JAGS_INCLUDES_DIR ${JAGS_BUILD_DIR}/include)
set(JAGS_LIBRARIES_DIR ${JAGS_BUILD_DIR}/lib)
