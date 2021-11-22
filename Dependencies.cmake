cmake_minimum_required(VERSION 3.21)

include(ExternalProject)
include(Tools/cmake/CPM.cmake)

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
  LOG_CONFIGURE ON
  LOG_BUILD ON
  LOG_INSTALL ON
  LOG_OUTPUT_ON_FAILURE ON
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
