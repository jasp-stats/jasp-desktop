cmake_minimum_required(VERSION 3.21)

include(ExternalProject)
include(Tools/cmake/CPM.cmake)

set(CPM_USE_LOCAL_PACKAGES ON)

cpmaddpackage(
  NAME
  Boost
  VERSION
  1.77.0
  OPTIONS
  "BUILD_TESTING OFF"
  GITHUB_REPOSITORY
  "boostorg/boost"
  GIT_TAG
  "boost-1.77.0")

cpmaddpackage(
  NAME
  jsoncpp
  VERSION
  1.9.5
  OPTIONS
  "JSONCPP_WITH_TESTS OFF"
  "JSONCPP_WITH_POST_BUILD_UNITTEST OFF"
  GITHUB_REPOSITORY
  "open-source-parsers/jsoncpp"
  GIT_TAG
  "1.9.5")

cpmaddpackage(
  NAME
  libarchive
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
  zlib
  VERSION
  1.2.11
  GITHUB_REPOSITORY
  "madler/zlib"
  GIT_TAG
  "v1.2.11")

find_program(MAKE NAMES gmake nmake make)
externalproject_add(
  readstat
  PREFIX ExternalProjects/readstat
  # SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/readstat
  GIT_REPOSITORY "https://github.com/WizardMac/ReadStat"
  GIT_TAG "v1.1.7"
  BUILD_IN_SOURCE ON
  LOG_CONFIGURE ON
  LOG_BUILD ON
  LOG_INSTALL ON
  LOG_OUTPUT_ON_FAILURE ON
  CONFIGURE_COMMAND ./autogen.sh && ./configure
                    --prefix=${CMAKE_CURRENT_BINARY_DIR}
  BUILD_COMMAND ${MAKE})
