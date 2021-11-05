cmake_minimum_required(VERSION 3.21)

set(JASP_REQUIRED_FILES ${CMAKE_SOURCE_DIR}/../jasp-required-files)

set(CURRENT_R_VERSION "4.1")
set(GIT_CURRENT_BRANCH "(git rev-parse --abbrev-ref HEAD)")
set(GIT_CURRENT_COMMIT "(git rev-parse --verify HEAD)")
set(JASP_VERSION_BUILD 0)
set(JASP_VERSION_MAJOR 0)
set(JASP_VERSION_MINOR 16)
set(JASP_VERSION_REVISION 0)

# Amir: Do we have two variable for one thing?
set(GITHUB_PAT_DEFINE ${GITHUB_PAT_DEF})

# Amir: We probably won't need them soon
set(JASP_LIBJSON_STATIC OFF)
set(PRINT_ENGINE_MESSAGES OFF)

# This is how we can link to the system R
# set(BUILD_WITH_SYSTEM_R OFF)

add_definitions(-DBUILDING_JASP)
add_definitions(-DJASP_DEBUG)
add_definitions(-DPRINT_ENGINE_MESSAGES)


option(USE_JASP_TIMER "Use JASP timer for profiling" OFF)

# TODO:
# - [ ] Find the Git location, I think I can use CMake's $ENV{GIT} or something like that
