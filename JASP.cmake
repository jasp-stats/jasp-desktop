cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT JASP)

set(JASP_REQUIRED_FILES ${CMAKE_SOURCE_DIR}/../jasp-required-files)

# TODO: Find this version number automatically
set(CURRENT_R_VERSION "4.1")

set(GIT_EXEC "git")

message(CHECK_START "Retrieving the git-branch information")
execute_process(
  COMMAND ${GIT_EXEC} rev-parse --abbrev-ref HEAD
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(
  COMMAND ${GIT_EXEC} rev-parse --verify HEAD
  OUTPUT_VARIABLE GIT_COMMIT
  OUTPUT_STRIP_TRAILING_WHITESPACE)
message(CHECK_PASS "done.")

set(GIT_CURRENT_BRANCH GIT_BRANCH)
set(GIT_CURRENT_COMMIT GIT_COMMIT)

# We can define the JASP project version in CMakeLists.txt, and then
# convert it to what we need, or just use them directly.
#
# The last two doesn't look very semver-y and we might want to just
# change them to PATCH and TWEAK anyway.
#
# Note:
#   I think for pushing these into the source, we should probably use
#   a #define structure and rely on make/cmake to handle this. I think
#   qmake does something similar but it looks a bit convoluted.
set(JASP_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(JASP_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(JASP_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(JASP_VERSION_TWEAK ${PROJECT_VERSION_TWEAK})

# Amir: We probably won't need them soon
# option(JASP_LIBJSON_STATIC
#        "Whether or not we are using the 'libjson' as static library?" OFF)

option(PRINT_ENGINE_MESSAGES "Whether or not JASPEngine prints log messages"
       OFF)

option(BUILD_WITH_SYSTEM_R "Build JASP using the system R" OFF)
option(BUILD_MACOSX_BUNDLE "Whether or not building a macOS Bundle" OFF)

# This is being set using the `Sys.setenv()` and later on when
# we install a module using `{renv}`, the `{credentials}` package
# knows how to read and use it.
set(GITHUB_PAT CACHE STRING "GitHub Personal Access Token")
if(GITHUB_PAT)
  message(STATUS "GITHUB_PAT is set to ${GITHUB_PAT}")
endif()

if(NOT R_REPOSITORY)
  set(R_REPOSITORY
      "http://cran.r-project.org"
      CACHE STRING "The CRAN mirror used by 'renv' and 'install.packages'")
endif()
message(STATUS "CRAN mirror: ${R_REPOSITORY}")

option(BUILDING_JASP "Indicates whether we are building JASP or not.
					  This helps jaspResults to find its lib_json." ON)
if(BUILDING_JASP)
  add_definitions(-DBUILDING_JASP)
endif()

option(JASP_DEBUG "Toggle the debug flag" ON)
if(JASP_DEBUG)
  add_definitions(-DJASP_DEBUG)
endif()

option(PRINT_ENGINE_MESSAGES
       "Indicates whether the log contains JASPEngine messages" ON)
if(PRINT_ENGINE_MESSAGES)
  add_definitions(-DPRINT_ENGINE_MESSAGES)
endif()

option(JASP_USES_QT_HERE "Indicates whether some files are using Qt.
						  This doesn't strike as a very informative name
						  for an option!" ON)
if(JASP_USES_QT_HERE)
  add_definitions(-DJASP_USES_QT_HERE)
endif()

# add_definitions(-DJASP_RESULTS_DEBUG_TRACES)

option(JASPTIMER_USED "Use JASP timer for profiling" OFF)
if(JASPTIMER_USED)
  add_definitions(-DUSE_JASP_TIMER)
endif()

# TODO:
# - [ ] Make sure that all variables from .pri and .pro make it to the CMake files
# - [ ] Find the Git location, I think I can use CMake's $ENV{GIT} or something like that
# - [ ] Find a better name for some of these variables
# - [ ] Setup the GITHUB_PAT

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
