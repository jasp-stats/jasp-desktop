cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT JASP)

set(JASP_REQUIRED_FILES ${CMAKE_SOURCE_DIR}/../jasp-required-files)

# TODO: Find this version number automatically
set(CURRENT_R_VERSION "4.1")

set(GIT_CURRENT_BRANCH "(git rev-parse --abbrev-ref HEAD)")
set(GIT_CURRENT_COMMIT "(git rev-parse --verify HEAD)")
set(JASP_VERSION_BUILD 0)
set(JASP_VERSION_MAJOR 0)
set(JASP_VERSION_MINOR 16)
set(JASP_VERSION_REVISION 0)

# Amir: We probably won't need them soon
set(JASP_LIBJSON_STATIC OFF)
set(PRINT_ENGINE_MESSAGES OFF)

option(BUILD_WITH_SYSTEM_R "Build JASP using the system R" OFF)
option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)
option(BUILD_MACOSX_BUNDLE "Whether or not building a macOS Bundle" OFF)

# This is how we can link to the system R set(BUILD_WITH_SYSTEM_R OFF)

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

option(USE_JASP_TIMER "Use JASP timer for profiling" OFF)
if(USE_JASP_TIMER)
  add_definitions(-DUSE_JASP_TIMER)
endif()

# TODO:
# - [ ] Make sure that all variables from .pri and .pro make it to the CMake files
# - [ ] Find the Git location, I think I can use CMake's $ENV{GIT} or something like that
# - [ ] Find a better name for some of these variables
# - [ ] Setup the GITHUB_PAT

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
