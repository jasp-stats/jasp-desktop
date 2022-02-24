list(APPEND CMAKE_MESSAGE_CONTEXT Config)

# This looks weird but CMake doesn't like ON/OFF in the if condition,
# especially if it's passed
option(SIGN_AT_BUILD_TIME
       "Whether to sign every library during the configuration and build" ON)

if(APPLE)
  if(${SIGN_AT_BUILD_TIME})
    message(STATUS "Signing everything during the configuration and building.")
    set(IS_SIGNING 1)
  else()
    message(STATUS "Only signing essential libraries and binaries.")
    set(IS_SIGNING 0)
  endif()
endif()

set(CUSTOM_R_PATH
    ""
    CACHE PATH "Path to your custom R installation")

# With this, we can hit up to 90% speed up!
option(USE_CCACHE "Whether to use ccache for build" OFF)
option(RUN_IWYU "Whether to run Include What You Use" OFF)
option(INSTALL_R_FRAMEWORK "Whether to download and prepare R.framework" ON)
option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)
option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  OFF)
option(BUILD_TESTS "Whether to build the test suits" OFF)
option(LINUX_LOCAL_BUILD "Whether we are building inside the Build folder" OFF)
option(FLATPAK_USED "Whether we are building for Flatpak" OFF)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  set(FLATPAK_USED ON)
endif()

if(LINUX_LOCAL_BUILD)
  set(FLATPAK_USED OFF)
  message(STATUS "JASP will be configured for local testing")
  message(
    WARNING
      "In this mode, JASP configures a local R/library; however this cannot be used for installing JASP. If you wish to install JASP (e.g., on Flatpak), you must diabled this flag."
  )
endif()

if(NOT (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux"))
  set(IS_LINUX_LOCAL_BUILD FALSE)
endif()

# I have a construct for this, and Qt often messes things up.
# I will consider turning this off, and letting Qt does it
# when everything else worked properly
option(USE_CONAN "Whether to use CONAN package manager" OFF)
if(WIN32)
  set(USE_CONAN ON)
endif()

if(INSTALL_R_MODULES AND (GITHUB_PAT STREQUAL ""))
  message(
    WARNING
      "You probably need to set the GITHUB_PAT; otherwise CMAKE cannot effectively communicate with GitHub."
  )
endif()

if(CCACHE_EXECUTABLE
   AND USE_CCACHE
   AND (NOT
        CMAKE_GENERATOR
        STREQUAL
        "Xcode"
       ))
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_EXECUTABLE}")
  message(STATUS "Found ccache: ${CCACHE_EXECUTABLE}")
endif()

if(IWYU_EXECUTABLE)
  message(STATUS "Found iwyu: ${IWYU_EXECUTABLE}")
endif()

# To change the binary architecture on Win
if(WIN32)
  set(SYSTEM_TYPE WIN32)
endif()

# In case Qt is not in path
# NEEDS TESTING
# if(WIN32)
#   if(QTDIR
#      OR DEFINED $ENV{QTDIR}
#      OR DEFINED $ENV{QTDIR32}
#      OR DEFINED $ENV{QTDIR64})
#     # Qt path set by user or env var
#   else()
#     set(QTDIR
#         ""
#         CACHE PATH "Path to Qt (e.g. C:/Qt/5.7/msvc2015_64)")
#     message(
#       WARNING
#         "QTDIR variable is missing.  Please set this variable to specify path to Qt (e.g. C:/Qt/5.7/msvc2015_64)"
#     )
#   endif()
# endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
