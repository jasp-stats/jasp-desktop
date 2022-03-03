list(APPEND CMAKE_MESSAGE_CONTEXT Config)

set_property(GLOBAL PROPERTY JOB_POOLS sequential=1)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  set(LINUX 1)
else()
  set(LINUX 0)
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  set(WINDOWS 1)
else()
  set(WINDOWS 0)
endif()

# With this, we can hit up to 90% speed up!
option(USE_CCACHE "Whether to use ccache for build" OFF)
option(RUN_IWYU "Whether to run Include What You Use" OFF)
option(INSTALL_R_MODULES "Whether or not installing R Modules" OFF)
option(
  INSTALL_JASP_REQUIRED_LIBRARIES
  "Indicates whether CMake should take care of the dependencies like 'Boost', 'jsoncpp', etc."
  ON)
option(BUILD_TESTS "Whether to build the test suits" OFF)
option(USE_CONAN "Whether to use CONAN package manager" OFF)

# ------------

if(APPLE)

  option(SIGN_AT_BUILD_TIME
         "Whether to sign every library during the configuration and build" ON)
  option(
    TIMESTAMP_AT_BUILD_TIME
    "Whether to add the timstamp every library during the configuration and build"
    ON)

  set(INSTALL_JASP_REQUIRED_LIBRARIES ON)

  if(${SIGN_AT_BUILD_TIME})
    message(STATUS "Signing everything during the configuration and building.")
    set(IS_SIGNING 1)
  else()
    message(STATUS "Only signing essential libraries and binaries.")
    set(IS_SIGNING 0)
  endif()

  if(${TIMESTAMP_AT_BUILD_TIME})
    set(CODESIGN_TIMESTAMP_FLAG "--timestamp")
  else()
    set(CODESIGN_TIMESTAMP_FLAG "--timestamp=none")
  endif()

  option(INSTALL_R_FRAMEWORK "Whether to download and prepare R.framework" ON)

  # Later we use this to produce two DMG file, or maybe a Universal one
  # but I'm not yet so sure how that will work
  if(CMAKE_OSX_ARCHITECTURES STREQUAL "x86_64")
    set(CPACK_ARCH_SUFFIX "Intel")
  elseif(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
    set(CPACK_ARCH_SUFFIX "Apple")
  else()
    set(CPACK_ARCH_SUFFIX ${CMAKE_HOST_SYSTEM_PROCESSOR})
  endif()

endif()

if(WIN32)

  set(USE_CONAN ON)
  set(SYSTEM_TYPE WIN32)

endif()

if(LINUX)

  set(CUSTOM_R_PATH
      ""
      CACHE PATH "Path to your custom R installation")

  option(LINUX_LOCAL_BUILD "Whether we are building inside the Build folder"
         OFF)

  option(FLATPAK_USED "Whether we are building for Flatpak" OFF)

  # IS_LINUX_LOCAL_BUILD is a special variable that will be used in install-module.R
  # and it is suppose to store R's TRUE or FALSE
  set(IS_LINUX_LOCAL_BUILD TRUE)

  if(LINUX_LOCAL_BUILD)
    set(FLATPAK_USED OFF)

    message(STATUS "JASP will be configured for local testing")
    message(
      WARNING
        "In this mode, JASP configures a local R/library; however this cannot be used for installing JASP. If you wish to install JASP (e.g., on Flatpak), you must diabled this flag."
    )
  else()
    set(FLATPAK_USED OFF)
  endif()

else()

  set(IS_LINUX_LOCAL_BUILD FALSE)

endif()

# I have a construct for this, and Qt often messes things up.
# I will consider turning this off, and letting Qt does it
# when everything else worked properly

if(INSTALL_R_MODULES AND (GITHUB_PAT STREQUAL ""))
  message(
    FATAL_ERROR
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
