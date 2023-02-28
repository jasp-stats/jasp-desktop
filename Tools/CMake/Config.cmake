# Config.cmake contains several CMake variables that are being used
# for configuring the JASP project. In addition, it tries to set
# other necessary variables based on users input, or deduct them when
# possible.
#
# On Linux,
#   - `CUSTOM_R_PATH` can be used to setup the CMake project to a custom
#     R installation instead of the one find in PATH
#   - `LINUX_LOCAL_BUILD` indicates whether or not, R packages should be
#     located in the build folder, or the R_HOME. This is useful when you
#     don't want to pollute your local R installation with JASP's build
#     artifacts
#   - `FLATPAK_USED` indicates whether we are building on Flatpak
#     - [ ] This should probably be called `FLATPAK_BUILD` instead.
#
# On macOS,
#   - You can specifically choose to sign, `SIGN_AT_BUILD_TIME`, and timestamp
#     your binaries during the build, `TIMESTAMP_AT_BUILD_TIME`.
#     - Be aware that often you don't have any other choice, and if you don't
#       sign your libraries, the build cannot continue because macOS wouldn't
#       allow your binaries to be called, or be executed.
#
# On Windows,
#   - Generally, I think we should be using more environment variable, and CMake
#     might have some helper tools, but I didn't look into it yet,
#     - This is especially important on GitHub Action where the pathing might not
#       be trivial. 

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
option(INSTALL_R_MODULES "Whether or not installing R Modules" ON)
option(BUILD_TESTS "Whether to build the test suits" OFF)
option(USE_CONAN "Whether to use CONAN package manager" OFF)

# ------------

if(APPLE)

  set(USE_CONAN ON)

  option(SIGN_AT_BUILD_TIME
         "Whether to sign every library during the configuration and build" ON)
  option(
    TIMESTAMP_AT_BUILD_TIME
    "Whether to add the timstamp every library during the configuration and build"
    ON)

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
    set(DARWIN_ARCH "x86_64")
    set(CPACK_ARCH_SUFFIX "x86_64")
    set(ARCH_LABEL "Intel")
  elseif(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
    set(CPACK_ARCH_SUFFIX "arm64")
    set(ARCH_LABEL "Apple Silicon")
    set(DARWIN_ARCH "aarch64")
  else()
    set(CPACK_ARCH_SUFFIX ${CMAKE_OSX_ARCHITECTURES})
  endif()

  # Getting the major version of Xcode
  # There is already a variable called XCODE_VERSION but that is only
  # set when Xcode generator is used, so I had to get this myself
  execute_process(
    ERROR_QUIET
    COMMAND xcodebuild -version
    OUTPUT_VARIABLE XCODEBUILD_OUTPUT
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  if(NOT (XCODEBUILD_OUTPUT STREQUAL ""))
    string(
      SUBSTRING ${XCODEBUILD_OUTPUT}
                6
                2
                XCODE_VERSION)
  else()
    set(XCODE_VERSION "")
  endif()

  if(XCODE_VERSION STREQUAL "")
    message(
      WARNING
        "Please set the Xcode version using XCODE_VERSION to be able to notarise your app, e.g., 13.2.1"
    )
  else()
    message(STATUS "Xcode version: ${XCODE_VERSION}")
  endif()

  execute_process(
    COMMAND uname -r
    RESULT_VARIABLE result
    OUTPUT_VARIABLE DARWIN_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  set(CONFIGURE_HOST_FLAG
      ${CMAKE_OSX_ARCHITECTURES}-apple-darwin${DARWIN_VERSION})
  message(STATUS "  ${CONFIGURE_HOST_FLAG}")

endif()

if(WIN32)

  set(USE_CONAN ON)
  set(SYSTEM_TYPE WIN32)

  message(STATUS ${MSVC_TOOLSET_VERSION})
  message(STATUS ${MSVC_VERSION})

  if(MSVC_VERSION GREATER "1930")
    set(VC_MERGE_MODULE_NAME
        "Microsoft_VC143_CRT_x64.msm"
        CACHE STRING "Module Merge Name")
    set(VC_TOOLS_REDIST_DIR_VARIABLE "%VCINSTALLDIR%")
    set(VC_TOOLS_REDIST_PATH
        "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Redist\\MSVC\\v143"
    )
    set(VC_VARS_PATH_NATIVE
        "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Auxiliary\\Build"
    )
  elseif(MSVC_VERSION GREATER "1920")
    set(VC_MERGE_MODULE_NAME
        "Microsoft_VC142_CRT_x64.msm"
        CACHE STRING "Module Merge Name")
    set(VC_TOOLS_REDIST_DIR_VARIABLE "%VCToolsRedistDir%")
    set(VC_TOOLS_REDIST_PATH
        "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Redist\\MSVC\\v142"
    )
    set(VC_VARS_PATH_NATIVE
        "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Auxiliary\\Build"
    )
  endif()

  set(VC_MERGE_MODULE_PATH_NATIVE
      "${VC_TOOLS_REDIST_PATH}/MergeModules/${VC_MERGE_MODULE_NAME}"
      CACHE PATH "Merge Module Path")

  message(CHECK_START
          "Looking for VC Merge Module, ${VC_MERGE_MODULE_PATH_NATIVE}")

  if(EXISTS ${VC_MERGE_MODULE_PATH_NATIVE})

    message(CHECK_PASS "found.")
    message(STATUS "  ${VC_MERGE_MODULE_PATH_NATIVE}")

    execute_process(
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${VC_MERGE_MODULE_PATH_NATIVE}" "${CMAKE_BINARY_DIR}")

  else()

    message(CHECK_FAIL "not found.")
    message(FATAL_ERROR "${VC_MERGE_MODULE_NAME} cannot be found.")

  endif()

endif()

if(LINUX)

  set(CUSTOM_R_PATH
      ""
      CACHE PATH "Path to your custom R installation")

  option(LINUX_LOCAL_BUILD "Whether we are building inside the Build folder" ON)

  option(FLATPAK_USED "Whether we are building for Flatpak" OFF)
  if(FLATPAK_USED)
    set(LINUX_LOCAL_BUILD OFF)
    set(IS_LINUX_LOCAL_BUILD FALSE)
    set(IS_FLATPAK_USED TRUE)
  else()
      set(IS_FLATPAK_USED FALSE)
  endif()

  # IS_LINUX_LOCAL_BUILD is a special variable that will be used in install-module.R
  # and it is suppose to store R's TRUE or FALSE
  # set(IS_LINUX_LOCAL_BUILD TRUE)

  if(LINUX_LOCAL_BUILD)

    set(IS_LINUX_LOCAL_BUILD TRUE)

    message(STATUS "JASP will be configured for local testing")
    message(
      WARNING
        "In this mode, JASP configures a local R/library; however this cannot be used for installing JASP. If you wish to install JASP (e.g., on Flatpak), you must diabled this flag."
    )

  else()

    set(IS_LINUX_LOCAL_BUILD FALSE)

  endif()

else()

  set(IS_LINUX_LOCAL_BUILD FALSE)
  set(IS_FLATPAK_USED FALSE)
endif()

# I have a construct for this, and Qt often messes things up.
# I will consider turning this off, and letting Qt does it
# when everything else worked properly

set(GITHUB_PAT     "" CACHE STRING "GitHub Personal Access Token to use during building")
set(GITHUB_PAT_DEF "" CACHE STRING "GitHub Personal Access Token to use in released version as default")


message(CHECK_START "Looking if GITHUB_PAT is set as an environment variable.")
set(GITHUB_PAT      $ENV{GITHUB_PAT})

if(GITHUB_PAT STREQUAL "")
  message(CHECK_FAIL "not found")
  message(
    FATAL_ERROR
      "You probably need to set the GITHUB_PAT; otherwise CMAKE cannot effectively communicate with GitHub. If you are using Qt Creator, you can set a new environment GITHUB_PAT variable in Qt Creator."
  )
endif()
message(CHECK_PASS "found")

message(CHECK_START "Looking if GITHUB_PAT_DEF is set as an environment variable.")
set(GITHUB_PAT_DEF      $ENV{GITHUB_PAT_DEF})

if(GITHUB_PAT_DEF STREQUAL "")
  message(CHECK_FAIL "not found")
  set(GITHUB_PAT_DEF ${GITHUB_PAT})
  message(
    WARNING
      "Your GITHUB_PAT is used as the default PAT for any JASP build with this config, if this is inteded as released software you will want to set GITHUB_PAT_DEF to something else than your personal PAT!"
  )
endif()
message(CHECK_PASS "found")

message(STATUS "GITHUB_PAT:     ${GITHUB_PAT}")
message(STATUS "GITHUB_PAT_DEF: ${GITHUB_PAT_DEF}")

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

# ------ Code signing

if(APPLE)

  set(APPLE_CODESIGN_IDENTITY
      "AWJJ3YVK9B"
      CACHE STRING "Code signing identity")
  set(APPLE_CODESIGN_ENTITLEMENTS
      "${CMAKE_SOURCE_DIR}/Tools/macOS/entitlements.plist")

  message(STATUS "Signing with \"${APPLE_CODESIGN_IDENTITY}\"")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
