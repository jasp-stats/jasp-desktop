# Taking care of the installation process. The `install` stage is where you
# specify all the files that you want to ship with your software. It's being
# used as the basis for the `packaging` stage, and on Linux it can be used
# directly to install on `/usr/local/`, etc.
#
# You can run this either by running `make install` or  `cmake --install .`
# from the build/ directory.
#
# - On Linux and Flatpak, we can probably just use this, and don't
# bother with the packaging.
#       - For linux, the only thing that need to do is to set the
#         `CMAKE_INSTALL_PREFIX` correctly, something like `/opt/jasp`
#         and everything will work

# include(GNUInstallDirs)

if(APPLE AND BUILD_MACOSX_BUNDLE)
  set(MACOS_BUNDLE_NAME JASP)
  set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/${MACOS_BUNDLE_NAME}.app")
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}/Contents/MacOS")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Contents/Resources")
  set(JASP_INSTALL_FRAMEWORKDIR "${JASP_INSTALL_PREFIX}/Contents/Frameworks")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Contents/Modules")
  set(JASP_INSTALL_DOCDIR "${JASP_INSTALL_RESOURCEDIR}")
endif()

if(APPLE AND BUILD_MACOSX_BUNDLE)
  install(FILES "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns"
          DESTINATION ${JASP_INSTALL_RESOURCEDIR})

endif()

# This is smart enough to put everything into the right place in
# different systems.
# - [ ] We can make it more granular on Windows and Linux later.
install(
  TARGETS JASP JASPEngine
  RUNTIME DESTINATION ${JASP_INSTALL_BINDIR}
  BUNDLE DESTINATION . COMPONENT jaspCore)

# We need to handle the JASPEngine a bit differenly because it is different
# install(
#   TARGETS JASPEngine
#   RUNTIME DESTINATION ${JASP_INSTALL_BINDIR}/MacOS
#   LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#   ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#   BUNDLE DESTINATION . COMPONENT jaspCore)

# At the moment, I don't remove the `.a` files
set(FILES_EXCLUDE_PATTERN
    ".*(\\.bib|\\.Rnw|\\.cpp|\\.c|\\.pdf|\\.html|\\.f|\\.dSYM|\\.log|\\.bak)$")
set(FOLDERS_EXCLUDE_PATTERN
    ".*(/doc|/examples|/help|/man|/html|/bib|/announce|/test|/tinytest|/tests)$"
)

install(
  DIRECTORY ${_R_Framework}
  DESTINATION ${JASP_INSTALL_FRAMEWORKDIR}
  COMPONENT jaspCore
  REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
  REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

# install(IMPORTED_RUNTIME_ARTIFACTS ${_R_Framework}
#   FRAMEWORK DESTINATION ${CMAKE_INSTALL_PREFIX}/Frameworks
#   BUNDLE DESTINATION JASP.app/Contents/Frameworks)

install(
  DIRECTORY ${MODULES_BINARY_PATH}/
  DESTINATION ${JASP_INSTALL_MODULEDIR}
  REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
  REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/Resources
  DESTINATION ${JASP_INSTALL_RESOURCEDIR}
  COMPONENT jaspCore)

if(APPLE AND BUILD_MACOSX_BUNDLE)
  set(R_FRAMEWORK_INSTALL_PATH ${JASP_INSTALL_FRAMEWORKDIR})

  set(BUNDLE_NAME "${MACOS_BUNDLE_NAME}.app")
  set(BUNDLE_DIRS "${CMAKE_PREFIX_PATH}/lib")
  set(APPLE_CODESIGN_IDENTITY
      "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)")
  set(APPLE_CODESIGN_ENTITLEMENTS
      "${CMAKE_SOURCE_DIR}/Tools/macOS/entitlements.plist")

  configure_file(${CMAKE_SOURCE_DIR}/Sign.cmake.in
                 ${CMAKE_BINARY_DIR}/Sign.cmake @ONLY)
  install(SCRIPT ${CMAKE_BINARY_DIR}/Sign.cmake)
endif()

# install(TARGETS ${_R_Framework}
#   FRAMEWORK DESTINATION )
#
# Deployment on macOS
#
find_program(DEPLOYQT_EXECUTABLE macdeployqt)

set(DEPLOY_OPTIONS
    [[JASP
    -verbose=2
    -codesign="Developer ID Application: Bruno Boutin (AWJJ3YVK9B)"
]])

configure_file(Deploy.cmake.in Deploy.cmake @ONLY)
# install(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/Deploy.cmake)

# ---- Windows

# Essential on WIN32 as some binaries should be around
if(WIN32)
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_COMPONENT JASP JASPEngine)
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_DESTINATION "${JASP_INSTALL_BINDIR}")
  if(MSVC AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_INSTALL_DEBUG_LIBRARIES true)
  endif()
  include(InstallRequiredSystemLibraries)
endif()
