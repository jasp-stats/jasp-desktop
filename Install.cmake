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

include(GNUInstallDirs)

# Essential on WIN32 as some binaries should be around
set(CMAKE_INSTALL_SYSTEM_RUNTIME_COMPONENT JASP JASPEngine)
include(InstallRequiredSystemLibraries)

# This is smart enough to put everything into the right place in
# different systems.
# - [ ] We can make it more granular on Windows and Linux later.
install(
  TARGETS JASP R-Interface Common
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  BUNDLE DESTINATION . COMPONENT jaspCore)

# We need to handle the JASPEngine a bit differenly because it is different
install(
  TARGETS JASPEngine
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  BUNDLE DESTINATION JASP.app/Contents/MacOS/ COMPONENT jaspCore)

# At the moment, I don't remove the `.a` files
set(FILES_EXCLUDE_PATTERN
    ".*(\\.bib|\\.Rnw|\\.cpp|\\.c|\\.pdf|\\.html|\\.f|\\.dSYM|\\.log|\\.bak)$")
set(FOLDERS_EXCLUDE_PATTERN
    ".*(/doc|/examples|/help|/man|/html|/bib|/announce|/test|/tinytest|/tests)$"
)

install(
  DIRECTORY ${_R_Framework}
  DESTINATION ${CMAKE_INSTALL_PREFIX}/Frameworks
  COMPONENT jaspCore
  REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
  REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

install(
  DIRECTORY ${MODULES_BINARY_PATH}
  DESTINATION ${CMAKE_INSTALL_PREFIX}
  REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
  REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/Resources
  DESTINATION ${CMAKE_INSTALL_PREFIX}
  COMPONENT jaspCore)
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
