# Taking care of the installation of the JASP.
#
# You can run this either by running `make install` or  `cmake --install .`
# from the build/ directory.
#
# - On Linux and Flatpak, we can probably just use this, and don't
# bother with the packaging.

include(GNUInstallDirs)

# - [ ] This needs to be removed. We don't necessary need it, and
#       for now, it is here for testing
set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/Install)
message(STATUS ${CMAKE_INSTALL_PREFIX})

install(
  TARGETS JASP JASPEngine
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# This is not the way to do it, but for now it works, and I'm just
# shuffling things around
install(DIRECTORY ${_R_Framework}
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../Frameworks)

install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../)

install(DIRECTORY ${MODULES_BINARY_PATH}
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../)

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
