# Taking care of the installation of the JASP.
#
# You can run this either by running `make install` or  `cmake --install .`
# from the build/ directory.
#
# - On Linux and Flatpak, we can probably just use this, and don't
# bother with the packaging.

include(GNUInstallDirs)

install(
  TARGETS JASP JASPEngine
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  BUNDLE DESTINATION ${CMAKE_INSTALL_LIBDIR}/Bundle)

# This is not the way to do it, but for now it works, and I'm just
# shuffling things around
install(DIRECTORY ${_R_Framework}
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../Frameworks)

install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../)

install(DIRECTORY ${MODULES_BINARY_PATH}
        DESTINATION ${CMAKE_INSTALL_BINDIR}/../)
