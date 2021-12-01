
include(GNUInstallDirs)

install(TARGETS JASP JASPEngine RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# install(FRAMEWORK ${_R_Framework})
