cmake_minimum_required(VERSION 3.21)

include(GNUInstallDirs)

install(TARGETS JASP JASPEngine RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# install(FRAMEWORK ${_R_Framework})
