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

# At the moment, I don't remove the `.a` files
set(FILES_EXCLUDE_PATTERN
    ".*(\\.bib|\\.Rnw|\\.cpp|\\.c|\\.pdf|\\.html|\\.f|\\.dSYM|\\.log|\\.bak)$")
set(FOLDERS_EXCLUDE_PATTERN
    ".*(/doc|/examples|/help|/man|/html|/bib|/announce|/test|/tinytest|/tests)$"
)

if(APPLE)
  set(MACOS_BUNDLE_NAME JASP)
  set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/${MACOS_BUNDLE_NAME}.app")
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}/Contents/MacOS")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Contents/Resources")
  set(JASP_INSTALL_FRAMEWORKDIR "${JASP_INSTALL_PREFIX}/Contents/Frameworks")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Contents/Modules")
  set(JASP_INSTALL_DOCDIR "${JASP_INSTALL_RESOURCEDIR}")

  install(FILES "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns"
          DESTINATION ${JASP_INSTALL_RESOURCEDIR})

  install(
    TARGETS JASP JASPEngine
    RUNTIME DESTINATION ${JASP_INSTALL_BINDIR}
    BUNDLE DESTINATION . COMPONENT jaspCore)

  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/
    DESTINATION ${JASP_INSTALL_RESOURCEDIR}
    COMPONENT jaspCore)

  set(R_FRAMEWORK_INSTALL_PATH ${JASP_INSTALL_FRAMEWORKDIR})

  set(BUNDLE_NAME "${MACOS_BUNDLE_NAME}.app")
  set(BUNDLE_DIRS "${CMAKE_PREFIX_PATH}/lib")
  set(APPLE_CODESIGN_IDENTITY
      "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)")
  set(APPLE_CODESIGN_ENTITLEMENTS
      "${CMAKE_SOURCE_DIR}/Tools/macOS/entitlements.plist")

  find_program(DEPLOYQT_EXECUTABLE macdeployqt)
  # set(DEPLOYQT_EXECUTABLE "/Users/amabdol/Qt/6.2.2/macos/bin/macdeployqt")
  set(JASP_QML_FILES "${CMAKE_SOURCE_DIR}/Desktop")
  set(PARALLEL_SIGNER "${CMAKE_BINARY_DIR}/ParallelSigner.sh")

  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/ParallelSigner.sh.in
                 ${CMAKE_BINARY_DIR}/ParallelSigner.sh @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Sign.cmake.in
                 ${CMAKE_BINARY_DIR}/Sign.cmake @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Deploy.mac.cmake.in
                 ${CMAKE_BINARY_DIR}/Deploy.mac.cmake @ONLY)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Deploy.mac.cmake)

  install(
    DIRECTORY ${_R_Framework}
    DESTINATION ${JASP_INSTALL_FRAMEWORKDIR}
    COMPONENT jaspCore
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  # I had to do this manually, since `macdeployqt` misses it.
  # See here: https://bugreports.qt.io/browse/QTBUG-100686
  #
  # Feel free to remove it when the bug is fixed
  install(
    FILES ${_LIB_BROTLICOMMON}
    DESTINATION ${JASP_INSTALL_FRAMEWORKDIR}
    COMPONENT jaspCore)

  install(
    DIRECTORY ${MODULES_BINARY_PATH}/
    DESTINATION ${JASP_INSTALL_MODULEDIR}
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(FILES ${CMAKE_BINARY_DIR}/Info.plist
          DESTINATION ${JASP_INSTALL_PREFIX}/Contents)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Sign.cmake)

endif()

# ---- Linux / Flatpak

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  if(EXISTS /app/bin)
    set(JASP_INSTALL_PREFIX "/app")
    message(STATUS "Flatpak environment is detected.")
  else()
    set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
  endif()
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}/bin")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Resources")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Modules")

  install(
    TARGETS JASP JASPEngine
    RUNTIME DESTINATION ${JASP_INSTALL_BINDIR}
    BUNDLE DESTINATION . COMPONENT jaspCore)

  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/
    DESTINATION ${JASP_INSTALL_RESOURCEDIR}
    COMPONENT jaspCore)

  install(DIRECTORY ${MODULES_BINARY_PATH}/
          DESTINATION ${JASP_INSTALL_MODULEDIR})

  install(DIRECTORY ${MODULES_RENV_ROOT_PATH}/
          DESTINATION ${JASP_INSTALL_PREFIX}/lib64/renv-root)

  install(DIRECTORY ${MODULES_RENV_CACHE_PATH}/
          DESTINATION ${JASP_INSTALL_PREFIX}/lib64/renv-cache)

  # Flatpak Misc.

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/org.jaspstats.JASP.desktop
          DESTINATION ${JASP_INSTALL_PREFIX}/share/applications)

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/org.jaspstats.JASP.svg
          DESTINATION ${JASP_INSTALL_PREFIX}/share/icons/hicolor/scalable/apps)

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/org.jaspstats.JASP.svg
          DESTINATION ${JASP_INSTALL_PREFIX}/share/icons/hicolor/scalable/apps)

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/64/org.jaspstats.JASP.png
          DESTINATION ${JASP_INSTALL_PREFIX}/share/icons/hicolor/64x64/apps)

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/128/org.jaspstats.JASP.png
          DESTINATION ${JASP_INSTALL_PREFIX}/share/icons/hicolor/128x128/apps)

  install(FILES ${CMAKE_SOURCE_DIR}/Tools/flatpak/org.jaspstats.JASP.appdata.xml
          DESTINATION ${JASP_INSTALL_PREFIX}/share/metainfo)

endif()

# ---- Windows

# Essential on WIN32 as some binaries should be around
if(WIN32)
  set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/Install")
  set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Resources")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Modules")
  set(JASP_INSTALL_DOCDIR "${JASP_INSTALL_RESOURCEDIR}")
  # set(CMAKE_INSTALL_SYSTEM_RUNTIME_COMPONENT JASP JASPEngine)
  # set(CMAKE_INSTALL_SYSTEM_RUNTIME_DESTINATION "${JASP_INSTALL_BINDIR}")
  # if(MSVC AND CMAKE_BUILD_TYPE STREQUAL "Debug")
  #   set(CMAKE_INSTALL_DEBUG_LIBRARIES true)
  # endif()
  include(InstallRequiredSystemLibraries)

  install(
    TARGETS JASP JASPEngine
    RUNTIME DESTINATION ${JASP_INSTALL_PREFIX}
    COMPONENT jaspCore)

  set(JASP_QML_FILES "${CMAKE_SOURCE_DIR}/Desktop")
  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Deploy.win.cmake.in
                 ${CMAKE_BINARY_DIR}/Deploy.win.cmake @ONLY)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Deploy.win.cmake)

  install(DIRECTORY ${CMAKE_BINARY_DIR}/R
    DESTINATION ${JASP_INSTALL_PREFIX}
    COMPONENT jaspCore)

  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/
    DESTINATION ${JASP_INSTALL_RESOURCEDIR}
    COMPONENT jaspCore)

  install(DIRECTORY ${CMAKE_BINARY_DIR}/Modules/
    DESTINATION ${JASP_INSTALL_MODULEDIR}
    COMPONENT jaspModules)

  install(FILES
    ${MINGW_LIBGCC_S_SEH} 
    ${MINGW_LIBSTDCPP} 
    ${MINGW_LIBWINPTHREAD} 
    ${MINGW_LIBJSONCPP} 
    ${_LIB_R_INTERFACE_SHARED} 
    DESTINATION ${JASP_INSTALL_PREFIX}
    COMPONENT jaspCore)

endif()
