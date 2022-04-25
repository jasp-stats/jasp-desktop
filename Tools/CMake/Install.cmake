# Install.cmake takes care of the installation process.
# The `install` stage is where you specify all the files that you want to ship
# with your software. It's being used as the basis for the `packaging` stage,
# and on Linux it can be used directly to install on `/usr/local/`, etc.
#
# You can run this either by running `cmake --build . --target install`
# from the build/ directory. In addition, you can select the `install` target from
# the Qt Creator Build setting, and that should triggers the install as well.
#
# > Be aware that this creates a folder called "Install" inside your build folder,
#   unless you specify a different path for it using the `-DCMAKE_INSTALL_PREFIX`
#
#
# On macOS,
#   - Here I hoped that CMake can create the `JASP.app` but that was quite problematic
#     since the whole bundle should behave and with R.framework in there, we had no
#     chance; so, instead, I'm setting up a proper directory structure and copy everything
#   - Since `macdeployqt` is a mess, I need to perform some extra cleanup at the end. This
#     is done by `Cleaner.sh` and removes a few debugger artifacts, and left over files
#
# On Linux,
#   - Generally, I don't expect anyone to trigger the install unless it's us trying to
#     install on Flatpak. So, the install script is very bare and I'm trusting Linux to
#     handle most of the heavy lifting.
#
# On Windows,
#   - In addition to the normal installation process, we need to install some of the
#     MinGW libraries. They can all be found with `RTOOLS_*_DLL` pattern.
#   - The install process configure a few Batch files for performing a few tasks, e.g.,
#     creating WIX installer. Those files need to be configured using the NATIVE directory
#     paths. Those `cmake_paths` are doing exactly that.
#   - CMake doesn't copy JASP Modules, `jasp*`, into the install folder, and therefore there
#     is no need to remove the junction inside the staged folder
#
# ------------------------------------------------------------------------------
# Notes
#   - There are quite some overlaps between the install stage in different OSes, but
#     for now, I would like to keep it like this for debugging, when the build is
#     stable I can start removing the redundancies.

list(APPEND CMAKE_MESSAGE_CONTEXT Install)

# Notes
#   - On macOS, gfortran is being installed by CMake, and it's being used during the build,
#     and here we exclude it.
set(FILES_EXCLUDE_PATTERN
    ".*(\\.bib|\\.Rnw|\\.cpp|\\.c|\\.pdf|\\.html|\\.f|\\.dSYM|\\.log|\\.bak|\\.deb|\\.DS_Store|\\.Rhistory)$"
)
set(FOLDERS_EXCLUDE_PATTERN
    ".*(/doc|/examples|/man|/html|/demo|/i386|/bib|/gfortran|/BH|/announce|/test|/tinytest|/tests)$"
)

# See here, http://cmake.org/cmake/help/v3.22/variable/CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT.html
# I'm still experimenting with the install sequence, I would like to have a
# staging installation without messing up users' preference if it's defined
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT AND (NOT LINUX))
  set(CMAKE_INSTALL_PREFIX
      "${CMAKE_BINARY_DIR}/Install"
      CACHE STRING "Directory to install JASP after building" FORCE)
  cmake_print_variables(CMAKE_INSTALL_PREFIX)
endif()

if(APPLE)
  set(MACOS_BUNDLE_NAME JASP)
  set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/${MACOS_BUNDLE_NAME}.app")
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}/Contents/MacOS")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Contents/Resources")
  set(JASP_INSTALL_FRAMEWORKDIR "${JASP_INSTALL_PREFIX}/Contents/Frameworks")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Contents/Modules")
  set(JASP_INSTALL_DOCDIR "${JASP_INSTALL_RESOURCEDIR}")

  install(FILES "${CMAKE_SOURCE_DIR}/Tools/macOS/JASP.icns"
                "${CMAKE_SOURCE_DIR}/Tools/macOS/Document.icns"
          DESTINATION ${JASP_INSTALL_RESOURCEDIR})

  install(
    TARGETS JASP JASPEngine
    RUNTIME DESTINATION ${JASP_INSTALL_BINDIR}
    BUNDLE DESTINATION .)

  install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/
          DESTINATION ${JASP_INSTALL_RESOURCEDIR})

  set(R_FRAMEWORK_INSTALL_PATH ${JASP_INSTALL_FRAMEWORKDIR})

  set(BUNDLE_NAME "${MACOS_BUNDLE_NAME}.app")
  set(BUNDLE_DIRS "${CMAKE_PREFIX_PATH}/lib")

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
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  # I had to do this manually, since `macdeployqt` misses it.
  # See here: https://bugreports.qt.io/browse/QTBUG-100686
  #
  # Feel free to remove it when the bug is fixed
  install(FILES ${_LIB_BROTLICOMMON} DESTINATION ${JASP_INSTALL_FRAMEWORKDIR})

  install(
    DIRECTORY ${MODULES_BINARY_PATH}/
    DESTINATION ${JASP_INSTALL_MODULEDIR}
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(FILES ${CMAKE_BINARY_DIR}/Info.plist
          DESTINATION ${JASP_INSTALL_PREFIX}/Contents)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Cleaner.sh.in
                 ${CMAKE_BINARY_DIR}/Cleaner.sh @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Clean.cmake.in
                 ${CMAKE_BINARY_DIR}/Clean.cmake @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Nightlies.plist.in
                 ${CMAKE_BINARY_DIR}/Nightlies.plist @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Nightlies.sh.in
                 ${CMAKE_BINARY_DIR}/Nightlies.sh @ONLY)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Clean.cmake)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Sign.cmake)

endif()

# ---- Linux / Flatpak

if(LINUX)

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
    BUNDLE DESTINATION .)

  install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/
          DESTINATION ${JASP_INSTALL_RESOURCEDIR})

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

if(WIN32)
  set(JASP_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
  set(JASP_INSTALL_BINDIR "${JASP_INSTALL_PREFIX}")
  set(JASP_INSTALL_RESOURCEDIR "${JASP_INSTALL_PREFIX}/Resources")
  set(JASP_INSTALL_MODULEDIR "${JASP_INSTALL_PREFIX}/Modules")
  set(JASP_INSTALL_DOCDIR "${JASP_INSTALL_RESOURCEDIR}")

  # if(MSVC AND (CMAKE_BUILD_TYPE STREQUAL "Debug"))
  #   set(CMAKE_INSTALL_DEBUG_LIBRARIES ON)
  # endif()
  # set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  # set(CMAKE_INSTALL_SYSTEM_RUNTIME_DESTINATION "${JASP_INSTALL_PREFIX}")

  # include(InstallRequiredSystemLibraries)
  # install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION .)

  install(TARGETS JASP JASPEngine RUNTIME DESTINATION .)

  set(JASP_QML_FILES "${CMAKE_SOURCE_DIR}/Desktop")
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(WINDEPLOY_QT_BUILD_TYPE "--debug")
  elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(WINDEPLOY_QT_BUILD_TYPE "--release")
  endif()
  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Deploy.win.cmake.in
                 ${CMAKE_BINARY_DIR}/Deploy.win.cmake @ONLY)

  cmake_path(
    NATIVE_PATH
    JASP_SOURCE_DIR
    NORMALIZE
    JASP_SOURCE_DIR_NATIVE)
  cmake_path(
    NATIVE_PATH
    JASP_BINARY_DIR
    NORMALIZE
    JASP_BINARY_DIR_NATIVE)
  cmake_path(
    NATIVE_PATH
    JASP_INSTALL_PREFIX
    NORMALIZE
    JASP_INSTALL_DIR_NATIVE)

  cmake_path(
    NATIVE_PATH
    R_HOME_PATH
    NORMALIZE
    R_HOME_PATH_NATIVE)
  cmake_path(
    NATIVE_PATH
    RINSIDE_PATH
    NORMALIZE
    RINSIDE_PATH_NATIVE)
  cmake_path(
    NATIVE_PATH
    RCPP_PATH
    NORMALIZE
    RCPP_PATH_NATIVE)
  cmake_path(
    NATIVE_PATH
    R_BIN_PATH
    NORMALIZE
    R_BIN_PATH_NATIVE)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/JASP.wxi.in
                 ${CMAKE_BINARY_DIR}/JASP.wxi @ONLY)
  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/JASP.wxs
                 ${CMAKE_BINARY_DIR}/JASP.wxs @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/WIX.cmd.in
                 ${CMAKE_BINARY_DIR}/WIX.cmd @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/ZIP.cmd.in
                 ${CMAKE_BINARY_DIR}/ZIP.cmd @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/CollectJunctions.cmd.in
                 ${CMAKE_BINARY_DIR}/CollectJunctions.cmd @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/RecreateJunctions.cmd.in
                 ${CMAKE_BINARY_DIR}/RecreateJunctions.cmd @ONLY)

  execute_process(
    WORKING_DIRECTORY ${JASP_INSTALL_PREFIX}
    COMMAND ${CMAKE_COMMAND} -E remove -f
            "${CMAKE_INSTALL_PREFIX}/junctions-recreated-successfully.log")

  install(SCRIPT ${CMAKE_BINARY_DIR}/Deploy.win.cmake)

  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/R
    DESTINATION .
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/ DESTINATION Resources)

  install(FILES ${CMAKE_SOURCE_DIR}/Desktop/icon.ico DESTINATION .)

  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/Modules/renv-cache
    DESTINATION Modules/
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(
    FILES ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/writeImage.R
          ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/zzzWrappers.R
          ${CMAKE_SOURCE_DIR}/R-Interface/R/workarounds.R
          ${CMAKE_SOURCE_DIR}/R-Interface/R/symlinkTools.R
    DESTINATION Modules/)

  install(
    FILES ${RTOOLS_LIBGCC_S_SEH_DLL}
          ${RTOOLS_LIBSTDCPP_DLL}
          ${RTOOLS_LIBWINPTHREAD_DLL}
          ${RTOOLS_LIB_BOOST_NOWIDE_DLL}
          ${RTOOLS_LIBJSONCPP_DLL}
          ${RTOOLS_LIBREADSTAT_DLL}
          ${RTOOLS_ZLIB_DLL}
          ${RTOOLS_LIBICONV_DLL}
          ${_LIB_R_INTERFACE_DLL}
    DESTINATION .)

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
