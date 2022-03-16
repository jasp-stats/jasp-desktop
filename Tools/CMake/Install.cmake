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

list(APPEND CMAKE_MESSAGE_CONTEXT Install)

# At the moment, I don't remove the `.a` files
set(FILES_EXCLUDE_PATTERN
    ".*(\\.bib|\\.Rnw|\\.cpp|\\.c|\\.pdf|\\.html|\\.f|\\.dSYM|\\.log|\\.bak|\\.deb|\\.DS_Store)$"
)
set(FOLDERS_EXCLUDE_PATTERN
    ".*(/doc|/examples|/help|/man|/html|/i386|/bib|/gfortran|/BH|/announce|/test|/tinytest|/tests)$"
)

# See here, http://cmake.org/cmake/help/v3.22/variable/CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT.html
# I'm still experimenting with the install sequence, I would like to have a
# staging installation without messing up users' preference if it's defined
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
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

  install(FILES "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns"
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

  
  if(MSVC AND (CMAKE_BUILD_TYPE STREQUAL "Debug"))
    set(CMAKE_INSTALL_DEBUG_LIBRARIES ON)
  endif()
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_DESTINATION "${JASP_INSTALL_PREFIX}")

  include (InstallRequiredSystemLibraries)
  install (PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
           DESTINATION .)

  install(TARGETS JASP JASPEngine
    RUNTIME_DEPENDENCY_SET JASP_DEPENDENCIES
     RUNTIME DESTINATION .
    )
  message(STATUS ${JASP_DEPENDENCIES})

  set(JASP_QML_FILES "${CMAKE_SOURCE_DIR}/Desktop")
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(WINDEPLOY_QT_BUILD_TYPE "--debug")
  elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(WINDEPLOY_QT_BUILD_TYPE "--release")
  endif()
  configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/Deploy.win.cmake.in
                 ${CMAKE_BINARY_DIR}/Deploy.win.cmake @ONLY)

  cmake_path(NATIVE_PATH JASP_SOURCE_DIR NORMALIZE JASP_SOURCE_DIR_NATIVE)
  cmake_path(NATIVE_PATH JASP_BINARY_DIR NORMALIZE JASP_BINARY_DIR_NATIVE)
  cmake_path(NATIVE_PATH JASP_INSTALL_PREFIX NORMALIZE JASP_INSTALL_PREFIX_NATIVE)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/jasp.wxi.in
    ${CMAKE_BINARY_DIR}/jasp.wxi @ONLY)
  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/jasp.wxs
    ${CMAKE_BINARY_DIR}/jasp.wxs @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/WIX.cmd.in
    ${CMAKE_BINARY_DIR}/WIX.cmd @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/CollectJunctions.cmd.in
    ${CMAKE_BINARY_DIR}/CollectJunctions.cmd @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/Tools/RecreateJunctions.cmd.in
    ${CMAKE_BINARY_DIR}/RecreateJunctions.cmd @ONLY)

  # configure_file(${CMAKE_SOURCE_DIR}/Tools/CMake/WIX.cmake.in
  #   ${CMAKE_BINARY_DIR}/WIX.cmake @ONLY)

  install(SCRIPT ${CMAKE_BINARY_DIR}/Deploy.win.cmake)

  # file(GET_RUNTIME_DEPENDENCIES)

  # install(CODE [[
  # file(GET_RUNTIME_DEPENDENCIES
  #   EXECUTABLES JASP.exe JASPEngine.exe
  #   PRE_EXCLUDE_REGEXES
  #   [[api-ms-win-.*]]
  #   [[ext-ms-.*]]
  #   [[kernel32\.dll]]
  #   POST_EXCLUDE_REGEXES
  #   [[*./system32/.*\.dll]]
  #   )
  # ]])

    # install(RUNTIME_DEPENDENCY_SET JASP_DEPENDENCIES
    # DESTINATION .
    # PRE_EXCLUDE_REGEXES
    # [[api-ms-win-.*]]
    # [[ext-ms-.*]]
    # [[kernel32\.dll]]
    # POST_EXCLUDE_REGEXES
    # [[*./system32/.*\.dll]])


  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/R
    DESTINATION .
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(DIRECTORY ${CMAKE_SOURCE_DIR}/Resources/ DESTINATION Resources)

  install(FILES ${CMAKE_SOURCE_DIR}/Desktop/icon.ico DESTINATION .)

  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/Modules/
    DESTINATION Modules
    REGEX ${FILES_EXCLUDE_PATTERN} EXCLUDE
    REGEX ${FOLDERS_EXCLUDE_PATTERN} EXCLUDE)

  install(
    FILES ${MINGW_LIBGCC_S_SEH}
          ${MINGW_LIBSTDCPP}
          ${MINGW_LIBWINPTHREAD}
          ${MINGW_LIB_BOOST_NOWIDE}
          ${MINGW_LIBJSONCPP}
          ${_LIB_R_INTERFACE_SHARED}
    DESTINATION .)

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
