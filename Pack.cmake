set(CPACK_PACKAGE_NAME "JASP")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A Fresh Way to Do Statistics")
set(CPACK_PACKAGE_VENDOR "University of Amsterdam")
set(CPACK_PACKAGE_CONTACT "Amir")

set(CPACK_VERBATIM_VARIABLES ON)

# They are set by default, but there are cases where CMake
# gets confused, so, it is better to set them explicitly.
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/COPYING.txt)

set(CPACK_PACKAGE_DIRECTORY ${CPACK_PACKAGE_NAME})

# if(WIN32)
#   set(CPACK_GENERATOR ZIP WIX)
# elseif(APPLE)

# set(CPACK_GENERATOR TGZ productbuild)
set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns")

# ------ Bundle Generator
set(CPACK_BUNDLE_NAME "JASP")
set(CPACK_BUNDLE_APPLE_CERT_APP
    "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)")
set(CPACK_BUNDLE_ICON "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns")
set(CPACK_BUNDLE_PLIST "${CMAKE_BINARY_DIR}/Desktop/Info.plist")

# ------ DMG Generator
set(CPACK_DMG_VOLUME_NAME "JASP")
set(CPACK_DMG_BACKGROUND_IMAGE "${CMAKE_SOURCE_DIR}/Tools/macOS/background.png")

# Note:
#   - `.cpack-ignore` must be properly escaped
file(READ ${CMAKE_CURRENT_LIST_DIR}/.cpack-ignore _cpack_ignore)
string(
  REGEX
  REPLACE "\n"
          ";"
          _cpack_ignore
          ${_cpack_ignore})

set(CPACK_SOURCE_IGNORE_FILES "${_cpack_ignore}")

include(CPack)

cpack_add_component(
  jaspCore
  DISPLAY_NAME JASP
  DESCRIPTION "Essential parts of the JASP program."
  REQUIRED
  # INSTALL_TYPE
  # Full
  # Developer
  # Minimal
)

cpack_add_component_group(
  commonModules
  DISPLAY_NAME "Common Modules"
  DESCRIPTION "JASP Common Modules")

cpack_add_component_group(
  extraModules
  DISPLAY_NAME "Extra Modules"
  DESCRIPTION "JASP Extra Modules")

# cpack_add_install_type(Full)
# cpack_add_install_type(Minimal)
# cpack_add_install_type(Developer DISPLAY_NAME "SDK Development")
