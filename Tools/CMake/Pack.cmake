list(APPEND CMAKE_MESSAGE_CONTEXT Pack)

set(CPACK_PACKAGE_NAME "JASP")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A Fresh Way to Do Statistics")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://jasp-stats.org")
set(CPACK_PACKAGE_VENDOR "University of Amsterdam")
set(CPACK_PACKAGE_CONTACT "EJ Wagenmakers")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "JASP")
set(CPACK_PACKAGE_EXECUTABLES "JASP;JASPEngine")
set(CPACK_CREATE_DESKTOP_LINKS "JASP")

set(CPACK_VERBATIM_VARIABLES ON)

# They are set by default, but there are cases where CMake
# gets confused, so, it is better to set them explicitly.
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})

# set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/COPYING.txt)

set(CPACK_PACKAGE_DIRECTORY ${CPACK_PACKAGE_NAME})

# --- WIX
set(CPACK_WIX_UPGRADE_GUID "")
# set(CPACK_WIX_LICENSE_RTF "${CMAKE_SOURCE_DIR}/Tools/wix/jaspLicense.rtf")
set(CPACK_WIX_PRODUCT_ICON "${CMAKE_SOURCE_DIR}/Desktop/icon.ico")
set(CPACK_WIX_PROPERTY_ARPHELPLINK "${CPACK_PACKAGE_HOMEPAGE_URL}")
set(CPACK_WIX_UI_BANNER "${CMAKE_SOURCE_DIR}/Tools/wix/installerBanner.png")
set(CPACK_WIX_UI_DIALOG "${CMAKE_SOURCE_DIR}/Tools/wix/installerBackground.png")

set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns")

if(APPLE)
  set(CPACK_PACKAGE_FILE_NAME
      "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-macOS-${CPACK_ARCH_SUFFIX}"
  )
  set(CPACK_DMG_VOLUME_NAME "${CPACK_PACKAGE_FILE_NAME}.dmg")
  set(CPACK_DMG_BACKGROUND_IMAGE
      "${CMAKE_SOURCE_DIR}/Tools/macOS/background.png")

  add_custom_target(
    dmg
    VERBATIM
    DEPENDS ${CMAKE_BINARY_DIR}/Install/JASP.app/Contents/MacOS/JASP
    COMMAND
      ${CREATE_DMG_EXECUTABLE} --volname "${CPACK_PACKAGE_FILE_NAME}" --volicon
      "${CPACK_PACKAGE_ICON}" --icon-size 96 --icon "JASP.app" 130 270
      --background "${CPACK_DMG_BACKGROUND_IMAGE}" --window-size 527 454
      --window-pos 200 200 --app-drop-link 430 270 --disk-image-size 4000
      "${CPACK_DMG_VOLUME_NAME}" "Install/"
    COMMAND ${CMAKE_COMMAND} -E make_directory JASP
    COMMAND ${CMAKE_COMMAND} -E copy "${CPACK_DMG_VOLUME_NAME}"
            ${CMAKE_BINARY_DIR}/JASP/
    COMMAND
      codesign --verbose --verify --deep --force --sign
      "${APPLE_CODESIGN_IDENTITY}" --options runtime
      "JASP/${CPACK_DMG_VOLUME_NAME}"
    COMMENT "------ Creating the ${CPACK_DMG_VOLUME_NAME}")

  # Add your password like this to the KeyChain
  #
  # % xcrun notarytool store-credentials "AC_PASSWORD"
  #              --apple-id "jasp.stats@gmail.com"
  #              --team-id AWJJ3YVK9B
  #              --password <secret_2FA_password>
  add_custom_target(
    notarise
    COMMAND xcrun notarytool submit "JASP/${CPACK_DMG_VOLUME_NAME}"
            --keychain-profile "AC_PASSWORD"
    COMMENT "Submitting the JASP/${CPACK_DMG_VOLUME_NAME} for notarisation")

  add_custom_target(staple COMMAND xcrun stapler staple "Install/JASP.app")
endif()

include(CPack)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
