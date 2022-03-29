# Pack.cmake packages the JASP binary
#
# On Windows,
#   - We have two bundler, WIX and ZIP; and they can be called by their target name
#       - `cmake --build . --target wix`
#       - `cmake --build . --target zip`
#   - In addition, there are two targets for collecting and recreating junctions that
#     are being called automatically before the creation of WIX,
#       - `cmake --build . --target recreate-junctions`
#       - `cmake --build . --target collect-junctions`
#
# On macOS,
#   - We are using the `create-dmg` script to create and design the DMG. You need to
#     install it before you can configure JASP. You can download it from Homebrew
#     using `brew install create-dmg`
#       - `cmake --build . --target dmg`
#   - You can also apply the binary for notarisation using the `notarise` command
#       - `cmake --build . --target notarise`
#   - Following the successful notarisation, you can staple the DMG using the `staple` target
#       - `cmake --build . --target staple`
#
list(APPEND CMAKE_MESSAGE_CONTEXT Pack)

set(CPACK_PACKAGE_NAME "JASP")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A Fresh Way to Do Statistics")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://jasp-stats.org")
set(CPACK_PACKAGE_VENDOR "University of Amsterdam")
set(CPACK_PACKAGE_CONTACT "EJ Wagenmakers")
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
set(CPACK_PACKAGE_INSTALL_DIRECTORY ${CPACK_PACKAGE_NAME})
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY ${CPACK_PACKAGE_NAME})

# --- WIX
if(WIN32)
  set(CPACK_GENERATOR "WIX")

  configure_file(${CMAKE_SOURCE_DIR}/Tools/wix/Upload.cmd.in
                 ${CMAKE_BINARY_DIR}/Upload.cmd @ONLY)

  add_custom_target(
    collect-junctions
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    BYPRODUCTS "${CMAKE_BINARY_DIR}/junctions.rds"
    COMMAND cmd.exe /C CollectJunctions.cmd
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_BINARY_DIR}/junctions.rds" "${JASP_INSTALL_PREFIX}/")
		
  set_target_properties(collect-junctions PROPERTIES EXCLUDE_FROM_ALL 1)

  add_custom_target(
    recreate-junctions
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    COMMAND cmd.exe /C RecreateJunctions.cmd)

  set_target_properties(recreate-junctions PROPERTIES EXCLUDE_FROM_ALL 1)

  add_custom_target(
    wix
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    DEPENDS "${CMAKE_BINARY_DIR}/junctions.rds"
    BYPRODUCTS "${CMAKE_SOURCE_DIR}/JASPFilesFragment.wixobj"
               "${CMAKE_SOURCE_DIR}/JASP.wixobj"
               "${CMAKE_SOURCE_DIR}/JASP/JASP.msi"
               "${CMAKE_SOURCE_DIR}/JASP/JASP.wixpdb"
    COMMAND ${CMAKE_COMMAND} -E make_directory JASP
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_BINARY_DIR}/junctions.rds" "${JASP_INSTALL_PREFIX}/"
    COMMAND cmd.exe /C WIX.cmd)

  set_target_properties(wix PROPERTIES EXCLUDE_FROM_ALL 1)

  add_custom_target(
    zip
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    DEPENDS "${CMAKE_BINARY_DIR}/junctions.rds"
    BYPRODUCTS "${CMAKE_SOURCE_DIR}/JASP/JASP.zip"
    COMMAND ${CMAKE_COMMAND} -E make_directory JASP
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_BINARY_DIR}/junctions.rds" "${JASP_INSTALL_PREFIX}/"
    COMMAND cmd.exe /C ZIP.cmd)

  set_target_properties(zip PROPERTIES EXCLUDE_FROM_ALL 1)
  
  add_custom_target(
    upload
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMAND cmd.exe /C Upload.cmd)

  set_target_properties(upload PROPERTIES EXCLUDE_FROM_ALL 1)
  
endif()

set(CPACK_WIX_LICENSE_RTF "${CMAKE_SOURCE_DIR}/Tools/wix/jaspLicense.rtf")
set(CPACK_WIX_PRODUCT_ICON "${CMAKE_SOURCE_DIR}/Desktop/icon.ico")
set(CPACK_WIX_PROPERTY_ARPHELPLINK "${CPACK_PACKAGE_HOMEPAGE_URL}")
set(CPACK_WIX_UI_BANNER "${CMAKE_SOURCE_DIR}/Tools/wix/installerBanner.png")
set(CPACK_WIX_UI_DIALOG "${CMAKE_SOURCE_DIR}/Tools/wix/installerBackground.png")
# set(CPACK_WIX_TEMPLATE "${CMAKE_SOURCE_DIR}/Tools/wix/JASP.wxs")
# set(CPACK_WIX_LIGHT_EXTENSIONS "WixUIExtension;WixUtilExtension")

if(WIN32)
  set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/Desktop/icon.ico")
else()
  set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/Tools/macOS/JASP.icns")
endif()

if(APPLE)
  set(CPACK_PACKAGE_FILE_NAME
      "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-macOS-${CPACK_ARCH_SUFFIX}"
  )
  set(CPACK_DMG_VOLUME_NAME "${CPACK_PACKAGE_FILE_NAME}.dmg")
  set(CPACK_DMG_BACKGROUND_IMAGE
      "${CMAKE_SOURCE_DIR}/Tools/macOS/background.png")

  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Upload.sh.in
                 ${CMAKE_BINARY_DIR}/Upload.sh @ONLY)

  add_custom_target(
    dmg
    VERBATIM
    DEPENDS ${CMAKE_BINARY_DIR}/Install/JASP.app/Contents/MacOS/JASP
    COMMAND
      ${CREATE_DMG_EXECUTABLE} --volname "${CPACK_PACKAGE_FILE_NAME}" --volicon
      "${CMAKE_SOURCE_DIR}/Tools/macOS/Volume.icns" --icon-size 96 --icon
      "JASP.app" 130 270 --background "${CPACK_DMG_BACKGROUND_IMAGE}"
      --window-size 527 454 --window-pos 200 200 --app-drop-link 430 270
      --disk-image-size 4000 "${CPACK_DMG_VOLUME_NAME}" "Install/"
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
  if(XCODE_VERSION VERSION_GREATER 12)
    # % xcrun notarytool store-credentials "AC_PASSWORD"
    #              --apple-id "jasp.stats@gmail.com"
    #              --team-id AWJJ3YVK9B
    #              --password <secret_2FA_password>
    add_custom_target(
      notarise
      COMMAND xcrun notarytool submit "JASP/${CPACK_DMG_VOLUME_NAME}"
              --keychain-profile "AC_PASSWORD"
      COMMENT "Submitting the JASP/${CPACK_DMG_VOLUME_NAME} for notarisation")

  else()
    # % xcrun altool --store-password-in-keychain-item "AC_PASSWORD"
    #            -u "AC_USERNAME"
    #            -p <secret_password>
    add_custom_target(
      notarise
      COMMAND
        xcrun altool --notarize-app --primary-bundle-id "org.jasp-stats.jasp"
        --password "@keychain:AC_PASSWORD" --file
        "JASP/${CPACK_DMG_VOLUME_NAME}"
      COMMENT "Submitting the JASP/${CPACK_DMG_VOLUME_NAME} for notarisation")
  endif()

  add_custom_target(
    staple
    COMMAND xcrun stapler staple "JASP/${CPACK_DMG_VOLUME_NAME}"
    COMMENT "Stapling the JASP/${CPACK_DMG_VOLUME_NAME}")

  add_custom_target(
    upload
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMAND Upload.sh)

endif()

include(CPack)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
