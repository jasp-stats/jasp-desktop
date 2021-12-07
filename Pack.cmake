include(CPack)

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
set(CPACK_PACKAGE_ICON)

set(CPACK_PACKAGE_DIRECTORY ${CPACK_PACKAGE_NAME})

if(WIN32)
  set(CPACK_GENERATOR ZIP WIX)
elseif(APPLE)
  set(CPACK_GENERATOR TGZ productbuild)
  set(CPACK_PACKAGE_ICON ${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(CPACK_GENERATOR TGZ RPM)
else()
  set(CPACK_GENERATOR TGZ)
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/.cpack-ignore _cpack_ignore)
string(
  REGEX
  REPLACE "\n"
          ";"
          _cpack_ignore
          ${_cpack_ignore})
set(CPACK_SOURCE_IGNORE_FILES "${_cpack_ignore}")
