
include(CPack)

set(CPACK_GENERATOR TGZ)
set(CPACK_SOURCE_GENERATOR TGZ)
set(CPACK_PACKAGE_VENDOR "JASP")
set(CPACK_PACKAGE_CONTACT "Amir")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

file(READ ${CMAKE_CURRENT_LIST_DIR}/.cpack-ignore _cpack_ignore)
string(
  REGEX
  REPLACE "\n"
          ";"
          _cpack_ignore
          ${_cpack_ignore})
set(CPACK_SOURCE_IGNORE_FILES "${_cpack_ignore}")
