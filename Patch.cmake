# This scripts patches and signs all the libraries found in the PATH variable.
#
# Notes:
#
#   - While this can easily be a CMake Function, or a Macro, it is not,
#     and it should be called using the `cmake -P Patch.cmake`. It is a standalone script
#     because CMake cannot call its function inside a COMMAND section of `execute_process` or
#     `add_custom_targets` and I needed to have that in a few situtation.
#

file(
  GLOB_RECURSE
  LIBRARIES
  "${PATH}/*.so"
  "${PATH}/*.dylib")
list(
  FILTER
  LIBRARIES
  EXCLUDE
  REGEX
  ".*dSYM*")

set(NEW_ID "")

foreach(FILE ${LIBRARIES})

  if(FILE MATCHES "/library/")

    string(
      REPLACE
        "${R_HOME_PATH}/library/"
        "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/library/"
        NEW_ID
        ${FILE})

  elseif(FILE MATCHES "/modules/")

    string(
      REPLACE
        "${R_HOME_PATH}/modules/"
        "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/modules/"
        NEW_ID
        ${FILE})

  elseif(FILE MATCHES "/lib/")

    string(
      REPLACE
        "${R_HOME_PATH}/lib/"
        "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib/"
        NEW_ID
        ${FILE})

  endif()

  execute_process(
    COMMAND_ECHO STDOUT
    WORKING_DIRECTORY ${PATH}
    COMMAND
      bash ${NAME_TOOL_EXECUTABLE} "${FILE}"
      "/Library/Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
      "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
  )

  execute_process(
    COMMAND_ECHO STDOUT
    WORKING_DIRECTORY ${PATH}
    COMMAND
      bash ${NAME_TOOL_EXECUTABLE} "${FILE}" "/opt/R/arm64/lib"
      "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/opt/R/arm64/lib"
  )

  execute_process(
    COMMAND_ECHO STDOUT
    WORKING_DIRECTORY ${PATH}
    COMMAND install_name_tool -id "${NEW_ID}" "${FILE}")

  execute_process(
    COMMAND_ECHO STDOUT
    WORKING_DIRECTORY ${PATH}
    COMMAND codesign --force --sign
            "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)" "${FILE}")

endforeach()
