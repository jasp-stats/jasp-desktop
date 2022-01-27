# This script patches and signs all the libraries found in the PATH variable. In addition,
# for every library that it patches, it creates an empty file, logging its action. So, for
# `R_HOME/lib/libR.dylib`, we will have `R_HOME/lib/libR.dylib.patched.log`. I'm basically
# implementing a simple cache to not patch things several times. This is especially useful
# when we are installing the modules
#
# Notes:
#
#   - While this can easily be a CMake Function, or a Macro, it is not,
#     and it should be called using the `cmake -P Patch.cmake`. It is a standalone script
#     because CMake cannot call its function inside a COMMAND section of `execute_process` or
#     `add_custom_targets` and I needed to have that in a few situtation.
#

cmake_policy(SET CMP0009 NEW)

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

  get_filename_component(FILE_NAME ${FILE} NAME)
  get_filename_component(DIRECTORY_NAME ${FILE} DIRECTORY)

  message(CHECK_START "-------- ${FILE}")

  if(NOT EXISTS "${DIRECTORY_NAME}/${FILE_NAME}.patched.log")

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

    elseif(FILE MATCHES "/opt/jags/")

      string(
        REPLACE
          "${R_HOME_PATH}/opt/jags/"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/opt/jags/"
          NEW_ID
          ${FILE})

    elseif(FILE MATCHES "/lib/")

      string(
        REPLACE
          "${R_HOME_PATH}/lib/"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib/"
          NEW_ID
          ${FILE})

    elseif(FILE MATCHES "/Modules/jasp")

      string(
        REPLACE "${MODULES_BINARY_PATH}/${MODULE}"
                "@executable_path/../Modules/${MODULE}"
                NEW_ID
                ${FILE})

    endif()

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${PATH}
      COMMAND
        bash ${NAME_TOOL_EXECUTABLE} "${FILE}"
        "/Library/Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
        "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
    )

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${PATH}
      COMMAND
        bash ${NAME_TOOL_EXECUTABLE} "${FILE}" "/opt/R/arm64/lib"
        "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/opt/R/arm64/lib"
    )

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${PATH}
      COMMAND install_name_tool -id "${NEW_ID}" "${FILE}")

    execute_process(
      # COMMAND_ECHO STDOUT
      ERROR_QUIET OUTPUT_QUIET
      WORKING_DIRECTORY ${PATH}
      COMMAND codesign --force --sign
              "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)" "${FILE}")

    file(WRITE ${DIRECTORY_NAME}/${FILE_NAME}.patched.log "")
    message(CHECK_PASS "successful.")

  else()

    message(CHECK_PASS "already patched.")

  endif()

endforeach()
