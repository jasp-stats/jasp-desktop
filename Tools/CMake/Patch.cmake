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

# cmake_policy(SET CMP0009 NEW)

if(NOT APPLE)
  message(STATUS "Nothing to fix here.")

else()

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

    message(CHECK_START "-------- Patching ${FILE}")

    if(NOT EXISTS "${DIRECTORY_NAME}/${FILE_NAME}.patched.log")

      # This `if` doesn't look great but it should cover our main patterns.
      # Later on, after we are sure that everything works as expected, we can
      # make it nicer.

      if((FILE MATCHES "prophet.so")
         OR (FILE MATCHES "metaBMA.so")
         OR (FILE MATCHES "libtbbmalloc.dylib")
         OR (FILE MATCHES "libtbbmalloc_proxy.dylib")
         OR (FILE MATCHES "libtbb.dylib"))

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change "@rpath/libtbbmalloc.dylib"
            "@executable_path/../Modules/${MODULE}/RcppParallel/lib/libtbbmalloc.dylib"
            "${FILE}")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change "@rpath/libtbbmalloc_proxy.dylib"
            "@executable_path/../Modules/${MODULE}/RcppParallel/lib/libtbbmalloc_proxy.dylib"
            "${FILE}")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change "@rpath/libtbb.dylib"
            "@executable_path/../Modules/${MODULE}/RcppParallel/lib/libtbb.dylib"
            "${FILE}")

        string(
          REPLACE "${MODULES_BINARY_PATH}/${MODULE}"
                  "@executable_path/../Modules/${MODULE}"
                  NEW_ID
                  ${FILE})

      elseif(FILE MATCHES "/Modules/jasp")

        string(
          REPLACE "${MODULES_BINARY_PATH}/${MODULE}"
                  "@executable_path/../Modules/${MODULE}"
                  NEW_ID
                  ${FILE})

      elseif(FILE MATCHES "/library/")

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

      endif()

      # Changing the `R_HOME/lib/` prefix
      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${PATH}
        COMMAND
          bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}"
          "/Library/Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
      )

      # Changing the `/opt/R/arm64/lib` prefix
      # These are additional libraries needed for arm64.
      # @todo, at some point, we might need to have a case for them, but for now they are fine
      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${PATH}
        COMMAND
          bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}" "/opt/R/arm64/lib"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/opt/R/arm64/lib"
      )

      # Changing the library `id`s
      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${PATH}
        COMMAND install_name_tool -id "${NEW_ID}" "${FILE}")

      file(WRITE ${DIRECTORY_NAME}/${FILE_NAME}.patched.log "")
      message(CHECK_PASS "successful.")

      # Signing the library

      if(${SIGNING})

        set(SIGNING_RESULT "timeout")

        message(CHECK_START "-------- Signing ${FILE}")

        while(${SIGNING_RESULT} STREQUAL "timeout")

          execute_process(
            # COMMAND_ECHO STDOUT
            TIMEOUT 30
            ERROR_QUIET OUTPUT_QUIET
            WORKING_DIRECTORY ${PATH}
            COMMAND
              codesign --force ${CODESIGN_TIMESTAMP_FLAG} --sign
              "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)" "${FILE}"
            RESULT_VARIABLE SIGNING_RESULT
            OUTPUT_VARIABLE SIGNING_OUTPUT)
        endwhile()

        if(NOT (SIGNING_RESULT STREQUAL "timeout"))
          message(CHECK_PASS "signed")
        else()
          message(CHECK_FAIL "failed")
        endif()

      endif()

    else()

      message(CHECK_PASS "already patched.")

    endif()

  endforeach()

endif()
