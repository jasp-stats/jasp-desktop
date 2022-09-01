# This script patches and signs all the libraries found in given PATH. In addition,
# for every library that it patches, it creates an empty file, logging its action. So, for
# `R_HOME/lib/libR.dylib`, we will have `R_HOME/lib/libR.dylib.patched.log`. I'm basically
# implementing a simple cache to not patch things several times. This is especially useful
# when we are installing the modules.
#
# Todos:
#   - [ ] Make the script more robust against the order of patching. I think this can be done
#         with stronger `MATCHES` instruction, but it needs some extensive testing
#   - [ ] Add a variable to control the verbosity of the output
#   - [ ] Some R packages have some binaries with them, e.g., `jags`. Although I noticed that
#         they are not usually liked against anything, and they are often just shell scripts,
#         we should probably check them and patch them if necessary.
#
# Warnings:
#   - The order of statements in this file matters a lot, so, we need to catch the
#     `/opt/R/arm64/gfortran/lib/` before the generic `lib/`; so, be aware of this
#     when you are attempting to modify this.
#   - When calling this script make sure that you are passing the parameters correctly.
#     See, `Modules.cmake` or `R.cmake` for examples.
#
# Notes:
#
#   - While this can easily be a CMake Function, or a Macro, it is not,
#     and it should be called using the `cmake -P Patch.cmake`. It is a standalone script
#     because CMake cannot call its function inside a COMMAND section of `execute_process` or
#     `add_custom_targets` and I needed to have that in a few situation.
#

cmake_policy(SET CMP0009 NEW)

if(NOT APPLE)
  message(STATUS "Nothing to fix here.")

else()

  file(
    GLOB_RECURSE
    LIBRARIES
    FOLLOW_SYMLINKS
    "${PATH}/*.so"
    "${PATH}/*.dylib")
  list(
    FILTER
    LIBRARIES
    EXCLUDE
    REGEX
    ".*dSYM*")

  # file(GLOB BINARIES "${PATH}/*/bin/*")
  # list(
  #   FILTER
  #   "${BINARIES}"
  #   EXCLUDE
  #   REGEX
  #   ".*\\.patched\\.log.*")

  # message(STATUS "HERE ARE BINARIES:\n ${BINARIES}")

  set(FILES "")
  list(
    APPEND
    FILES
    ${LIBRARIES}
    ${BINARIES})

  set(NEW_ID "")
  set(FRAMEWORK_RESOURCES "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources")

  foreach(FILE ${FILES})

    get_filename_component(FILE_NAME ${FILE} NAME)
    get_filename_component(DIRECTORY_NAME ${FILE} DIRECTORY)

    string(LENGTH "${PATH}/" PATH_LENGTH)
    string(
      SUBSTRING "${FILE}"
                "${PATH_LENGTH}"
                "-1"
                FILE_SHORT_PATH)

    message(CHECK_START "--- Patching ${FILE_SHORT_PATH}")

    if(NOT EXISTS "${DIRECTORY_NAME}/${FILE_NAME}.patched.log")

      # This `if` doesn't look great but it should cover our main patterns.
      # Later on, after we are sure that everything works as expected, we can
      # make it nicer.

      if((FILE MATCHES "prophet.so")
         OR (FILE MATCHES "metaBMA.so")
         OR (FILE MATCHES "rstanarm.so")
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

      elseif(FILE MATCHES "/opt/R/arm64/gfortran/lib/")

        string(
          REPLACE
            "${R_HOME_PATH}/opt/R/arm64/gfortran/lib/"
            "${FRAMEWORK_RESOURCES}/opt/R/arm64/gfortran/lib/"
            NEW_ID
            ${FILE})

      elseif(FILE MATCHES "/opt/R/arm64/lib/")

        string(
          REPLACE
            "${R_HOME_PATH}/opt/R/arm64/lib/"
            "${FRAMEWORK_RESOURCES}/opt/R/arm64/lib/"
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
            "${FRAMEWORK_RESOURCES}/library/"
            NEW_ID
            ${FILE})

      elseif(FILE MATCHES "/modules/")

        string(
          REPLACE
            "${R_HOME_PATH}/modules/"
            "${FRAMEWORK_RESOURCES}/modules/"
            NEW_ID
            ${FILE})

      elseif(FILE MATCHES "/opt/jags/")

        string(
          REPLACE
            "${R_HOME_PATH}/opt/jags/"
            "${FRAMEWORK_RESOURCES}/opt/jags/"
            NEW_ID
            ${FILE})

      elseif(FILE MATCHES "/usr/local/lib/libjags")

        string(
          REPLACE
            "/usr/local/lib/"
            "${FRAMEWORK_RESOURCES}/opt/jags/"
            NEW_ID
            ${FILE})

       elseif(FILE MATCHES "/usr/local/lib/JAGS")

        string(
          REPLACE
            "/usr/local/lib/JAGS"
            "${FRAMEWORK_RESOURCES}/opt/jags/lib/JAGS"
            NEW_ID
            ${FILE})

      elseif(FILE MATCHES "/lib/")

        string(
          REPLACE
            "${R_HOME_PATH}/lib/"
            "${FRAMEWORK_RESOURCES}/lib/"
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
          "${FRAMEWORK_RESOURCES}/lib"
      )

      # Changing the `R_HOME/opt/jags/lib` prefix
      # This only applies on .so inside the /opt/jags/lib/JAGS/modules-4/
      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${PATH}
        COMMAND
          bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}"
          "${R_HOME_PATH}/opt/jags/lib"
          "${FRAMEWORK_RESOURCES}/opt/jags/lib"
      )

      if(R_DIR_NAME MATCHES "arm64")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change
            "${R_HOME_PATH}/opt/R/arm64/gfortran/lib/libgfortran.dylib"
            "${FRAMEWORK_RESOURCES}/lib/libgfortran.5.dylib"
            "${FILE}")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}"
            "/opt/R/arm64/gfortran/lib"
            "${FRAMEWORK_RESOURCES}/opt/R/arm64/gfortran/lib"
        )

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}"
            "${R_HOME_PATH}/opt/R/arm64/gfortran/lib"
            "${FRAMEWORK_RESOURCES}/opt/R/arm64/gfortran/lib"
        )

      else()

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change
            "${R_HOME_PATH}/opt/local/gfortran/lib/libgfortran.dylib"
            "${FRAMEWORK_RESOURCES}/lib/libgfortran.5.dylib"
            "${FILE}")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            install_name_tool -change
            "${R_HOME_PATH}/opt/local/gfortran/lib/libquadmath.dylib"
            "${FRAMEWORK_RESOURCES}/lib/libquadmath.0.dylib"
            "${FILE}")

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}"
            "/usr/local/gfortran/lib"
            "${FRAMEWORK_RESOURCES}/opt/local/gfortran/lib"
        )
        # For whatever reason, the above command cannot replace the prefix of these libraries. I have tried to
        # directly changed their 'id' even, and that didn't help either!
        #   - libgcc_ext.10.4.dylib
        #   - libgcc_ext.10.5.dylib

        if(NOT (FILE MATCHES ".*(runjags|rjags|RoBMA|metaBMA).*"))

          execute_process(
            # COMMAND_ECHO STDOUT
            ERROR_QUIET OUTPUT_QUIET
            WORKING_DIRECTORY ${PATH}
            COMMAND
              bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}" "/usr/local/lib"
              "${FRAMEWORK_RESOURCES}/opt/local/lib"
          )

        endif()

      endif()

      # Changing the `/opt/R/arm64/lib` prefix
      # These are additional libraries needed for arm64.
      # @todo, at some point, we might need to have a case for them, but for now they are fine
      if(NOT (FILE MATCHES ".*(runjags|rjags|RoBMA|metaBMA).*"))

        execute_process(
          # COMMAND_ECHO STDOUT
          ERROR_QUIET OUTPUT_QUIET
          WORKING_DIRECTORY ${PATH}
          COMMAND
            bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}" "/opt/R/arm64/lib"
            "${FRAMEWORK_RESOURCES}/opt/R/arm64/lib"
        )

      else()

      # Changing the `/usr/local/lib` prefix
      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${PATH}
        COMMAND
          bash ${NAME_TOOL_PREFIX_PATCHER} "${FILE}" "/usr/local/lib"
          "${FRAMEWORK_RESOURCES}/opt/jags/lib"
      )

      endif()

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
        set(APPLE_CODESIGN_IDENTITY "${SIGNING_IDENTITY}")

        set(SIGNING_RESULT "timeout")

        message(CHECK_START "--- Signing  ${FILE_SHORT_PATH}")

        while(${SIGNING_RESULT} MATCHES "timeout")

          execute_process(
            # COMMAND_ECHO STDOUT
            ERROR_QUIET OUTPUT_QUIET
            TIMEOUT 30
            WORKING_DIRECTORY ${PATH}
            COMMAND codesign --deep --force ${CODESIGN_TIMESTAMP_FLAG} --sign
                    ${APPLE_CODESIGN_IDENTITY} --options runtime "${FILE}"
            RESULT_VARIABLE SIGNING_RESULT
            OUTPUT_VARIABLE SIGNING_OUTPUT)
        endwhile()

        if(SIGNING_RESULT STREQUAL "0")
          message(CHECK_PASS "successful")
        else()
          message(CHECK_FAIL "unsuccessful")
          message(
            WARNING
              "Signing of ${FILE_SHORT_PATH} was NOT successful. This could break your build!"
          )
        endif()

      endif()

    else()

      message(CHECK_PASS "already patched.")

    endif()

  endforeach()

endif()
