macro(patch_ld_paths)

  cmake_print_variables(r_pkg_r_home)
  cmake_print_variables(r_pkg_SOURCE_DIR)

  file(GLOB_RECURSE SO_LIBRARIES "${r_pkg_r_home}/*.so")
  file(GLOB_RECURSE DYLIB_LIBRARIES "${r_pkg_r_home}/*.dylib")
  list(
    FILTER
    DYLIB_LIBRARIES
    EXCLUDE
    REGEX
    ".*dSYM.*")

  foreach(SO_LIB ${SO_LIBRARIES})

    cmake_path(
      GET
      SO_LIB
      FILENAME
      SO_LIB_FILENAME)
    message(CHECK_START "Patching ${SO_LIB_FILENAME}")

    string(
      REGEX MATCH
            ".*library.*"
            SO_LIB_IN_LIBRARY
            ${SO_LIB})

    if(SO_LIB_IN_LIBRARY)

      get_filename_component(LIB_NAME ${SO_LIB_IN_LIBRARY} NAME)
      message(STATUS "LIB_NAME: " ${LIB_NAME})

      # Basically, we are replacing ${R_HOME} with the @executable_path/../ etc.
      string(
        REPLACE
          "${r_pkg_r_home}/library/"
          "@executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/library/"
          SO_LIB_NEW_ID
          ${SO_LIB_IN_LIBRARY})
      message(STATUS "SO_LIB_NEW_ID: " ${SO_LIB_NEW_ID})

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND
          ./install_name_prefix_tool.sh ${SO_LIB_IN_LIBRARY}
          /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
          @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
      )

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND
          ./install_name_prefix_tool.sh ${SO_LIB_IN_LIBRARY} /opt/R/arm64/lib
          @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/opt/R/arm64/lib
      )

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND install_name_tool -id ${SO_LIB_NEW_ID} ${SO_LIB_IN_LIBRARY})

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND
          codesign --force --verbose --sign
          "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)"
          ${SO_LIB_IN_LIBRARY})

    endif()

    string(
      REGEX MATCH
            ".*module.*"
            SO_LIB_IN_MODULES
            ${SO_LIB})

    if(SO_LIB_IN_MODULES)

      get_filename_component(LIB_NAME ${SO_LIB_IN_MODULES} NAME)
      message(STATUS ${LIB_NAME})

      # Basically, we are replacing ${R_HOME} with the @executable_path/../ etc.
      string(
        REPLACE
          "${r_pkg_r_home}/modules/"
          "@executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/modules/"
          SO_LIB_NEW_ID
          ${SO_LIB_IN_MODULES})
      message(STATUS "SO_LIB_NEW_ID: " ${SO_LIB_NEW_ID})

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND
          ./install_name_prefix_tool.sh ${SO_LIB_IN_MODULES}
          /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
          @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
      )

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND
          ./install_name_prefix_tool.sh ${SO_LIB_IN_MODULES} /opt/R/arm64/lib
          @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/opt/R/arm64/lib
      )

      execute_process(
        WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
        COMMAND install_name_tool -id ${SO_LIB_NEW_ID} ${SO_LIB_IN_MODULES})

    endif()

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        codesign --force --verbose --sign
        "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)"
        ${SO_LIB_IN_MODULES})

    message(CHECK_PASS "successful.\n")
  endforeach()

  foreach(DYLIB_LIB ${DYLIB_LIBRARIES})

    cmake_path(
      GET
      DYLIB_LIB
      FILENAME
      DYLIB_LIB_FILENAME)
    message(CHECK_START "Patching ${DYLIB_LIB_FILENAME}")

    get_filename_component(LIB_NAME ${DYLIB_LIB} NAME)
    message(STATUS "LIB_NAME: " ${LIB_NAME})

    # Basically, we are replacing ${R_HOME} with the @executable_path/../ etc.
    string(
      REPLACE
        "${r_pkg_r_home}/lib/"
        "@executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/"
        DYLIB_LIB_NEW_ID
        ${DYLIB_LIB})
    message(STATUS "SO_LIB_NEW_ID: " ${DYLIB_LIB_NEW_ID})

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        ./install_name_prefix_tool.sh ${DYLIB_LIB}
        /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
        @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
    )

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        ./install_name_prefix_tool.sh ${DYLIB_LIB} /opt/R/arm64/lib
        @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/opt/R/arm64/lib
    )

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND install_name_tool -id ${DYLIB_LIB} ${DYLIB_LIB_NEW_ID})

    execute_process(
      COMMAND
        codesign --force --verbose --sign
        "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)" ${DYLIB_LIB})

    message(CHECK_PASS "successful.\n")
  endforeach()

  set(BINARIES "${r_pkg_r_home}/bin/exec/R")

  foreach(BINARY ${BINARIES})

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        ./install_name_prefix_tool.sh ${BINARY}
        /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
        @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
    )

    execute_process(
      COMMAND codesign --force --verbose --sign
              "Developer ID Application: Bruno Boutin (AWJJ3YVK9B)" ${BINARY})

  endforeach()

  # This is to cheat the system that we can run the R console without JASP
  execute_process(WORKING_DIRECTORY ${r_pkg_r_home}/bin
                  COMMAND ln -s ../../../../../../Frameworks Frameworks)

endmacro()
