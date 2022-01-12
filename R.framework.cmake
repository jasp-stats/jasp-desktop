macro(patch_ld_paths)

  cmake_print_variables(r_pkg_r_home)
  cmake_print_variables(r_pkg_SOURCE_DIR)

  file(GLOB_RECURSE SO_LIBRARIES "*.so")
  file(GLOB_RECURSE DYLIB_LIBRARIES "*.dylib")
  list(
    FILTER
    DYLIB_LIBRARIES
    EXCLUDE
    REGEX
    ".*dSYM.*")

  # foreach(SO_LIB ${SO_LIBRARIES})

  #   cmake_path(
  #     GET
  #     SO_LIB
  #     FILENAME
  #     SO_LIB_FILENAME)
  #   message(CHECK_START "Patching ${SO_LIB_FILENAME}")

  #   string(
  #     REGEX MATCH
  #           ".*library.*"
  #           SO_LIB_IN_LIBRARY
  #           ${SO_LIB})

  #   if(SO_LIB_IN_LIBRARY)

  #     execute_process(
  #       WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
  #       COMMAND
  #         ./install_name_prefix_tool.sh ${SO_LIB_IN_LIBRARY}
  #         /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
  #         @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
  #     )

  #   endif()

  #   string(
  #     REGEX MATCH
  #           ".*module.*"
  #           SO_LIB_IN_MODULES
  #           ${SO_LIB})

  #   if(SO_LIB_IN_MODULES)

  #     execute_process(
  #       WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
  #       COMMAND
  #         ./install_name_prefix_tool.sh ${SO_LIB_IN_MODULES}
  #         /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
  #         @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
  #     )

  #   endif()

  #   message(CHECK_PASS "successful.")
  # endforeach()

  foreach(DYLIB_LIB ${DYLIB_LIBRARIES})

    cmake_path(
      GET
      DYLIB_LIB
      FILENAME
      DYLIB_LIB_FILENAME)
    message(CHECK_START "Patching ${DYLIB_LIB_FILENAME}")

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        ./install_name_prefix_tool.sh ${DYLIB_LIB}
        /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
        @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
    )

    message(CHECK_PASS "successful.")
  endforeach()

  set(BINARIES "R.framework/Versions/4.1-arm64/Resources/bin/exec/R")

  foreach(BINARY ${BINARIES})

    execute_process(
      WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
      COMMAND
        ./install_name_prefix_tool.sh ${BINARY}
        /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
        @executable_path/../Frameworks/R.framework/Versions/4.1-arm64/Resources/lib
    )

  endforeach()

endmacro()
