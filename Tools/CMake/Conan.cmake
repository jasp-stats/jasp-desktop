list(APPEND CMAKE_MESSAGE_CONTEXT Conan)

if(WIN32 OR USE_CONAN)

  message(CHECK_START "Configuring Conan")
  set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR})

  execute_process(
    ERROR_QUIET OUTPUT_QUIET
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMAND conan install ${CONAN_FILE_PATH} --build=missing)

  if(EXISTS ${CMAKE_BINARY_DIR}/conan_paths.cmake)
    message(CHECK_PASS "successful")
  else()
    message(CHECK_FAIL "unsuccessful")
    message(
      FATAL_ERROR
        "Conan configuration failed. You may try running the conan command from your command line, e.g., `conan install <path-to-source-dir> --build=missing`"
    )
  endif()

  include(${CMAKE_BINARY_DIR}/conan_paths.cmake)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
