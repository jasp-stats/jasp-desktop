list(APPEND CMAKE_MESSAGE_CONTEXT Conan)

if(WIN32 OR USE_CONAN)

  set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR})
  if(NOT EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)

    execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                    COMMAND conan install ${CONAN_FILE_PATH} --build=missing)
  endif()

  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
  conan_basic_setup(TARGETS)

  # include(${CMAKE_BINARY_DIR}/conan_paths.cmake)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
