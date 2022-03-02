list(APPEND CMAKE_MESSAGE_CONTEXT Conan)

if(WIN32 OR USE_CONAN)

  set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR})
  
  execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                    COMMAND conan install ${CONAN_FILE_PATH} --build=missing)

  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
  include(${CMAKE_BINARY_DIR}/conan_paths.cmake)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
