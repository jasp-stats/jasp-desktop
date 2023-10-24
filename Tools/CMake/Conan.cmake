# Conan.cmake tries to run the `conan install` command using the right
# parameters. If everything goes right, you don't need to do anything,
# and CMake and Conan should handle all the dependencies properly. However,
# if you have any issues with Conan, you need to get your hand dirty, and
# acutally run the `conan install` command.
#
# In general, it's better if users are running their command, for now,
# if this works, I would like to handle it more automatically, but this
# turns out to be complicated or problematic, I will remove this and
# add a step to the build guide.
#
# As for what happens here, Conan download and build the necessary libraries,
# and if everything goes right, it generates several Find*.cmake files in the
# build folder, and these will be used by CMake and Libraries.cmake to find
# and link necessary libraries to JASP.

list(APPEND CMAKE_MESSAGE_CONTEXT Conan)

if(USE_CONAN)

  message(CHECK_START "Configuring Conan")
  set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR})

  message(STATUS "  ${CMAKE_BUILD_TYPE}")
  set(CONAN_COMPILER_RUNTIME "dynamic")
  set(CONAN_RESULT_FILE "conanbuild.bat") #for windows

  if(WIN32)

    message(STATUS "  ${CONAN_COMPILER_RUNTIME}")

    execute_process(
      COMMAND_ECHO STDOUT
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      COMMAND
      conan install ${CONAN_FILE_PATH} --output-folder=${CMAKE_BINARY_DIR}/conan_build
      -s build_type=${CMAKE_BUILD_TYPE}
      -s compiler.runtime=${CONAN_COMPILER_RUNTIME} --build=missing)

  elseif(APPLE)

    set(CONAN_RESULT_FILE "conanbuild.sh")

    if(CROSS_COMPILING)

      execute_process(
        COMMAND_ECHO STDOUT
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMAND
          conan install ${CONAN_FILE_PATH} -s build_type=${CMAKE_BUILD_TYPE} -s
          os.version=${CMAKE_OSX_DEPLOYMENT_TARGET} -s
          arch=${CONAN_ARCH} -s arch_build=${CONAN_ARCH} --build=missing -of ${CMAKE_BINARY_DIR}/conan_build)

    else()

      execute_process(
        COMMAND_ECHO STDOUT
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMAND
          conan install ${CONAN_FILE_PATH} -s build_type=${CMAKE_BUILD_TYPE} -s
          os.version=${CMAKE_OSX_DEPLOYMENT_TARGET}
          --build=missing -of ${CMAKE_BINARY_DIR}/conan_build)

    endif()

  endif()

  if(EXISTS ${CMAKE_BINARY_DIR}/conan_build/${CONAN_RESULT_FILE})
    message(CHECK_PASS "successful")
  else()
    message(CHECK_FAIL "unsuccessful")
    message(
      FATAL_ERROR
        "Conan configuration failed. You may try running the above conan command from your command line, in your build directory."
    )
  endif()

  include(${CMAKE_BINARY_DIR}/conan_build/conan_toolchain.cmake)

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
