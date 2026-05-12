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

  if(JASP_SYNTAX_INTERFACE_ONLY)
    set(CONAN_SYNTAX_OPTION "-o syntax_interface_only=True")
  else()
    set(CONAN_SYNTAX_OPTION "")
  endif()

  # We use our own recipe with some patches to cook up a functional version of freexl, so get the recipe:
  if(NOT JASP_SYNTAX_INTERFACE_ONLY)
    message(STATUS "Cloning freexl dependency")
    set(FREEXL_VERSION "2.0.99.cci.20260225")
    FetchContent_Declare(
      freexl
      GIT_REPOSITORY   https://github.com/jasp-stats/conan-recipes.git
      GIT_TAG          620019a56c6ba94936c9844ab5c79e8db9baa06b
    )
    FetchContent_MakeAvailable(freexl)
  endif()

  # Configure Conan for windows
  if(WIN32)
    set(CONAN_RESULT_FILE "conanbuild.bat") #for windows

    message(STATUS "  ${CONAN_COMPILER_RUNTIME}")

    if(NOT JASP_SYNTAX_INTERFACE_ONLY)
      if(freexl_POPULATED)
        message(STATUS "Compiling freexl dependency ${freexl_SOURCE_DIR}")
        execute_process(
            COMMAND_ECHO STDOUT
            WORKING_DIRECTORY ${freexl_SOURCE_DIR}/freexl
            COMMAND
            conan create ${freexl_SOURCE_DIR}/freexl --version=${FREEXL_VERSION}
            -s build_type=${CMAKE_BUILD_TYPE}
            -c tools.cmake.cmaketoolchain:generator=${CMAKE_GENERATOR}
            -s compiler.runtime=${CONAN_COMPILER_RUNTIME} --build=missing
            --test-missing
        )
      else()
        message(CHECK_FAIL "build freexl failed")
      endif()
    endif()

    execute_process(
      COMMAND_ECHO STDOUT
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      COMMAND
      conan install ${CONAN_FILE_PATH} --output-folder=${CMAKE_BINARY_DIR}/_conan_build
      -s build_type=${CMAKE_BUILD_TYPE}
      -c tools.cmake.cmaketoolchain:generator=${CMAKE_GENERATOR}
      -s compiler.runtime=${CONAN_COMPILER_RUNTIME} --build=missing
      ${CONAN_SYNTAX_OPTION})

    set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES _deps)

      # configure conan for apple
  elseif(APPLE)

    set(CONAN_RESULT_FILE "conanbuild.sh")

    # We set CC and CCX to nothing because that was the only difference between running conan in a terminal (where it worked) and in qt creator (where it did not)
    # They were set to bona fide looking xtools stuff but apparently this was too much for conan.

    if(NOT JASP_SYNTAX_INTERFACE_ONLY)
      execute_process(
        COMMAND_ECHO STDOUT
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMAND zsh -c -l "export CC=\"\"; export CCX=\"\"; conan create ${freexl_SOURCE_DIR}/freexl --version=${FREEXL_VERSION} -s build_type=${CMAKE_BUILD_TYPE} -s os.version=${CMAKE_OSX_DEPLOYMENT_TARGET} --build=missing --test-missing")
    endif()

    execute_process(
        COMMAND_ECHO STDOUT
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMAND zsh -c -l "export CC=\"\"; export CCX=\"\"; conan install ${CONAN_FILE_PATH} -s build_type=${CMAKE_BUILD_TYPE} -s os.version=${CMAKE_OSX_DEPLOYMENT_TARGET} --build=missing ${CONAN_SYNTAX_OPTION} -of ${CMAKE_BINARY_DIR}/_conan_build")

  endif()

  if(EXISTS ${CMAKE_BINARY_DIR}/_conan_build/${CONAN_RESULT_FILE})
    message(CHECK_PASS "successful")
  else()
    message(CHECK_FAIL "unsuccessful")
    message(
      FATAL_ERROR
        "Conan configuration failed. You may try running the above conan command from your command line, in your build directory."
    )
  endif()

  include(${CMAKE_BINARY_DIR}/_conan_build/conan_toolchain.cmake)
  
  set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES _deps)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
