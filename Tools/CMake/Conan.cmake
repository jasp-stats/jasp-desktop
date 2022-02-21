list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

	if(WIN32 OR USE_CONAN)
	  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

	  if(NOT EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	    execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	                    COMMAND conan install ${CMAKE_SOURCE_DIR} --build=missing)
	  endif()

	  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	  conan_basic_setup(TARGETS)
	endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
