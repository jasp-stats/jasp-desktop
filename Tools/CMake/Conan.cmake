list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

	if(WIN32 OR USE_CONAN)
	  set(QT_CREATOR_SKIP_CONAN_SETUP ON)

	  set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR})
	  if(NOT EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	  	if (${PROJECT_NAME} STREQUAL "R-Interface")
	  		set(CONAN_FILE_PATH ${CMAKE_SOURCE_DIR}/..)
	  	endif()
	    execute_process(WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	                    COMMAND conan install ${CONAN_FILE_PATH} --build=missing)
	  endif()

	  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	  conan_basic_setup(TARGETS)
	endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
