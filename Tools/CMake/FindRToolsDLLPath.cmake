

macro(find_rtools_dll_path dllPath dllFilename)
	message(CHECK_START "Looking for ${dllFilename} for ${dllPath}")
	find_file(
		${dllPath}
		NAMES ${dllFilename}
		PATHS ${RTOOLS_PATH}/bin ${RTOOLS_PATH}/lib ${RTOOLS_PATH}/../usr/bin
		NO_DEFAULT_PATH)

	if(EXISTS ${${dllPath}})
		message(CHECK_PASS "found")
	  message(STATUS "  ${${dllPath}}")
    else()
		message(CHECK_FAIL "not found")
	  message(
		FATAL_ERROR
		  "MSYS2 and some of its libraries (${dllFilename}) are required for building on Windows, please follow the build instruction before you continue."
	  )
    endif()
endmacro()


macro(copy_rtools_header headerVar headerName headerDestination)
	message(CHECK_START "Looking for ${headerName} for ${headerVar}")
	find_file(
		${headerVar}
		NAMES ${headerName}
		PATHS ${RTOOLS_PATH}/include
		NO_DEFAULT_PATH)

	if(EXISTS ${${headerVar}})
		message(CHECK_PASS "found")
	  message(STATUS "Now copy ${${headerVar}} to ${headerDestination}.")
	  configure_file("${${headerVar}}" "${headerDestination}" COPYONLY)

    else()
		message(CHECK_FAIL "not found")
	  message(
		FATAL_ERROR
		  "Header (${headerName}) is required for building on Windows, please follow the build instruction before you continue."
	  )
    endif()
endmacro()
