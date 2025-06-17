

macro(find_rtools_dll_path out dllPath dllFilename)
	message(CHECK_START "Looking for ${dllFilename}")
    find_file(
      ${dllPath}
      NAMES ${dllFilename}
      PATHS ${RTOOLS_PATH}/bin
      NO_DEFAULT_PATH)
  
    if(EXISTS ${dllPath})
      message(CHECK_PASS "found")
      message(STATUS "  ${dllPath}")
    else()
      message(CHECK_FAIL "not found")
      message(
        FATAL_ERROR
          "MSYS2 and some of its libraries (${dllFilename}) are required for building on Windows, please follow the build instruction before you continue."
      )
    endif()
	
endmacro()
