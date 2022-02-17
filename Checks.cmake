if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

	message(CHECK_START "Looking for 'gfortran'")
	find_program(FORTRAN_EXECUTABLE
		NAMES gfortran
	 REQUIRED
	 DOC "'gfortran' is needed for building some of the R packages")

	if(NOT FORTRAN_EXECUTABLE)
		message(CHECK_FAIL "not found. Please install 'gfortran' before continuing.")
	else()
		message(CHECK_PASS "found")
	endif()

	message(CHECK_START "Looking for 'mercurial'")
	find_program(MERCURIAL_EXECUTABLE
		NAMES hg
		DOC "'mercurial' is needed for downloading some of the dependencies.")

	if(NOT MERCURIAL_EXECUTABLE)
		message(CHECK_FAIL "not found. Please install 'mercurial' before continuing.")
	else()
		message(CHECK_PASS "found")
	endif()

	message(CHECK_START "Looking for 'flex'")
	find_program(FLEX_EXECUTABLE
		NAMES flex
		DOC "'flex' is needed for building some of the dependencies.")

	if(NOT FLEX_EXECUTABLE)
		message(CHECK_FAIL "not found. Please install 'flex' before continuing.")
	else()
		message(CHECK_PASS "found")
	endif()

	message(CHECK_START "Looking for 'bison'")
	find_program(BISON_EXECUTABLE
		NAMES flex
		DOC "'bison' is needed for building some of the dependencies.")

	if(NOT BISON_EXECUTABLE)
		message(CHECK_FAIL "not found. Please install 'bison' before continuing.")
	else()
		message(CHECK_PASS "found")
	endif()

endif()