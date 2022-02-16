if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

	find_program(FORTRAN_EXECUTABLE
		NAMES gfortran
	 REQUIRED)

endif()