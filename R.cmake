cmake_minimum_required(VERSION 3.21)

if (BUILD_WITH_SYSTEM_R)
	message(STATUS "[JASP]: Building with system R...")

	# Trying to set the RPATH using CMake, but it's not there yet...
	set(CMAKE_SKIP_BUILD_RPATH OFF)
	set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)

	set(CMAKE_INSTALL_RPATH "/opt/homebrew/lib/R/4.1/site-library/Rcpp/libs/;/opt/homebrew/lib/R/4.1/site-library/RInside/libs/")
	set(CMAKE_BUILD_RPATH "/opt/homebrew/lib/R/4.1/site-library/Rcpp/libs/;/opt/homebrew/lib/R/4.1/site-library/RInside/libs/")

	find_package(PkgConfig REQUIRED)

	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

	if (${LIBR_FOUND}) 

		set(_R_EXE "/opt/homebrew/bin/R")
		set(_R_HOME "/opt/homebrew/lib/R/")
		set(_R_Library "opt/homebrew/lib/R/library/")
		set(_Rcpp_HOME "/opt/homebrew/lib/R/4.1/site-library/Rcpp/")
		set(_RInside_HOME "/opt/homebrew/lib/R/4.1/site-library/RInside/")

		message(STATUS "[JASP]: Checking for 'Rcpp.so'")
		find_library(_LIB_RCPP
			NAMES Rcpp.so
			PATHS ${_Rcpp_HOME}/libs
			NO_CACHE
			REQUIRED)

		if (_LIB_RCPP)
			message(STATUS "[JASP]: Found the 'Rcpp.so' library in " ${_LIB_RCPP})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'Rcpp.so'")
		endif()

		message(STATUS "[JASP]: Checking for 'RInside.so'")
		find_library(_LIB_RInside
			NAMES RInside.so
			PATHS ${_RInside_HOME}/libs
			NO_CACHE
			REQUIRED)

		if(_LIB_RInside)
			message(STATUS "[JASP]: Found the 'RInside.so' library in " ${_LIB_RInside})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'RInside.so'")
		endif()

	endif()

else()

	message(STATUS "[Config]: Building using the R.framework")


	if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		set(_R_HOME "R.framework/Versions/4.1/Resources")
		set(_R_Library_HOME "${_R_HOME}/library")
		set(_R_EXE "${_R_HOME}/bin/R")
		set(_Rcpp_HOME "${_R_Library_HOME}/Rcpp")
		set(_RInside_HOME "${_R_Library_HOME}/RInside")

		# message(STATUS ${_R_HOME})
		# message(STATUS ${_Rcpp_HOME})
		# set(CMAKE_INSTALL_RPATH "${_Rcpp_HOME}/libs;${_RInside_HOME}/libs")
		# set(CMAKE_BUILD_RPATH "${_Rcpp_HOME}/libs;${_RInside_HOME}/libs")

		message(STATUS "[JASP]: Looking for 'R.framework'")
		find_library(_R_Framework
			NAMES R
			PATHS ${CMAKE_SOURCE_DIR}
			NO_CACHE
			REQUIRED)

		if(_R_Framework)
			message(STATUS "[JASP]: Found the 'R.framework' library in " ${_R_Framework})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'R.framework' in ${CMAKE_SOURCE_DIR}")
		endif()

		message(STATUS "[JASP]: Checking for 'libR'")
		find_library(_LIB_R
			NAMES R
			PATHS ${_R_HOME}/lib
			NO_CACHE
			REQUIRED)

		if (_LIB_R)
			message(STATUS "[JASP]: Found the 'libR' library in " ${_LIB_R})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'libR' in ${_R_HOME}/lib")
		endif()

		message(STATUS "[JASP]: Checking for 'Rcpp.so'")
		find_library(_LIB_RCPP
			NAMES Rcpp.so
			PATHS ${_Rcpp_HOME}/libs
			NO_CACHE
			REQUIRED)

		if (_LIB_RCPP)
			message(STATUS "[JASP]: Found the 'Rcpp.so' library in " ${_LIB_RCPP})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'Rcpp.so' in ${_Rcpp_HOME}/libs")
		endif()

		message(STATUS "[JASP]: Checking for 'RInside.so'")
		find_library(_LIB_RInside
			NAMES RInside
			PATHS ${_RInside_HOME}/lib
			NO_CACHE
			REQUIRED)

		if(_LIB_RInside)
			message(STATUS "[JASP]: Found the 'RInside.so' library in " ${_LIB_RInside})
		else()
			message(FATAL_ERROR "[JASP]: Couldn't find the 'RInside.so' in ${_RInside_HOME}/libs")
		endif()


	endif()


endif()

message(STATUS "[Config]: _R_HOME is set to ${_R_HOME}")
message(STATUS "[Config]: _Rcpp_HOME is set to ${_Rcpp_HOME}")
message(STATUS "[Config]: _RInside_HOME is set to ${_RInside_HOME}")
message(STATUS "[Config]: _R_Library is set to ${_R_Library_HOME}")