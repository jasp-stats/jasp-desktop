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

	if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		set(_R_HOME "${JASP_REQUIRED_FILES}/Frameworks/R.framework/Versions/${CURRENT_R_VERSION}/Resources")
		set(_R_EXE "${_R_HOME}/bin/R")
	endif()


	set(_R_Library "${_R_HOME}/library")

endif()

message(STATUS "[Config]: _R_HOME is set to ${_R_HOME}")
message(STATUS "[Config]: _Rcpp_HOME is set to ${_Rcpp_HOME}")
message(STATUS "[Config]: _RInside_HOME is set to ${_RInside_HOME}")
message(STATUS "[Config]: _R_Library is set to ${_R_Library}")