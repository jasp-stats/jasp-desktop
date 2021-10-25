cmake_minimum_required(VERSION 3.20)

if (BUILD_WITH_SYSTEM_R)
	message("[JASP]: Building with system R...")

	find_package(PkgConfig REQUIRED)

	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

	if (${LIBR_FOUND}) 

		set(_R_EXE "/opt/homebrew/bin/R")
		set(_R_HOME "/opt/homebrew/lib/R/")
		set(_R_Library "opt/homebrew/lib/R/library/")
		set(_Rcpp_HOME "/opt/homebrew/lib/R/4.1/site-library/Rcpp/")
		set(_RInside_HOME "/opt/homebrew/lib/R/4.1/site-library/RInside/")

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