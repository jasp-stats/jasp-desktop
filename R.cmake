cmake_minimum_required(VERSION 3.2)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(_R_HOME "${JASP_REQUIRED_FILES}/Frameworks/R.framework/Versions/${CURRENT_R_VERSION}/Resources")
	set(_R_EXE "${_R_HOME}/bin/R")
endif()

set(_R_Library "${_R_HOME}/library")