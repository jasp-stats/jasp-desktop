cmake_minimum_required(VERSION 3.2)


if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(_R_HOME "${JASP_REQUIRED_FILES}/Frameworks/R.framework/Versions/${CURRENT_R_VERSION}/Resources")
	# message(${_R_HOME})
	set(R_EXE "${_R_HOME}/bin/R")
	# message(${R_EXE})
endif()

