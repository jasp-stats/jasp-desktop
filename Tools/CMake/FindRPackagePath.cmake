
# Locate the full paths (so those in the cache, not the symlinks)

#Argh
if("${CMAKE_PROJECT_NAME}" STREQUAL "R-Interface")
	set(FIND_PACKAGE_PATH_SCRIPT		${CMAKE_SOURCE_DIR}/../Tools/find_package_path.R)
else()

	set(FIND_PACKAGE_PATH_SCRIPT		${CMAKE_SOURCE_DIR}/Tools/find_package_path.R)
endif()

macro(find_package_path out libPath packageName)
	#We are not using Rscript anywhere in CMake because that doesn't work on macOs
	execute_process(
		COMMAND ${R_EXECUTABLE} --slave --file=${FIND_PACKAGE_PATH_SCRIPT} ${libPath} ${packageName} 
		OUTPUT_VARIABLE TEMP_OUT
		COMMAND_ECHO STDOUT)
	#R could be printing all sorts of bullshit, so instead we simply take the last line of output
	message(STATUS "TEMP_OUT is ${TEMP_OUT}")
	string(REGEX MATCHALL "[^\n\r]+" TEMPLINES ${TEMP_OUT})
	list(GET TEMPLINES -1 ${out})
endmacro()
