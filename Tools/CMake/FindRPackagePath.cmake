
# Locate the full paths (so those in the cache, not the symlinks)
set(FIND_PACKAGE_PATH_SCRIPT		${PROJECT_SOURCE_DIR}/Tools/find_package_path.R)
macro(find_package_path out libPath packageName)
	#We are not using Rscript anywhere in CMake because that doesn't work on macOs
	execute_process(
		COMMAND ${R_EXECUTABLE} --slave --file=${FIND_PACKAGE_PATH_SCRIPT} ${libPath} ${packageName} 
		OUTPUT_VARIABLE TEMP_OUT
		COMMAND_ECHO STDOUT)
	#R could be printing all sorts of bullshit, so instead we simply take the last line of output
	string(REGEX MATCHALL "[^\n\r]+" TEMPLINES ${TEMP_OUT})
	list(GET TEMPLINES -1 ${out})
endmacro()
