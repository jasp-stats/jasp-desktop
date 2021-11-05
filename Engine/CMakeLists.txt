cmake_minimum_required(VERSION 3.21)

message(STATUS "[JASP]: Configuring the Engine")

#####################################################################
## JASPEngine Binary:
#####################################################################

set(HEADER_FILES
	engine.h
	otoolstuff.h
	rbridge.h
)

set(SOURCE_FILES
	engine.cpp 
	main.cpp
	otoolstuff.cpp 
	rbridge.cpp 
)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)

# TODO: I don't think Engine needs all of these, but for now, I leave them here
find_package(Boost COMPONENTS nowide filesystem system date_time timer chrono REQUIRED)

find_package(Qt6 REQUIRED
				 COMPONENTS
				 Core
			)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})
include_directories(${LIBARCHIVE_INCLUDE_DIRS})

# TODO: This should be done better
include_directories(${PROJECT_SOURCE_DIR}/R-Interface)
include_directories(${PROJECT_SOURCE_DIR}/Common/)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

# configure_file(appinfo.cpp.in appinfo.cpp)

if (BUILD_WITH_SYSTEM_R)
	message(STATUS "[Engine]: Building with system R..")
	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

	include_directories(${LIBR_INCLUDE_DIRS})
	include_directories(${_Rcpp_HOME}/include)
	include_directories(${_RInside_HOME}/include)

else()
	message(STATUS "[Engine]: Building with R.framework...")
	message(STATUS "[Engine]: Setting the headers...")

	include_directories(${CMAKE_SOURCE_DIR}/${_R_HOME}/include)
	include_directories(${CMAKE_SOURCE_DIR}/${_Rcpp_HOME}/include)
	include_directories(${CMAKE_SOURCE_DIR}/${_RInside_HOME}/include)
endif()


add_definitions(-DBUILDING_JASP_ENGINE)
add_definitions(-DR_HOME=${_R_HOME})

add_executable(JASPEngine
	${HEADER_FILES}
	${SOURCE_FILES}
)


target_link_libraries(
	JASPEngine PUBLIC 
		Common
		R-Interface
	)

add_dependencies(JASPEngine R-Interface)


set(MACOSX_BUNDLE_DIR "${CMAKE_BINARY_DIR}/Desktop/Debug/${PROJECT_NAME}.app/Contents")
set(MACOSX_BUNDLE_RUNTIME_DIR "${MACOSX_BUNDLE_DIR}/MacOS")
set(MACOSX_BUNDLE_RESOURCE_DIR "${MACOSX_BUNDLE_DIR}/Resources")

# This does the job, but I'm not sure if that's the right way of doing it.
# I'm still asking around to see how to do this correctly. The side-effect of
# doing it post build like this is that Xcode loses control, and therefore 
# cannot sign the binary properly
if(CMAKE_GENERATOR STREQUAL "Xcode")
	# add_custom_command(TARGET JASPEngine
	#     POST_BUILD
	#     COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/Debug/JASPEngine ${MACOSX_BUNDLE_RUNTIME_DIR}
 	# 	    COMMENT "       [JASP]: Placing the Engine into the MacOS folder...")

	set_target_properties(JASPEngine PROPERTIES
	    XCODE_ATTRIBUTE_SKIP_INSTALL  NO
	    XCODE_ATTRIBUTE_INSTALL_PATH  "$(LOCAL_APPS_DIR)/JASP.app/Contents/MacOS"
	)
else()

	add_custom_command(TARGET JASPEngine
    POST_BUILD
    COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/JASPEngine ${PROJECT_SOURCE_DIR}/Desktop
	    COMMENT "       [JASP]: Placing the Engine into the Desktop folder...")
endif()


# set_property(SOURCE JASPEngine
# 			 PROPERTY MACOSX_PACKAGE_LOCATION "MacOS")
