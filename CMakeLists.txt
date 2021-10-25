cmake_minimum_required(VERSION 3.20)

project(JASP 
	VERSION 0.16.0
	LANGUAGES CXX C)
# TODO: Later on, we can use CMake to even have proper TWEAK versioning as well

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

include(JASP.cmake)
include(R.cmake)

find_package(Qt6 REQUIRED
				 COMPONENTS
				 Core
				 Gui
				 OpenGL
				 Widgets 
				 Qml
				)

add_subdirectory(Common)
add_subdirectory(R-Interface)
add_subdirectory(Engine)
add_subdirectory(Desktop)
# add_subdirectory(Modules)

# Some primarily checks. This will extend as we move to other systems.
if(NOT CMAKE_BUILD_TYPE MATCHES Debug) #Release, RelWithDebInfo and MinSizeRel
	if(WIN32) # Check if we are on Windows
		set(SYSTEM_TYPE WIN32)
	elseif(APPLE) # MacOS X
		set(SYSTEM_TYPE MACOSX_BUNDLE)
	else() # UNIX
		set(SYSTEM_TYPE EXCLUDE_FROM_ALL)
	endif()
	message(STATUS "System type is " ${SYSTEM_TYPE})
endif()