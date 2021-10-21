cmake_minimum_required(VERSION 3.2)

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
# add_subdirectory(Desktop)
# add_subdirectory(Modules)

# if(UNIX)
# endif()
