# This will build the Common library
#
# On Linux,
#   - We need to link to the `librt.so`, and for some reason, CMake cannot find it
#     easily, so, Libraries module tries to find it and provides it to Common here.
#   - Since we are not using Conan, I link to the libjsoncpp, using the PkgConfig variables
#
#
list(APPEND CMAKE_MESSAGE_CONTEXT Common)

set(APPINFO_CPP "${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp")
configure_file(${CMAKE_CURRENT_LIST_DIR}/appinfo.cpp.in
               ${APPINFO_CPP})
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB HEADER_FILES    "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB SOURCE_FILES    "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
file(GLOB JSON_SOURCES    "${CMAKE_CURRENT_LIST_DIR}/json/*.cpp")

# Remove stale appinfo.cpp from source tree (moved to binary dir)
list(FILTER SOURCE_FILES EXCLUDE REGEX ".*appinfo\\.cpp$")

list(APPEND SOURCE_FILES ${JSON_SOURCES} ${APPINFO_CPP})

add_library(Common STATIC ${SOURCE_FILES} ${HEADER_FILES})

target_include_directories(
  Common
  PUBLIC
  # R
  ${R_INCLUDE_PATH}
  ${R_HOME_PATH}/include
  ${RCPP_PATH}/include
  ${Boost_INCLUDE_DIRS}
	)

target_link_libraries(
	Common
	PUBLIC
	$<$<BOOL:${WINDOWS}>:ntdll>
  )

target_compile_definitions(
  Common PUBLIC
                $<$<BOOL:${JASP_TIMER_USED}>:PROFILE_JASP>
        JSONCPP_NO_LOCALE_SUPPORT	)

if(WINDOWS)
  target_compile_definitions(
    Common
    PUBLIC NOMINMAX
           WIN32_LEAN_AND_MEAN)
endif()

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(Common PROPERTIES CXX_INCLUDE_WHAT_YOU_USE ${IWYU_EXECUTABLE})
endif()


list(POP_BACK CMAKE_MESSAGE_CONTEXT)
