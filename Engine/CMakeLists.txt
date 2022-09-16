# Configuring and building the JASPEngine. To build JASPEngine we need the R-Interface, and
# the `add_dependencies` takes care of this dependency, however, there is a subtle difference
# between how it works on Windows compared to other OSes. On Windwos, R-Interface is a
# custom target, while on other OSes its a target defined by `add_library`.
#
# On Windows,
#   - We need to manually link to the R-Interface.dll.a and also place the R-Interface.dll
#     inside the build folder.
#
list(APPEND CMAKE_MESSAGE_CONTEXT Engine)

if(LINUX)

  target_link_libraries(R-Interface PUBLIC ${_LIB_R})

endif()

file(GLOB HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

file(GLOB JASPRESULTS_HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspBase/src/*.h")
file(GLOB JASPRESULTS_SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspBase/src/*.cpp")

# this target is not compiled and only exists so that the sources and headers of jaspResults show up in the project and can be opened with Ctrl+k
add_custom_target(jaspResults ALL SOURCES ${JASPRESULTS_SOURCE_FILES} ${JASPRESULTS_HEADER_FILES})

add_executable(
    JASPEngine
    ${SOURCE_FILES}
    ${HEADER_FILES}
    $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Engine/JASPEngine.exe.manifest>
)

add_dependencies(JASPEngine R-Interface)

target_include_directories(
  JASPEngine
  PUBLIC ${PROJECT_SOURCE_DIR}/R-Interface
         ${PROJECT_SOURCE_DIR}/Common
         # Conan
         #$<$<BOOL:${USE_CONAN}>:${CONAN_INCLUDE_DIRS_JSONCPP}>
         #
         ${Boost_INCLUDE_DIRS})

if(WIN32)

  message(CHECK_START "Setting path to libR-Interface.dll")
  set(_LIB_R_INTERFACE_DLL
      "${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll"
      CACHE STRING "Path to libR-Interface.dll")
  message(STATUS "  ${_LIB_R_INTERFACE_DLL}")

  message(CHECK_START "Setting path to libR-Interface.dll.a")
  set(_LIB_R_INTERFACE_STATIC_DLL_A
      "${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll.a"
      CACHE STRING "Path to libR-Interface.dll.a")
  message(STATUS "  ${_LIB_R_INTERFACE_STATIC_DLL_A}")

  message(STATUS "libR-Interface will be build if it is missing.")

endif()

target_link_libraries(
  JASPEngine
  PUBLIC Common
         #
         $<$<PLATFORM_ID:Windows>:${_LIB_R_INTERFACE_STATIC_DLL_A}>
         $<$<NOT:$<PLATFORM_ID:Windows>>:R-Interface>
         # Boost
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         #
         LibArchive::LibArchive)

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(JASPEngine PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                              ${IWYU_EXECUTABLE})
endif()

target_compile_definitions(
  JASPEngine
  PUBLIC $<$<BOOL:${JASP_PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>
         BUILDING_JASP_ENGINE R_HOME=${R_HOME_PATH})

# Putting the JASPEngine next to the JASP executable
set_target_properties(JASPEngine PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                            ${CMAKE_BINARY_DIR}/Desktop)

if(WINDOWS)
  target_compile_definitions(
    JASPEngine
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_WINDOWS
           NOMINMAX
           WIN32_LEAN_AND_MEAN)

  target_link_libraries(JASPEngine PUBLIC ole32 oleaut32)

  # increase the stack size so that recursive calls in renv when installing jasp modules do not cause stack overflows, see https://github.com/jasp-stats/INTERNAL-jasp/issues/2072
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:48000000")

  # These are mainly for R-Interface, and all build using the MinGW
  add_custom_command(
    TARGET JASPEngine
    POST_BUILD DEPENDS ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBGCC_S_SEH_DLL}
            ${CMAKE_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBSTDCPP_DLL}
			${CMAKE_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_MSYS_DLL}
			${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBWINPTHREAD_DLL}
            ${CMAKE_BINARY_DIR}
			#COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBJSONCPP_DLL}
	#        ${CMAKE_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBREADSTAT_DLL}
			${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_ZLIB_DLL}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTOOLS_LIBICONV_DLL}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${_LIB_R_INTERFACE_DLL}
            ${CMAKE_BINARY_DIR}
    COMMENT "------ Re-installing the dependencies (if necessary)")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
