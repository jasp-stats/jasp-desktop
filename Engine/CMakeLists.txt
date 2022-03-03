list(APPEND CMAKE_MESSAGE_CONTEXT Engine)

if(LINUX)

  target_link_libraries(R-Interface PUBLIC ${_LIB_R})

endif()

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_executable(JASPEngine ${HEADER_FILES} ${SOURCE_FILES})

# add_custom_command(
#   TARGET JASPEngine
#   POST_BUILD
#   WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/R-Interface
#   DEPENDS
#   ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
#   ${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log
#   ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
#   COMMAND ${CMAKE_COMMAND} -E copy_if_different jaspResults/R/writeImage.R
#           ${MODULES_BINARY_PATH}/
#   COMMAND ${CMAKE_COMMAND} -E copy_if_different jaspResults/R/zzzWrappers.R
#           ${MODULES_BINARY_PATH}/
#   COMMAND ${CMAKE_COMMAND} -E copy_if_different R/workarounds.R
#           ${MODULES_BINARY_PATH}/
#   COMMAND ${CMAKE_COMMAND} -E copy_if_different R/symlinkTools.R
#           ${MODULES_BINARY_PATH}/)

# Note that on WIN32 this is going to invoke the external build of
# the R-Interface. So, while I'm using the same name for these targets
# they are not 100% the same!
add_dependencies(JASPEngine R-Interface jaspBase)

target_include_directories(
  JASPEngine
  PUBLIC ${PROJECT_SOURCE_DIR}/R-Interface ${PROJECT_SOURCE_DIR}/Common
         # Conan
         $<$<BOOL:${USE_CONAN}>:${CONAN_INCLUDE_DIRS_JSONCPP}>)

if(WIN32)

  message(CHECK_START "Setting path to libR-Interface.dll")
  set(_LIB_R_INTERFACE_SHARED
      ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll)
  message(STATUS "  ${_LIB_R_INTERFACE_SHARED}")

  message(CHECK_START "Setting path to libR-Interface.dll.a")
  set(_LIB_R_INTERFACE_STATIC
      ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll.a)
  message(STATUS "  ${_LIB_R_INTERFACE_STATIC}")

  message(STATUS "libR-Interface will be build if it is missing.")

endif()

target_link_libraries(
  JASPEngine
  PUBLIC Common
         #
         $<$<PLATFORM_ID:Windows>:${_LIB_R_INTERFACE_STATIC}>
         $<$<NOT:$<PLATFORM_ID:Windows>>:R-Interface>
         # Boost
         Boost::nowide
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

set(MACOSX_BUNDLE_DIR
    "${CMAKE_BINARY_DIR}/Desktop/Debug/${PROJECT_NAME}.app/Contents")
set(MACOSX_BUNDLE_RUNTIME_DIR "${MACOSX_BUNDLE_DIR}/MacOS")
set(MACOSX_BUNDLE_RESOURCE_DIR "${MACOSX_BUNDLE_DIR}/Resources")

# This does the job, but I'm not sure if that's the right way of doing it. I'm
# still asking around to see how to do this correctly. The side-effect of doing
# it post build like this is that Xcode loses control, and therefore cannot sign
# the binary properly
if(CMAKE_GENERATOR STREQUAL "Xcode")
  # add_custom_command(TARGET JASPEngine POST_BUILD COMMAND cp
  # ${CMAKE_CURRENT_BINARY_DIR}/Debug/JASPEngine ${MACOSX_BUNDLE_RUNTIME_DIR}
  # COMMENT "       [JASP]: Placing the Engine into the MacOS folder...")

  # I don't think this works
  # set_property(SOURCE JASPEngine PROPERTY MACOSX_PACKAGE_LOCATION "MacOS")

  # set_target_properties(JASPEngine PROPERTIES MACOSX_PACKAGE_LOCATION "MacOS")

else()

  set_target_properties(JASPEngine PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                              ${CMAKE_BINARY_DIR}/Desktop)
endif()

if(APPLE)
  # Amir: There are some compiler messages that I left out for now

  # target_compile_options(
  #   JASPEngine
  #   PUBLIC -Wno-unused-parameter -Wno-unused-local-typedef
  #          -Wno-c++11-extensions -Wno-c++11-long-long -Wno-c++11-extra-semi
  #          -stdlib=libc++)

  # target_compile_definitions(JASPEngine PUBLIC )
elseif(WIN32)
  target_compile_definitions(
    JASPEngine
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)

  target_link_libraries(JASPEngine PUBLIC ole32 oleaut32)

  # These are mainly for R-Interface, and all build using the MinGW
  add_custom_command(
    TARGET JASPEngine
    POST_BUILD DEPENDS ${CMAKE_BINARY_DIR}/R-Interface/libR-Interface.dll
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MINGW_LIBGCC_S_SEH}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MINGW_LIBSTDCPP}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MINGW_LIBWINPTHREAD}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MINGW_LIBJSONCPP}
            ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${_LIB_R_INTERFACE_SHARED}
            ${CMAKE_BINARY_DIR}
    COMMENT "------ Re-installing the dependencies (if necessary)")

elseif(LINUX)

  # target_link_options(JASPEngine
  # PUBLIC rt)
  # target_link_libraries(JASPEngine PUBLIC "-lrt")

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
