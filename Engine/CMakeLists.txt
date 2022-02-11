list(APPEND CMAKE_MESSAGE_CONTEXT Engine)

find_package(LibArchive)
if(NOT LibArchive_FOUND)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    LibArchive
    REQUIRED
    IMPORTED_TARGET
    libarchive)
endif()

set(Boost_USE_STATIC_LIBS ON)
find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)

else()

  # find_library(
  #   LIBJSONCPP REQUIRED
  #   NAMES jsoncpp
  #   HINTS ${jsoncpp_LIBRARY_DIRS})

  # WARNING: This is not the way to do it, and it does not work with Ninja :\

  set(LIBJSONCPP_INCLUDE_DIRS ${jsoncpp_INCLUDE_DIRS})
  set(LIBJSONCPP_LIBRARY_DIRS ${jsoncpp_LIBRARY_DIRS})
  set(LIBJSONCPP_LINK_LIBRARIES ${jsoncpp_LIBRARY_DIRS}/libjsoncpp.a)

endif()

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_executable(JASPEngine ${HEADER_FILES} ${SOURCE_FILES})

add_dependencies(JASPEngine R-Interface)

target_include_directories(JASPEngine PUBLIC ${LIBJSONCPP_INCLUDE_DIRS}
                                             ${PROJECT_SOURCE_DIR}/R-Interface)

target_link_libraries(
  JASPEngine
  PUBLIC Common
         R-Interface
         #
         Boost::nowide
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         # System
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
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
