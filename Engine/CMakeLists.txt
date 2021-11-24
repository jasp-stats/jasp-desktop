cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Engine)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

find_package(PkgConfig REQUIRED)
find_package(LibArchive REQUIRED)

pkg_check_modules(
  LIBJSON
  REQUIRED
  IMPORTED_TARGET
  jsoncpp)

add_executable(JASPEngine ${SYSTEM_TYPE} ${HEADER_FILES} ${SOURCE_FILES})

target_include_directories(
  JASPEngine
  PUBLIC ${Boost_INCLUDE_DIRS}
         ${LIBJSON_INCLUDE_DIRS}
         # ${LIBARCHIVE_INCLUDE_DIRS}
         ${LibArchive_INCLUDE_DIRS}
         # and these:
         ${PROJECT_SOURCE_DIR}/R-Interface
         ${PROJECT_SOURCE_DIR}/Common/
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

if(BUILD_WITH_SYSTEM_R)
  message(STATUS "Building with system R..")
  pkg_check_modules(
    LIBR
    REQUIRED
    IMPORTED_TARGET
    libR)

  target_include_directories(
    JASPEngine PUBLIC ${LIBR_INCLUDE_DIRS} ${_Rcpp_HOME}/include
                      ${_RInside_HOME}/include)

else()
  message(STATUS "Building with R.framework...")
  message(STATUS "Setting the headers...")

  target_include_directories(
    JASPEngine
    PUBLIC ${CMAKE_SOURCE_DIR}/${_R_HOME}/include
           ${CMAKE_SOURCE_DIR}/${_Rcpp_HOME}/include
           ${CMAKE_SOURCE_DIR}/${_RInside_HOME}/include)
endif()

target_link_libraries(JASPEngine PUBLIC Common R-Interface)

target_compile_definitions(JASPEngine PUBLIC BUILDING_JASP_ENGINE
                                             R_HOME=${_R_HOME})

add_dependencies(JASPEngine R-Interface)

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
  set_target_properties(
    JASPEngine
    PROPERTIES XCODE_ATTRIBUTE_SKIP_INSTALL NO
               XCODE_ATTRIBUTE_INSTALL_PATH
               "$(LOCAL_APPS_DIR)/JASP.app/Contents/MacOS")
else()

  add_custom_command(
    TARGET JASPEngine
    POST_BUILD
    COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/JASPEngine
            ${PROJECT_BINARY_DIR}/Desktop
    COMMENT "       [JASP]: Placing the Engine into the Desktop folder...")
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

# set_property(SOURCE JASPEngine PROPERTY MACOSX_PACKAGE_LOCATION "MacOS")

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
