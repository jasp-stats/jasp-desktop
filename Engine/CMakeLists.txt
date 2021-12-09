list(APPEND CMAKE_MESSAGE_CONTEXT Engine)

find_package(PkgConfig REQUIRED)
find_package(LibArchive REQUIRED)
find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

pkg_check_modules(
  LIBJSONCPP
  REQUIRED
  IMPORTED_TARGET
  jsoncpp)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_executable(JASPEngine ${HEADER_FILES} ${SOURCE_FILES})

add_dependencies(JASPEngine R-Interface)

target_include_directories(
  JASPEngine
  PUBLIC ${LIBJSONCPP_INCLUDE_DIRS}
         # JASP
         ${PROJECT_SOURCE_DIR}/R-Interface
         ${PROJECT_SOURCE_DIR}/Common/
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
         # R
         ${CMAKE_SOURCE_DIR}/${_R_HOME}/include
         ${CMAKE_SOURCE_DIR}/${_Rcpp_HOME}/include
         ${CMAKE_SOURCE_DIR}/${_RInside_HOME}/include)

target_link_libraries(
  JASPEngine
  PUBLIC Common
         R-Interface
         Boost::nowide
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         LibArchive::LibArchive)

if(IWYU AND RUN_IWYU)
  set_target_properties(JASPEngine PROPERTIES CXX_INCLUDE_WHAT_YOU_USE ${IWYU})
endif()

target_compile_definitions(
  JASPEngine PUBLIC $<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>
                    BUILDING_JASP_ENGINE R_HOME=${_R_HOME})

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

# set_property(SOURCE JASPEngine PROPERTY MACOSX_PACKAGE_LOCATION "MacOS")

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
