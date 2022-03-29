# This will build the Common library
#
# On Linux,
#   - We need to link to the `librt.so`, and for some reason, CMake cannot find it
#     easily, so, Libraries module tries to find it and provides it to Common here.
#   - Since we are not using Conan, I link to the libjsoncpp, using the PkgConfig variables
#
#
list(APPEND CMAKE_MESSAGE_CONTEXT Common)

configure_file(${CMAKE_CURRENT_LIST_DIR}/appinfo.cpp.in
               ${CMAKE_CURRENT_LIST_DIR}/appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_library(Common ${HEADER_FILES} ${SOURCE_FILES})

if(LINUX)

  target_link_libraries(Common PUBLIC ${_LIB_RT})
  target_link_options(Common PUBLIC -lrt)

endif()

target_include_directories(
  Common
  PUBLIC # JASP
         jaspColumnEncoder
         # R
         ${R_INCLUDE_PATH}
         ${R_HOME_PATH}/include
         ${RCPP_PATH}/include
         #
         $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_INCLUDEDIR}>)

target_link_libraries(
  Common
  PUBLIC # jsoncpp
         $<$<BOOL:${USE_CONAN}>:jsoncpp::jsoncpp>
         $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LIBRARIES}>
         $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LINK_LIBRARIES}>
         #
         LibArchive::LibArchive
         # Boost
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         #
         $<$<BOOL:${JASP_USES_QT_HERE}>:Qt::Core>)

target_compile_definitions(
  Common PUBLIC $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
                $<$<BOOL:${JASP_TIMER_USED}>:PROFILE_JASP>)

if(APPLE)
  target_compile_definitions(Common PUBLIC BOOST_INTERPROCESS_SHARED_DIR_FUNC)
elseif(WINDOWS)
  target_link_libraries(Common PUBLIC Boost::nowide)

  target_compile_definitions(
    Common
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_WINDOWS
           NOMINMAX
           WIN32_LEAN_AND_MEAN)
endif()

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(Common PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
