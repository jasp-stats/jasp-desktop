# This will build the Common library
#
# On Linux,
#   - We need to link to the `librt.so`, and for some reason, CMake cannot find it
#     easily, so, Libraries module tries to find it and provides it to Common here.
#   - Since we are
#
#
list(APPEND CMAKE_MESSAGE_CONTEXT Common)

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp)

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

if(WINDOWS)
  target_link_libraries(Common PUBLIC Boost::nowide)
endif()

target_compile_definitions(
  Common PUBLIC $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
                $<$<BOOL:${JASP_TIMER_USED}>:PROFILE_JASP>)

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(Common PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
endif()

if(APPLE)
  target_compile_definitions(Common PUBLIC BOOST_INTERPROCESS_SHARED_DIR_FUNC)
elseif(WIN32)
  target_compile_definitions(
    Common
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_WINDOWS
           NOMINMAX
           WIN32_LEAN_AND_MEAN)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
