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
         ${LIBJSONCPP_INCLUDE_DIRS}
         ${Boost_INCLUDE_DIRS})

target_link_libraries(
  Common
  PUBLIC # jsoncpp
         $<$<BOOL:${USE_CONAN}>:jsoncpp::jsoncpp>
         $<$<NOT:$<BOOL:${USE_CONAN}>>:${LIBJSONCPP_LINK_LIBRARIES}>
         #
         LibArchive::LibArchive
         # Boost
         $<$<PLATFORM_ID:Windows>:Boost::nowide>
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

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(Common PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
endif()

# TODOs:
#
# - [ ] After confirming these, they can all be rewritten as generator-expressions
# - [ ] I don't think most of the Boost stuffs have effect here, and they should
#       probably move into `Dependencies.cmake` where I actually build Boost.
#
if(APPLE)
  target_compile_definitions(Common PUBLIC BOOST_INTERPROCESS_SHARED_DIR_FUNC)
elseif(WIN32)
  target_compile_definitions(
    Common
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)

  # TODO: I have no idea what these are. For now, for Windows build, I'm just
  # translating the Qmake
  #
  # target_link_libraries(Common PUBLIC ole32 oleaut32)
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
