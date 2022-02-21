list(APPEND CMAKE_MESSAGE_CONTEXT Common)

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "appinfo.cpp is successfully generated...")

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp)

add_library(Common ${HEADER_FILES} ${SOURCE_FILES})

if(INSTALL_JASP_REQUIRED_LIBRARIES)
  add_dependencies(Common jsoncpp-install)
endif()

target_include_directories(
  Common
  PUBLIC ${LIBJSONCPP_INCLUDE_DIRS}
         ${CONAN_INCLUDE_DIRS_libjson}
         # JASP
         jaspColumnEncoder
         # R
         ${R_INCLUDE_PATH}
         ${R_HOME_PATH}/include
         ${RCPP_PATH}/include
         # Conan
         ${CONAN_INCLUDE_DIRS_JSONCPP}
         ${CONAN_INCLUDE_DIRS_LIBARCHIVE}
         #
         ${Boost_INCLUDE_DIRS})

target_link_libraries(
  Common
  PUBLIC Boost::nowide
         Boost::filesystem
         Boost::system
         Boost::date_time
         Boost::timer
         Boost::chrono
         # Conan
         # LibArchive::LibArchive
         # ${LIBJSONCPP_LINK_LIBRARIES}
         # ${CONAN_LIBS_libarchive}
         # ${CONAN_LIBS_jsoncpp}
         $<$<BOOL:${USE_CONAN}>:CONAN_PKG::jsoncpp>
         $<$<BOOL:${USE_CONAN}>:CONAN_PKG::libarchive>
         LibArchive::LibArchive
         ${LIBJSONCPP_LINK_LIBRARIES}
         # Only if JASP_USES_QT_HERE is ON
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

# I'm leaving the flatpak config behind, as it should be pretty straighforward,
# and I think CMake can take care of most of it

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
