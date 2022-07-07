list(APPEND CMAKE_MESSAGE_CONTEXT CommonQt)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_library(CommonQt ${SOURCE_FILES})

if(LINUX)

  target_link_libraries(QMLForm PUBLIC ${_LIB_RT})
  target_link_options(QMLForm PUBLIC -lrt)

endif()

configure_file(${CMAKE_CURRENT_LIST_DIR}/appdirs.h.in
               ${CMAKE_CURRENT_LIST_DIR}/appdirs.h)
message(STATUS "appdirs.h is successfully generated...")

target_include_directories(
  CommonQt
  PUBLIC # JASP
          ${PROJECT_SOURCE_DIR}/Common
          ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
          ${Boost_INCLUDE_DIRS}
         )


target_link_libraries(
  CommonQt
  PUBLIC
         Qt::Core
         Qt::Widgets
         Qt::Qml
         Qt::Quick
         $<$<NOT:$<BOOL:${FLATPAK_USED}>>:Qt::Core5Compat>
         $<$<BOOL:${FLATPAK_USED}>:/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/libQt6Core5Compat.so>
     )

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
