# This will build the QMLForm library

list(APPEND CMAKE_MESSAGE_CONTEXT QMLForm)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_library(QMLForm ${SOURCE_FILES})

if(LINUX)

  target_link_libraries(QMLForm PUBLIC ${_LIB_RT})
  target_link_options(QMLForm PUBLIC -lrt)

endif()

target_include_directories(
  QMLForm
  PUBLIC # JASP
          ${PROJECT_SOURCE_DIR}/Common
          ${PROJECT_SOURCE_DIR}/CommonQt
          ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
          ${Boost_INCLUDE_DIRS}
         )

target_link_libraries(
  QMLForm
  PUBLIC
         Qt::Core
         Qt::Gui
         Qt::Widgets
         Qt::Qml
         Qt::Quick
         Qt::QuickLayouts
         Qt::QuickControls2
         Qt::QuickControls2Impl
         Qt::QuickWidgets
         Qt::QuickTemplates2
         $<$<NOT:$<BOOL:${FLATPAK_USED}>>:Qt::Core5Compat>
         $<$<BOOL:${FLATPAK_USED}>:/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/libQt6Core5Compat.so>
     )

 file(GLOB_RECURSE QML_RESOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/components/*")
 qt_add_resources(
   QMLForm
   "qml"
   PREFIX
   "/"
   BASE
   ${CMAKE_CURRENT_LIST_DIR}
   FILES
   ${QML_RESOURCE_FILES})

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
