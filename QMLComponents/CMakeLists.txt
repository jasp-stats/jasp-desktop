# This will build the QMLComponents library

list(APPEND CMAKE_MESSAGE_CONTEXT QMLComponents)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
file(GLOB_RECURSE QML_FILES RELATIVE "${CMAKE_CURRENT_LIST_DIR}" "components/JASP/Controls/*.qml")

qt_add_qml_module(QMLComponents
	URI JASP.Controls
	#STATIC
	OUTPUT_DIRECTORY "JASP/Controls"
	RESOURCE_PREFIX /jasp-stats.org/imports
	#PLUGIN_TARGET QMLComponents
	DEPENDENCIES QtQuick
	#NO_PLUGIN_OPTIONAL
	NO_GENERATE_PLUGIN_SOURCE
	CLASS_NAME JASPQmlPlugin
	SOURCES ${SOURCE_FILES} ${HEADER_FILES}
	QML_FILES ${QML_FILES}
)

if(LINUX)
  target_link_libraries(QMLComponents PUBLIC ${_LIB_RT})
  target_link_options(QMLComponents PUBLIC -lrt)
  if(LINUX_LOCAL_BUILD)
    target_compile_definitions(QMLComponents PUBLIC LINUX_LOCAL_BUILD)
  endif()
  if(FLATPAK_USED)
    target_compile_definitions(QMLComponents PUBLIC FLATPAK_USED)
  endif()
endif()

configure_file(${CMAKE_CURRENT_LIST_DIR}/utilities/appdirs.h.in
               ${CMAKE_CURRENT_LIST_DIR}/utilities//appdirs.h)
message(STATUS "appdirs.h is successfully generated...")

target_include_directories(
	QMLComponents
	PUBLIC # JASP
	${PROJECT_SOURCE_DIR}/Common
	${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
	${CMAKE_CURRENT_LIST_DIR}/controls
	${CMAKE_CURRENT_LIST_DIR}/rsyntax
)


target_link_libraries(
	QMLComponents
	PUBLIC
	Common
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

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
