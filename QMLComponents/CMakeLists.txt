# This will build the QMLComponents library

list(APPEND CMAKE_MESSAGE_CONTEXT QMLComponents)

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
file(GLOB_RECURSE QML_FILES RELATIVE "${CMAKE_CURRENT_LIST_DIR}" "components/JASP/Controls/*.qml")

qt_add_qml_module(
	QMLComponents
	STATIC
	URI						JASP.Controls
	OUTPUT_DIRECTORY		"JASP/Controls"
	RESOURCE_PREFIX			/jasp-stats.org/imports
	DEPENDENCIES			QtQuick
	SOURCES					${SOURCE_FILES} ${HEADER_FILES}
	QML_FILES				${QML_FILES}
)

# This define is needed for the linking
target_compile_definitions(QMLComponents PUBLIC QT_STATICPLUGIN)

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

target_compile_definitions(QMLComponents PUBLIC JASP_USES_QT_HERE)

target_include_directories(
	QMLComponents
	PUBLIC # JASP
	${PROJECT_SOURCE_DIR}/Common
	${PROJECT_SOURCE_DIR}/CommonData
	${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
	${CMAKE_CURRENT_LIST_DIR}/controls
	${CMAKE_CURRENT_LIST_DIR}/rsyntax
	${CMAKE_CURRENT_LIST_DIR}/ALTNavigation
)


target_link_libraries(
	QMLComponents
	PUBLIC
	Common
	CommonData
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
