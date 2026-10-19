# This will build the interface for the jaspSyntax R package

list(APPEND CMAKE_MESSAGE_CONTEXT SyntaxInterface)

file(GLOB_RECURSE HEADER_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.cpp")

add_library(SyntaxInterface SHARED ${SOURCE_FILES} ${HEADER_FILES})
qt_add_resources(SyntaxInterface "Dummy" FILES Dummy.qml)

#add_dependencies(CommonData QMLComponents)

target_include_directories(
	SyntaxInterface
	PUBLIC
		${PROJECT_SOURCE_DIR}/Common
		${PROJECT_SOURCE_DIR}/CommonData
		${PROJECT_SOURCE_DIR}/QMLComponents
		${PROJECT_SOURCE_DIR}/R-Interface
	)

target_link_libraries(
	SyntaxInterface
	PUBLIC
		Common
		CommonData
		$<$<PLATFORM_ID:Windows>:${_LIB_R_INTERFACE_NORINSIDE_STATIC_DLL_A}>
		$<$<NOT:$<PLATFORM_ID:Windows>>:R-Interface>
		QMLComponentsplugin
		Qt::Core
		Qt::Quick
		Qt::QuickControls2
)

if(USE_QT_STATIC_LIBS)
	target_link_libraries(
		SyntaxInterface
		PUBLIC
		$<$<PLATFORM_ID:Windows>:${CMAKE_PREFIX_PATH}/plugins/platforms/qminimal.lib>
		$<$<NOT:$<PLATFORM_ID:Windows>>:${CMAKE_PREFIX_PATH}/plugins/platforms/libqminimal.a>
	)
endif()

target_compile_definitions(
	SyntaxInterface
	PRIVATE
	SYNTAX_INTERFACE_LIBRARY
	JASP_R_INTERFACE_LIBRARY
	)

if(MSVC)
	target_compile_options(SyntaxInterface PRIVATE /EHsc)
endif()

if(USE_QT_STATIC_LIBS)
	target_compile_definitions(
		SyntaxInterface
		PRIVATE
		USE_QT_STATIC_LIBS
	)
endif()


qt_import_qml_plugins(SyntaxInterface)

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
