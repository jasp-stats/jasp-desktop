# This will build the RJaspInterface shared library

list(APPEND CMAKE_MESSAGE_CONTEXT SyntaxInterface)

file(GLOB_RECURSE HEADER_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.cpp")

if(WIN32)
	add_library(SyntaxInterface SHARED ${SOURCE_FILES} ${HEADER_FILES})
else()
	add_library(SyntaxInterface STATIC ${SOURCE_FILES} ${HEADER_FILES})
endif()

#add_dependencies(CommonData QMLComponents)

target_include_directories(
	SyntaxInterface
	PUBLIC
		${PROJECT_SOURCE_DIR}/Common
		${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
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

target_compile_definitions(
	SyntaxInterface
	PUBLIC
	SYNTAX_INTERFACE_LIBRARY
	JASP_R_INTERFACE_LIBRARY
	)


list(POP_BACK CMAKE_MESSAGE_CONTEXT)
