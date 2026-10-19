# This will build the CommonData library
#
# On Linux,
#   - We need to link to the `librt.so`, and for some reason, CMake cannot find it
#     easily, so, Libraries module tries to find it and provides it to CommonData here.
#   - Since we are not using Conan, I link to the libjsoncpp, using the PkgConfig variables
#
#
list(APPEND CMAKE_MESSAGE_CONTEXT CommonData)

make_includable(internalDbDefinition.sql  internalDbDefinition.h)
make_includable(createIndexes.sql		  createIndexes.h)

make_includable(../Engine/jaspBase/R/distributionSamplers.R				jaspBase_distributionSamplers.h)
make_includable(../Engine/jaspBase/R/friendlyConstructorFunctions.R		jaspBase_friendlyConstructorFunctions.h)
make_includable(../Engine/jaspBase/R/transformFunctions.R				jaspBase_transformFunctions.h)
make_includable(../Engine/jaspBase/R/transformBoxCox.R					jaspBase_transformBoxCox.h)
make_includable(../Engine/jaspBase/R/transformJohnson.R					jaspBase_transformJohnson.h)
make_includable(../Engine/jaspBase/R/transformYeoJohnson.R				jaspBase_transformYeoJohnson.h)
make_includable(../Engine/jaspBase/R/transformPower.R					jaspBase_transformPower.h)


file(GLOB_RECURSE HEADER_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES	"${CMAKE_CURRENT_LIST_DIR}/*.cpp")
file(GLOB_RECURSE SQL_FILES		"${CMAKE_CURRENT_LIST_DIR}/*.sql")

file(GLOB JASPRESULTS_HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspBase/src/*.h")
file(GLOB JASPRESULTS_SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspBase/src/*.cpp")

# this target is not compiled and only exists so that the sources and headers of jaspResults show up in the project and can be opened with Ctrl+k
add_custom_target(jaspResults ALL SOURCES ${JASPRESULTS_SOURCE_FILES} ${JASPRESULTS_HEADER_FILES})


add_library(CommonData STATIC ${SOURCE_FILES} ${HEADER_FILES} ${SQL_FILES})

#add_dependencies(CommonData R-Interface)

target_include_directories(
	CommonData
	PUBLIC # JASP
	${PROJECT_SOURCE_DIR}/R-Interface
	${PROJECT_SOURCE_DIR}/Common
	# R
	${R_INCLUDE_PATH}
	${R_HOME_PATH}/include
	${RCPP_PATH}/include
	${Boost_INCLUDE_DIRS}
	PRIVATE
	SQLite::SQLite3
	)

target_link_libraries(
    CommonData
    PUBLIC
	Qt::Core
	Qt::Gui
    Qt::Qml
    Qt::Sql
    Qt::Quick
    LibArchive::LibArchive
    SQLite::SQLite3
	PRIVATE
	$<$<PLATFORM_ID:Windows>:${_LIB_R_INTERFACE_STATIC_DLL_A}>
	$<$<NOT:$<PLATFORM_ID:Windows>>:R-Interface>

)
if(MSVC)
  target_compile_options(CommonData PRIVATE /EHsc)
endif()
target_compile_definitions(
  CommonData
  PUBLIC 
  $<$<BOOL:${JASP_PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>
  BUILDING_JASP_ENGINE R_HOME=${R_HOME_PATH}
  $<$<BOOL:${JASP_TIMER_USED}>:PROFILE_JASP>
  JSONCPP_NO_LOCALE_SUPPORT	
  JASP_USES_QT_HERE
)

if(APPLE OR WINDOWS)
  target_compile_definitions(CommonData PUBLIC BOOST_INTERPROCESS_SHARED_DIR_FUNC)
endif()

if(WINDOWS)
  target_compile_definitions(
    CommonData
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_WINDOWS
           NOMINMAX
           WIN32_LEAN_AND_MEAN)
endif()

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(CommonData PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
