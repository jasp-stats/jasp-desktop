# Configures and builds JASP executable.
#
# Notes:
#   - There are way too many Qt libraries are linked, some are not necessary; however,
#     this is because `qtdeployqt` is not doing a great job at collecting and deploying
#     all the necessary libraries. So, if I don't include some of those, the deployment
#     process fails to create a proper package!
#   - We can either add the resources using the `.qrc` files, or ask CMake to
#     build them for us based on the directory that they are in. This can be
#     simplified with some GLOBing, then we can just create a new files, and
#     don't worry about keeping the `.qrc` files up-to-date. Alternatively, we can
#     maintain the QRC files and add them like this.
#
#     set(RESOURCE_FILES
#         html/html.qrc
#         resources/icons.qrc
#         resources/resources.qrc
#         qml.qrc)
#
list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

if(APPLE)
  configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Info.plist.in
                 ${CMAKE_BINARY_DIR}/Info.plist)
endif()

configure_file(${CMAKE_CURRENT_LIST_DIR}/gui/githubpat.h.in
               ${CMAKE_CURRENT_LIST_DIR}/gui/githubpat.h)
message(STATUS "githubpat.h is successfully generated...")


file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(REMOVE_ITEM SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/container_permission_checker/main.cpp")

file(GLOB_RECURSE BUNDLE_RESOURCES "${CMAKE_SOURCE_DIR}/Resources/*")

qt_add_executable(
  JASP
  ${SYSTEM_TYPE}
  ${SOURCE_FILES}
  ${HEADER_FILES}
  ${BUNDLE_RESOURCES}
  $<$<PLATFORM_ID:Windows>:${CMAKE_CURRENT_LIST_DIR}/icon.rc>
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)

set(
    QML_IMPORT_PATH
	"${PROJECT_SOURCE_DIR}/Desktop/components" 
	"${PROJECT_SOURCE_DIR}/QMLComponents/components"
    CACHE
    PATH
	"Path used to locate QML modules by Qt Creator"
    FORCE
)


if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(JASP PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                        ${IWYU_EXECUTABLE})
endif()


add_custom_command(
  TARGET JASP
  POST_BUILD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy_directory Resources/
          ${CMAKE_BINARY_DIR}/Resources
  BYPRODUCTS ${CMAKE_BINARY_DIR}/Resources
  COMMENT "------ Updating the Resources")

add_dependencies(JASP JASPEngine)

if(WINDOWS)
  # this little program checks for file permissions
  message(STATUS "We will build the windows app container permission checker")
  add_executable(ContainerFilePermissionChecker "${CMAKE_CURRENT_LIST_DIR}/container_permission_checker/main.cpp")
  add_dependencies(JASP ContainerFilePermissionChecker)

  set_target_properties(JASP PROPERTIES OUTPUT_NAME "JASPDesktop") # https://github.com/jasp-stats/jasp-issues/issues/1030
endif()

if(INSTALL_R_MODULES AND NOT FLATPAK_USED)
  add_dependencies(JASP Modules)
  if(NOT LINUX)
    add_dependencies(JASP jags)
  endif()
endif()

if(LOCAL_MODULES_BUILD)
  add_dependencies(JASP ModulesBuild)
endif()

if(APPLE)
  add_dependencies(JASP readstat)
endif()

if(WINDOWS)
  # This is added to the build folder that JASP doesn't try to recreate the
  # junctions in the build folder, they are there anyway!
  execute_process(
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E touch "junctions-recreated-successfully.log")
endif()

message(STATUS "LIBRDATA_INCLUDE_DIRS:    ${LIBRDATA_INCLUDE_DIRS}")
message(STATUS "LIBREADSTAT_INCLUDE_DIRS: ${LIBREADSTAT_INCLUDE_DIRS}")

target_include_directories(
    JASP
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
    ${PROJECT_SOURCE_DIR}/Common
    ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
    ${PROJECT_SOURCE_DIR}/CommonData
    ${PROJECT_SOURCE_DIR}/QMLComponents
    ${LIBREADSTAT_INCLUDE_DIRS}
    ${LIBRDATA_INCLUDE_DIRS}
    # JSONCPP
    $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_INCLUDEDIR}>
    $<$<BOOL:${FLATPAK_USED}>:/app/include/QtCore5Compat>
    $<$<BOOL:${FLATPAK_USED}>:/app/include/QtWebEngineQuick>
    $<$<BOOL:${FLATPAK_USED}>:/app/include/QtWebEngineCore>
    ${LIBFREEXL_INCLUDE_DIRS}
)

target_link_libraries(
    JASP
	PUBLIC
	QMLComponentsplugin
    CommonData
    Common
    $<$<PLATFORM_ID:Darwin>:compression>
    Qt::Core
    Qt::Gui
    Qt::OpenGL
    Qt::Widgets
    Qt::Qml
    Qt::WebEngineQuick
    Qt::WebChannel
    Qt::WebChannelQuick
    Qt::Svg
    Qt::Network
	#? Qt::PrintSupport
    Qt::Xml
    Qt::Sql
    Qt::Quick
    Qt::QuickLayouts
    Qt::QuickControls2
    Qt::QuickControls2Impl
    Qt::QuickWidgets
    $<$<NOT:$<BOOL:${FLATPAK_USED}>>:Qt::Core5Compat>
    $<$<BOOL:${FLATPAK_USED}>:/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/libQt6Core5Compat.so>
    Qt::QuickTemplates2
    Qt::DBus
    Qt::QmlWorkerScript
    Qt::LabsFolderListModel
    # System
    ZLIB::ZLIB
    Iconv::Iconv
    OpenSSL::SSL
    OpenSSL::Crypto
    # FreeXL
    ${LIBFREEXL_LIBRARIES}
    $<$<NOT:$<BOOL:${LINUX}>>:freexl::freexl>
    # ReadStat -----------------------------------
    ${LIBREADSTAT_LIBRARIES}
    # Rtools's ReadStat
    $<$<PLATFORM_ID:Windows>:${RTOOLS_LIBREADSTAT_DLL_A}>
    # librdata -----------------------------------
    ${LIBRDATA_LIBRARIES}
    $<$<PLATFORM_ID:Windows>:${RTOOLS_LIBRDATA_DLL_A}>
    $<$<PLATFORM_ID:Darwin>:librdata::librdata>
    # JSONCPP
         #$<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LIBRARIES}>
         #$<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LINK_LIBRARIES}>
    # R-Framework --------------------------------
    $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
)

target_compile_definitions(JASP PUBLIC JASP_USES_QT_HERE)

if(NOT WINDOWS)
  target_compile_options(JASP PUBLIC -Wno-deprecated-declarations)

  target_link_libraries(
    JASP
	PRIVATE
    # When using Conan, we need to provide the static libraries directly, for some reason!
    ${Iconv_LIBRARY_LIST})
endif()

if(LINUX)
  if(LINUX_LOCAL_BUILD)
    target_compile_definitions(JASP PUBLIC LINUX_LOCAL_BUILD)
  endif()

  if(FLATPAK_USED)
    target_compile_definitions(JASP PUBLIC FLATPAK_USED)
  endif()
endif()

file(GLOB_RECURSE HTML_RESOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/html/*")
qt_add_resources(
  JASP
  "html"
  PREFIX
  "/html"
  BASE
  "${CMAKE_CURRENT_LIST_DIR}/html"
  FILES
  ${HTML_RESOURCE_FILES})

file(GLOB_RECURSE TRANSLATION_RESOURCE_FILES
     "${CMAKE_CURRENT_LIST_DIR}/translations/*.png")
qt_add_resources(
  JASP
  "translations"
  PREFIX
  "/translations"
  BASE
  "${CMAKE_CURRENT_LIST_DIR}/translations"
  FILES
  ${TRANSLATION_RESOURCE_FILES})

file(GLOB_RECURSE QML_RESOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/components/*")
qt_add_resources(
  JASP
  "widgetsandtheme"
  PREFIX
  "/"
  BASE
  ${CMAKE_CURRENT_LIST_DIR}
  FILES
  ${QML_RESOURCE_FILES})

# and some random stuff
set(RESOURCES_RESOURCE_FILES
    "resources/../modules/upgrader/upgrades.json"
    "resources/fonts/FiraCode-Retina.ttf" "resources/fonts/FreeSans.ttf")

qt_add_resources(
  JASP
  "resources"
  PREFIX
  "/"
  BASE
  "${CMAKE_CURRENT_LIST_DIR}/resources"
  FILES
  ${RESOURCES_RESOURCE_FILES})

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
