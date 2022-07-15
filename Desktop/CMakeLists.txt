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


file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")

file(GLOB_RECURSE BUNDLE_RESOURCES "${CMAKE_SOURCE_DIR}/Resources/*")

qt_add_executable(
  JASP
  ${SYSTEM_TYPE}
  ${SOURCE_FILES}
  ${HEADER_FILES}
  ${BUNDLE_RESOURCES}
  $<$<PLATFORM_ID:Windows>:${CMAKE_CURRENT_LIST_DIR}/icon.rc>
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>)

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

if(INSTALL_R_MODULES)
  add_dependencies(JASP Modules)
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

target_include_directories(
  JASP
  PUBLIC 	${CMAKE_CURRENT_LIST_DIR}
                        ${PROJECT_SOURCE_DIR}/Common
                        ${PROJECT_SOURCE_DIR}/QMLComponents
                        ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
			# ReadStat
			$<$<PLATFORM_ID:Windows>:${RTOOLS_LIBREADSTAT_H}>
			${LIBREADSTAT_INCLUDE_DIRS}
			# JSONCPP
			$<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_INCLUDEDIR}>
			$<$<BOOL:${FLATPAK_USED}>:/app/include/QtCore5Compat>
			$<$<BOOL:${FLATPAK_USED}>:/app/include/QtWebEngineQuick>
			$<$<BOOL:${FLATPAK_USED}>:/app/include/QtWebEngineCore>
			)

target_link_libraries(
  JASP
  PUBLIC Common
         QMLComponents
         Qt::Core
         Qt::Gui
         Qt::OpenGL
         Qt::Widgets
         Qt::Qml
         $<$<NOT:$<BOOL:${FLATPAK_USED}>>:Qt::WebEngineQuick>
         $<$<BOOL:${FLATPAK_USED}>:/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/libQt6WebEngineQuick.so>
		 $<$<BOOL:${FLATPAK_USED}>:/app/lib/$ENV{FLATPAK_ARCH}-linux-gnu/libQt6WebEngineCore.so>
                 $<$<BOOL:${FLATPAK_USED}>:/app/qml/QtWebEngine/libqtwebenginequickplugin.so>
         Qt::WebChannel
         Qt::Svg
         Qt::Network
         Qt::PrintSupport
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
         # ReadStat -----------------------------------
         ${LIBREADSTAT_LIBRARIES}
         # MinGW's ReadStat
         $<$<PLATFORM_ID:Windows>:${RTOOLS_LIBREADSTAT_DLL_A}>
         # JSONCPP
         $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LIBRARIES}>
         $<$<PLATFORM_ID:Linux>:${_PKGCONFIG_LIB_JSONCPP_LINK_LIBRARIES}>
         # R-Framework --------------------------------
         $<$<PLATFORM_ID:Darwin>:${_R_Framework}>)


target_compile_definitions(JASP PUBLIC JASP_USES_QT_HERE)

if(NOT WINDOWS)
  target_compile_options(JASP PUBLIC -Wno-deprecated-declarations)

  target_link_libraries(
    JASP
    PUBLIC
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

file(GLOB_RECURSE ICONS_RESOURCE_FILES
     "${CMAKE_CURRENT_LIST_DIR}/resources/icons/*")
qt_add_resources(
  JASP
  "icons"
  PREFIX
  "/"
  BASE
  "${CMAKE_CURRENT_LIST_DIR}/resources"
  FILES
  ${ICONS_RESOURCE_FILES})

file(GLOB_RECURSE QML_RESOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/components/*")
qt_add_resources(
  JASP
  "widgets"
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
