list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

find_package(PkgConfig REQUIRED)
find_package(LibArchive REQUIRED)

pkg_check_modules(
  LIBJSONCPP
  REQUIRED
  IMPORTED_TARGET
  jsoncpp)

find_package(ZLIB)
find_package(Iconv)

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)
  pkg_check_modules(
    LIBREADSTAT
    REQUIRED
    IMPORTED_TARGET
    readstat)
else()
  find_library(
    LIBREADSTAT
    NAMES readstat
    HINTS ${readstat_LIBRARY_DIRS})

  set(LIBREADSTAT_INCLUDE_DIRS ${readstat_INCLUDE_DIRS})
  set(LIBREADSTAT_LIBRARY_DIRS ${readstat_LIBRARY_DIRS})
  set(LIBREADSTAT_LINK_LIBRARIES ${LIBREADSTAT})
endif()

find_package(OpenSSL REQUIRED COMPONENTS SSL Crypto)

find_package(
  Boost REQUIRED
  COMPONENTS nowide
             filesystem
             system
             date_time
             timer
             chrono)

find_package(
  Qt6 REQUIRED
  COMPONENTS Core
             Gui
             OpenGL
             Widgets
             Qml
             WebEngineWidgets # remove this after the rebase
             WebEngineQuick
             WebChannel
             Svg
             Network
             PrintSupport
             Xml
             Quick
             QuickControls2
             QuickWidgets
             Core5Compat)

set(MACOSX_ICON_PATH ${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns)
configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Info.plist.template
               ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)

# We can technically use these two lines to grab all *.h, and *.cpp files
# but it think there are unused files.

file(GLOB_RECURSE HEADER_FILES "${CMAKE_CURRENT_LIST_DIR}/*.h")
list(REMOVE_ITEM HEADER_FILES
     ${CMAKE_CURRENT_LIST_DIR}/data/exporters/odsexporter.h)

file(GLOB_RECURSE SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/*.cpp")
list(REMOVE_ITEM SOURCE_FILES
     ${CMAKE_CURRENT_LIST_DIR}/data/exporters/odsexporter.cpp)

file(GLOB_RECURSE BUNDLE_RESOURCES "${CMAKE_SOURCE_DIR}/Resources/*")

# NOTE:
#   The resources has to be added and managed before the creation of the
#   JASP target
if(CMAKE_GENERATOR STREQUAL "Xcode")

  # This is a handy little for-loop from here, https://stackoverflow.com/a/66401708/1141307,
  # which keeps all the resource files into the Resources folder while
  # keeping their relative path. So, I don't have to group them manually, and
  # ndividually set the file's path properties.
  foreach(FILE ${BUNDLE_RESOURCES})
    # Get the relative path from the Resources to the particular file.
    file(
      RELATIVE_PATH
      NEW_FILE
      "${CMAKE_SOURCE_DIR}/Resources"
      ${FILE})

    # Get the relative path to the file.
    get_filename_component(NEW_FILE_PATH ${NEW_FILE} DIRECTORY)

    # Set it's location inside the app package (under Resources).
    set_property(SOURCE ${FILE} PROPERTY MACOSX_PACKAGE_LOCATION
                                         "Resources/${NEW_FILE_PATH}")

    # Add the file to the 'Resources' folder group in Xcode. This also preserves folder structure.
    source_group("Resources/${NEW_FILE_PATH}" FILES "${FILE}")
  endforeach()

  set_property(SOURCE JASPEngine PROPERTY MACOSX_PACKAGE_LOCATION "MacOS")

  add_executable(
    JASP MACOSX_BUNDLE
    ${HEADER_FILES}
    ${SOURCE_FILES}
    ${BUNDLE_RESOURCES}
    ${_R_Framework})

  # This influences some of the #ifdef's
  target_compile_definitions(JASP PUBLIC MACOSX_BUNDLE)

else()

  qt_add_executable(
    JASP
    ${SYSTEM_TYPE}
    ${HEADER_FILES}
    ${SOURCE_FILES}
    ${BUNDLE_RESOURCES}
    ${_R_Framework})

  if(IWYU AND RUN_IWYU)
    set_target_properties(JASP PROPERTIES CXX_INCLUDE_WHAT_YOU_USE ${IWYU})
  endif()

  add_custom_target(
    Resources
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMAND cp -r Resources ${CMAKE_BINARY_DIR}
    BYPRODUCTS ${CMAKE_BINARY_DIR}/Resources)

  add_dependencies(JASP Resources)

endif()

target_compile_definitions(JASP PUBLIC JASP_USES_QT_HERE)

target_compile_options(JASP PUBLIC -Wno-deprecated-declarations)

add_dependencies(JASP JASPEngine)

if(INSTALL_R_MODULES)
  add_dependencies(JASP Modules)
endif()

if(INSTALL_JASP_REQUIRED_LIBRARIES)
  add_dependencies(JASP readstat-install)
endif()

target_include_directories(
  JASP
  PUBLIC ${Boost_INCLUDE_DIRS}
         ${LIBJSONCPP_INCLUDE_DIRS}
         ${LibArchive_INCLUDE_DIRS}
         ${LIBREADSTAT_INCLUDE_DIRS}
         ${PROJECT_SOURCE_DIR}/Common
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

if(APPLE)

  target_link_libraries(
    JASP
    PUBLIC
      Common
      Qt::Core
      Qt::Gui
      Qt::OpenGL
      Qt::Widgets
      Qt::Qml
      Qt::WebEngineWidgets
      Qt::WebEngineQuick
      Qt::WebChannel
      Qt::Svg
      Qt::Network
      Qt::PrintSupport
      Qt::Xml
      Qt::Quick
      Qt::QuickControls2
      Qt::QuickWidgets
      Qt::Core5Compat
      # Based on our config, this is either from PkgConfig, or the build/_deps/
      ${LIBREADSTAT_LINK_LIBRARIES}
      ${Iconv_LIBRARIES}
      ${_R_Framework})

elseif(WIN32)
  # As far as I understood, on Windows we don't need to do a lot.
  # I only want to remove the _R_Framework explicitely. But, as we
  # progress that can be dealt with with a variable.
else() # Linux

endif()

set_target_properties(
  JASP PROPERTIES XCODE_ATTRIBUTE_SKIP_INSTALL NO XCODE_ATTRIBUTE_INSTALL_PATH
                                                  "$(LOCAL_APPS_DIR)")

set_target_properties(
  JASP
  PROPERTIES XCODE_EMBED_FRAMEWORKS ${_R_Framework}
             XCODE_EMBED_FRAMEWORKS_CODE_SIGN_ON_COPY TRUE
             XCODE_EMBED_FRAMEWORKS_REMOVE_HEADERS_ON_COPY FALSE)

# Amir:
# We can either add the resources using the `.qrc` files, or ask CMake to
# build them for us based on the directory that they are in. This can be
# simplified with some GLOBing, then we can just create a new files, and
# don't worry about keeping the `.qrc` files up-to-date.
# Alternatively, we can maintain the QRC files and add them like this,
#    set(RESOURCE_FILES
#        html/html.qrc
#        resources/icons.qrc
#        resources/resources.qrc
#        qml.qrc
#    )

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
  "qml"
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
