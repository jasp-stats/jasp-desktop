cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)
# pkg_check_modules(LIBSSL REQUIRED IMPORTED_TARGET libssl)
# pkg_check_modules(LIBCRYPTO REQUIRED IMPORTED_TARGET libcrypto)
pkg_check_modules(LIBZ REQUIRED IMPORTED_TARGET zlib)
pkg_check_modules(LIBREADSTAT REQUIRED IMPORTED_TARGET readstat)

find_package(Boost REQUIRED COMPONENTS nowide filesystem system date_time timer
                                       chrono)
find_package(
  Qt6 REQUIRED
  COMPONENTS Core
             Gui
             OpenGL
             Widgets
             Qml
             WebEngineWidgets
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

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})
include_directories(${LIBARCHIVE_INCLUDE_DIRS})
include_directories(${LIBREADSTAT_INCLUDE_DIRS})

include_directories(${PROJECT_SOURCE_DIR}/Common)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

add_definitions(-DBUILDING_JASP)
add_definitions(-DJASP_USES_QT_HERE)
add_definitions(-DPRINT_ENGINE_MESSAGES)
add_definitions(-Wno-deprecated-declarations)

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

if(CMAKE_GENERATOR STREQUAL "Xcode")

  file(GLOB_RECURSE BUNDLE_RESOURCES "${CMAKE_SOURCE_DIR}/Resources/*")

  # This is a handy little for-loop from here, https://stackoverflow.com/a/66401708/1141307,
  # which keeps all the resource files into the Resources folder while
  # keeping their relative path. So, I don't have to group them manually, and
  # ndividually set the file's path properties.
  foreach(FILE ${BUNDLE_RESOURCES})
    # Get the relative path from the Resources to the particular file.
    file(RELATIVE_PATH NEW_FILE "${CMAKE_SOURCE_DIR}/Resources" ${FILE})

    # Get the relative path to the file.
    get_filename_component(NEW_FILE_PATH ${NEW_FILE} DIRECTORY)

    # Set it's location inside the app package (under Resources).
    set_property(SOURCE ${FILE} PROPERTY MACOSX_PACKAGE_LOCATION
                                         "Resources/${NEW_FILE_PATH}")

    # Optional: Add the file to the 'Resources' folder group in Xcode.
    #           This also preserves folder structure.
    source_group("Resources/${NEW_FILE_PATH}" FILES "${FILE}")
  endforeach()

else()

  # I think we can put some of these files into the JASP binary, e.g., translations, and helps
  file(COPY ${CMAKE_SOURCE_DIR}/Resources
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  message(
    STATUS
      "Successfully copied the Resources files to ${CMAKE_CURRENT_BINARY_DIR}"
  )

endif()

# message(STATUS ${CMAKE_BINARY_DIR})

# set_property(SOURCE ${CMAKE_BINARY_DIR}/Desktop/JASPEngine
#              PROPERTY MACOSX_PACKAGE_LOCATION MacOS
#              )

if(CMAKE_GENERATOR STREQUAL "Xcode")
  qt_add_executable(JASP MACOSX_BUNDLE ${HEADER_FILES} ${SOURCE_FILES}
                    ${BUNDLE_RESOURCES} ${_R_Framework})
else()
  qt_add_executable(JASP ${HEADER_FILES} ${SOURCE_FILES} ${BUNDLE_RESOURCES}
                    ${_R_Framework})
endif()

add_dependencies(JASP JASPEngine)

target_link_libraries(
  JASP
  PUBLIC Common
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
         PkgConfig::LIBREADSTAT
         ${_R_Framework})

set_target_properties(
  JASP PROPERTIES XCODE_ATTRIBUTE_SKIP_INSTALL NO XCODE_ATTRIBUTE_INSTALL_PATH
                                                  "$(LOCAL_APPS_DIR)")

set_target_properties(
  JASP
  PROPERTIES XCODE_EMBED_FRAMEWORKS ${_R_Framework}
             XCODE_EMBED_FRAMEWORKS_CODE_SIGN_ON_COPY TRUE
             XCODE_EMBED_FRAMEWORKS_REMOVE_HEADERS_ON_COPY FALSE)

# set_target_properties(JASP PROPERTIES
#     XCODE_EMBED_APP_EXTENTIONS  JASPEngine
# )

# set_source_files_properties(
#     ${_R_Framework} PROPERTIES
#     MACOSX_PACKAGE_LOCATION "Frameworks"
#     )

# set_target_properties(JASP PROPERTIES
#     RESOURCE "${BUNDLE_RESOURCES_FILES}")

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