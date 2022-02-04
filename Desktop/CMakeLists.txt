list(APPEND CMAKE_MESSAGE_CONTEXT Desktop)

set(CMAKE_BUILD_WITH_INSTALL_RPATH YES)

find_package(LibArchive REQUIRED)
find_package(ZLIB REQUIRED)
find_package(Iconv REQUIRED)

# find_package(OpenSSL REQUIRED COMPONENTS SSL Crypto)

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

if(NOT INSTALL_JASP_REQUIRED_LIBRARIES)

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(
    LIBJSONCPP
    REQUIRED
    IMPORTED_TARGET
    jsoncpp)

  pkg_check_modules(
    LIBREADSTAT
    REQUIRED
    IMPORTED_TARGET
    readstat)

else()

  # find_library(
  #   LIBJSONCPP REQUIRED
  #   NAMES jsoncpp
  #   HINTS ${jsoncpp_LIBRARY_DIRS})

  # WARNING: This is not the way to do it, and it does not work with Ninja :\

  # jsoncpp
  set(LIBJSONCPP_INCLUDE_DIRS ${jsoncpp_INCLUDE_DIRS})
  set(LIBJSONCPP_LIBRARY_DIRS ${jsoncpp_LIBRARY_DIRS})
  set(LIBJSONCPP_LINK_LIBRARIES ${jsoncpp_LIBRARY_DIRS}/libjsoncpp.a)

  # readstat
  set(LIBREADSTAT_INCLUDE_DIRS ${readstat_INCLUDE_DIRS})
  set(LIBREADSTAT_LIBRARY_DIRS ${readstat_LIBRARY_DIRS})
  set(LIBREADSTAT_LINK_LIBRARIES ${LIBREADSTAT_LIBRARY_DIRS}/libreadstat.a)

endif()

# TODO:
#
# - [ ] There is not much to configure here, CMake takes care of the
#       most variable, but for now, I keep it like this. When we are
#       sure that things are working, we can just drop the `.template`
# - [ ] If and when Xcode build works, we can use this in the Info.plist
#       and let Xcode derives it based on our build setting.
#
# <key>LSMinimumSystemVersion</key>
# <string>$(MACOSX_DEPLOYMENT_TARGET)</string>

if(APPLE)
  if(CMAKE_GENERATOR STREQUAL "Xcode")
    configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Info.plist.in
                   ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)
  else()
    configure_file(${CMAKE_SOURCE_DIR}/Tools/macOS/Info.plist.template
                   ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)
  endif()
endif()

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
if((CMAKE_GENERATOR STREQUAL "Xcode") OR ${BUILD_MACOSX_BUNDLE})

  # -- Resources -----------------
  #
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

  qt_add_executable(
    JASP
    MACOSX_BUNDLE
    #
    ${HEADER_FILES}
    ${SOURCE_FILES}
    ${_R_Framework}
    #
    ${BUNDLE_RESOURCES}
    ${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns)

  # This does NOT work, sadly!
  set_source_files_properties(JASPEngine PROPERTIES MACOSX_PACKAGE_LOCATION
                                                    MacOS)

  # Placing the Icon in the right place
  # set_source_files_properties(${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns
  #                             PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

  # When creating an App Bundle, we cannot simply copy paste the
  # Framework into the Bundle, we can embed it, and that sort of
  # works, but not really. It's only at the Archive stage that
  # Xcode decides to copy it, but I don't know who that will look
  # like because I cannot get it working properly.
  set_target_properties(
    JASP
    PROPERTIES XCODE_EMBED_FRAMEWORKS ${_R_Framework}
               XCODE_EMBED_FRAMEWORKS_CODE_SIGN_ON_COPY OFF
               XCODE_EMBED_FRAMEWORKS_REMOVE_HEADERS_ON_COPY OFF)

  # ------------------------------

  set_target_properties(
    JASP
    PROPERTIES MACOSX_BUNDLE_BUNDLE_NAME "${PROJECT_NAME}"
               MACOSX_BUNDLE_BUNDLE_VERSION "${PROJECT_VERSION}"
               MACOSX_BUNDLE_SHORT_VERSION_STRING
               "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}"
               MACOSX_BUNDLE_GUI_IDENTIFIER "org.jasp-stats.jasp"
               MACOSX_BUNDLE_ICON_FILE
               "${CMAKE_SOURCE_DIR}/Tools/macOS/icon.icns"
               MACOSX_BUNDLE_COPYRIGHT "© JASP Team"
               MACOSX_BUNDLE_INFO_PLIST
               "${CMAKE_SOURCE_DIR}/Tools/macOS/Info.plist.in")

else()

  qt_add_executable(
    JASP
    ${SYSTEM_TYPE}
    ${HEADER_FILES}
    ${SOURCE_FILES}
    ${BUNDLE_RESOURCES}
    ${_R_Framework})

  if(IWYU_EXECUTABLE AND RUN_IWYU)
    set_target_properties(JASP PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                          ${IWYU_EXECUTABLE})
  endif()

  add_custom_target(
    Resources
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMAND cp -r Resources ${CMAKE_BINARY_DIR}
    BYPRODUCTS ${CMAKE_BINARY_DIR}/Resources)

  add_dependencies(JASP Resources)

endif()

add_dependencies(JASP JASPEngine)

target_compile_definitions(JASP PUBLIC JASP_USES_QT_HERE)

target_compile_options(JASP PUBLIC -Wno-deprecated-declarations)

if(INSTALL_R_MODULES)
  add_dependencies(JASP Modules)
endif()

if(INSTALL_JASP_REQUIRED_LIBRARIES)
  add_dependencies(JASP readstat-install)
endif()

target_include_directories(
  JASP
  PUBLIC ${LIBJSONCPP_INCLUDE_DIRS}
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
      # OpenSSL::SSL
      # OpenSSL::Crypto
      # Based on our config, this is either from PkgConfig, or the build/_deps/
      ${LIBREADSTAT_LINK_LIBRARIES}
      # System
      ${Iconv_LIBRARIES}
      ${ZLIB_LIBRARIES} # Needed for libreadstat.a
      ${_R_Framework})

elseif(WIN32)
  # As far as I understood, on Windows we don't need to do a lot.
  # I only want to remove the _R_Framework explicitely. But, as we
  # progress that can be dealt with with a variable.
else() # Linux

endif()

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
