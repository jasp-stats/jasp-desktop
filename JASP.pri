#Used for common JASP qmake build settings

#Jasp-R-Interface
JASP_R_INTERFACE_TARGET = JASP-R-Interface

JASP_R_INTERFACE_MAJOR_VERSION = 3 # Interface changes
JASP_R_INTERFACE_MINOR_VERSION = 1 # Code changes

JASP_R_INTERFACE_NAME = $$JASP_R_INTERFACE_TARGET$$JASP_R_INTERFACE_MAJOR_VERSION'.'$$JASP_R_INTERFACE_MINOR_VERSION

#R settings
CURRENT_R_VERSION = 3.4
DEFINES += "CURRENT_R_VERSION=\"$$CURRENT_R_VERSION\""

#JASP Version
JASP_VERSION_MAJOR      = 0
JASP_VERSION_MINOR      = 10
JASP_VERSION_REVISION   = 0
JASP_VERSION_BUILD      = 1 #Should be incremented or retrieved from somewhere

DEFINES +=    "JASP_VERSION_MAJOR=$$JASP_VERSION_MAJOR"
DEFINES +=    "JASP_VERSION_MINOR=$$JASP_VERSION_MINOR"
DEFINES += "JASP_VERSION_REVISION=$$JASP_VERSION_REVISION"
DEFINES +=    "JASP_VERSION_BUILD=$$JASP_VERSION_BUILD"


BUILDING_JASP_ENGINE=false
DEFINES += PRINT_ENGINE_MESSAGES

#macx | windows | exists(/app/lib/*) {
#	message(using libjson static)
  DEFINES += JASP_LIBJSON_STATIC # lets just always use libjson-static, they keep moving the include files...
#} else {
#    linux {
#        message(using libjson from distro and pkgconfig)
#        QT_CONFIG -= no-pkg-config
#        CONFIG += link_pkgconfig
#        PKGCONFIG += jsoncpp
#        LIBS += -ljsoncpp
#
#        CONFIG(debug, debug|release) {  DEFINES+=JASP_DEBUG }
#    }
#}

exists(/app/lib/*) {
  linux:  DEFINES += FLATPAK_USED
} else {
  linux:	CONFIG(debug, debug|release) {  DEFINES+=JASP_DEBUG }
}
macx | windows { CONFIG(debug, debug|release) {  DEFINES+=JASP_DEBUG } }


windows {
	message(QT_ARCH $$QT_ARCH)
	contains(QT_ARCH, i386) {
		ARCH = i386
	} else {
		ARCH = x64
	}
}

unix: QMAKE_CXXFLAGS += -Werror=return-type
