#Used for common JASP qmake build settings

#Jasp-R-Interface
JASP_R_INTERFACE_TARGET = JASP-R-Interface
JASP_R_INTERFACE_MAJOR_VERSION = 1 # Interface changes
JASP_R_INTERFACE_MINOR_VERSION = 0 # Code changes
JASP_R_INTERFACE_NAME = $$JASP_R_INTERFACE_TARGET$$JASP_R_INTERFACE_MAJOR_VERSION'.'$$JASP_R_INTERFACE_MINOR_VERSION

#R settings
CURRENT_R_VERSION = 3.4
DEFINES += "CURRENT_R_VERSION=\"$$CURRENT_R_VERSION\""
BUILDING_JASP_ENGINE=false


macx | windows | exists(/app/lib/*) { DEFINES += JASP_LIBJSON_STATIC } else
{
    linux {
        CONFIG += link_pkgconfig
        PKGCONFIG += jsoncpp
        LIBS += -ljsoncpp
    }
}

exists(/app/lib/*)  {} else {
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

