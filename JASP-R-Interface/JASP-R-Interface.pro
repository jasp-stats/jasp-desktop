QT -= gui

CURRENT_R_VERSION = 3.3

windows:CONFIG += c++11
macx:CONFIG += c++11
linux:CONFIG += c++11

TARGET = JASP-R-Interface
DESTDIR = ..
TEMPLATE = lib

_R_HOME = $$(R_HOME)

 ! isEmpty(_R_HOME) : message(using R_HOME of $$_R_HOME)

macx {
    isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
}

linux {
    isEmpty(_R_HOME):_R_HOME = /usr/lib/R
}

windows {
    isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../R
	message(QT_ARCH $$QT_ARCH)
	contains(QT_ARCH, i386) {
		ARCH = i386
	} else {
		ARCH = x64
	}
}
message(using R_HOME of $$_R_HOME)

INCLUDEPATH += ../../boost_1_64_0

DEFINES += JASP_R_INTERFACE_LIBRARY

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += \
    jasprcpp.cpp \
    RInside/MemBuf.cpp \
    RInside/RInside.cpp

HEADERS += \
    jasprcpp_interface.h \
    jasprcpp.h \
    RInside/Callbacks.h \
    RInside/MemBuf.h \
    RInside/RInside.h \
    RInside/RInsideAutoloads.h \
    RInside/RInsideCommon.h \
    RInside/RInsideConfig.h \
    RInside/RInsideEnvVars.h

unix {
	target.path = /usr/lib
	INSTALLS += target
}

INCLUDEPATH += \
    $$_R_HOME/library/Rcpp/include \
    $$_R_HOME/include

linux:INCLUDEPATH += \
    $$_R_HOME/site-library/Rcpp/include \
    /usr/share/R/include

macx:LIBS += \
    -L$$_R_HOME/lib -lR

linux:LIBS += \
    -L$$_R_HOME/lib -lR \
    -lrt

win32:LIBS += \
    -L$$_R_HOME/bin/$$ARCH -lR

