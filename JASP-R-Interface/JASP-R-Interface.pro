QT -= gui

include(../JASP.pri) 

CONFIG += c++11
TARGET = $$JASP_R_INTERFACE_NAME
DESTDIR = ..
TEMPLATE = lib
linux:CONFIG += staticlib

QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.a'

windows:QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.lib' $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.dll'

macx: QMAKE_CLEAN +=$$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.dylib'


macx {
    isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
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
    RInside/RInside.cpp \
    rinside_consolelogging.cpp

HEADERS += \
    jasprcpp_interface.h \
    jasprcpp.h \
    RInside/Callbacks.h \
    RInside/MemBuf.h \
    RInside/RInside.h \
    RInside/RInsideAutoloads.h \
    RInside/RInsideCommon.h \
    RInside/RInsideConfig.h \
    RInside/RInsideEnvVars.h \
    rinside_consolelogging.h

INCLUDEPATH += \
    $$_R_HOME/library/Rcpp/include \
    $$_R_HOME/include


macx:LIBS += \
    -L$$_R_HOME/lib -lR

win32:LIBS += \
    -L$$_R_HOME/bin/$$ARCH -lR

windows{
	SOURCE_LIBFILE = $$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_NAME'.a'
    SOURCE_LIBFILE ~= s,/,\\,g
	DEST_LIBFILE = $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_NAME'.lib'
    DEST_LIBFILE ~= s,/,\\,g
    copyfile.commands += $$quote(cmd /c copy /Y $$SOURCE_LIBFILE $$DEST_LIBFILE)

    first.depends = $(first) copyfile
    export(first.depends)
    export(copyfile.commands)
    QMAKE_EXTRA_TARGETS += first copyfile
 }

macx{
	setpath.commands += install_name_tool -id @rpath/lib$$JASP_R_INTERFACE_NAME'.1.0.0.dylib' $$OUT_PWD/$$DESTDIR/lib$$JASP_R_INTERFACE_NAME'.1.0.0.dylib'
	first.depends = $(first) setpath
	export(first.depends)
	export(setpath.commands)
	QMAKE_EXTRA_TARGETS += first setpath
}


