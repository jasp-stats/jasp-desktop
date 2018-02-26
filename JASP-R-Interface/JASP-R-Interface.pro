QT -= gui

include(../JASP.pri)

CONFIG += c++11
TARGET = $$JASP_R_INTERFACE_NAME
DESTDIR = ..
TEMPLATE = lib
unix:CONFIG += staticlib

QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.a'

#comment this out if you do not want helpertraces for development of jaspResults and such
#CONFIG(debug, debug|release) {  DEFINES+=JASP_RESULTS_DEBUG_TRACES }

windows:QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.lib' $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.dll'

macx: QMAKE_CLEAN +=$$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.dylib'

include(../R_HOME.pri)
INCLUDEPATH += ../../boost_1_64_0
INCLUDEPATH += ../JASP-Common

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
    rinside_consolelogging.cpp \
    jaspResults/src/jaspHtml.cpp \
    jaspResults/src/jaspObject.cpp \
    jaspResults/src/jaspJson.cpp \
    jaspResults/src/jaspContainer.cpp \
    jaspResults/src/jaspPlot.cpp \
    jaspResults/src/jaspResults.cpp \
    jaspResults/src/jaspTable.cpp \
    jaspResults/src/jaspState.cpp

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
    rinside_consolelogging.h \
    jaspResults/src/jaspHtml.h \
    jaspResults/src/jaspObject.h \
    jaspResults/src/jaspJson.h \
    jaspResults/src/jaspList.h \
    jaspResults/src/jaspContainer.h \
    jaspResults/src/jaspPlot.h \
    jaspResults/src/jaspResults.h \
    jaspResults/src/jaspTable.h \
    jaspResults/src/jaspModuleRegistration.h \
    jaspResults/src/jaspState.h


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
