QT -= gui core

include(../JASP.pri)

CONFIG    += c++11
TARGET     = $$JASP_R_INTERFACE_NAME
DESTDIR    = ..
TEMPLATE   = lib

#comment this out if you do not want helpertraces for development of jaspResults and such
#CONFIG(debug, debug|release) {  DEFINES+=JASP_RESULTS_DEBUG_TRACES }

include(../R_HOME.pri)

unix{
  CONFIG      += staticlib
  QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.a'
}

windows{
  QMAKE_CLEAN += $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.lib' $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.dll'
  LIBS        += -L$$_R_HOME/bin/$$ARCH -lR
}

macx: QMAKE_CLEAN +=$$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.dylib'

INCLUDEPATH += ../Common RInside
DEFINES     += JASP_R_INTERFACE_LIBRARY QT_DEPRECATED_WARNINGS

# QT_DEPRECATED_WARNINGS is there for:
# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.

SOURCES += \
    jaspResults/src/jaspQmlSource.cpp \
    jasprcpp.cpp \
    RInside/MemBuf.cpp \
    RInside/RInside.cpp \
    jaspResults/src/jaspHtml.cpp \
    jaspResults/src/jaspObject.cpp \
    jaspResults/src/jaspJson.cpp \
    jaspResults/src/jaspContainer.cpp \
    jaspResults/src/jaspPlot.cpp \
    jaspResults/src/jaspResults.cpp \
    jaspResults/src/jaspTable.cpp \
    jaspResults/src/jaspState.cpp \
    jaspResults/src/jaspColumn.cpp

HEADERS += \
    jaspResults/src/jaspQmlSource.h \
	jasprcpp.h \
    jasprcpp_interface.h \
    RInside/Callbacks.h \
    RInside/MemBuf.h \
    RInside/RInside.h \
    RInside/RInsideAutoloads.h \
    RInside/RInsideCommon.h \
    RInside/RInsideConfig.h \
    RInside/RInsideEnvVars.h \
    jaspResults/src/jaspHtml.h \
    jaspResults/src/jaspObject.h \
    jaspResults/src/jaspJson.h \
    jaspResults/src/jaspList.h \
    jaspResults/src/jaspContainer.h \
    jaspResults/src/jaspPlot.h \
    jaspResults/src/jaspResults.h \
    jaspResults/src/jaspTable.h \
    jaspResults/src/jaspModuleRegistration.h \
    jaspResults/src/jaspState.h \
    jaspResults/src/jaspColumn.h

macx:		INCLUDEPATH += ../../boost_1_71_0
windows:	INCLUDEPATH += ../../boost_1_71_0

windows{
	QMAKE_CXXFLAGS  += -Og -municode #for big object files & support for unicode/utf16 in mingw maybe?
	SOURCE_LIBFILE   = $$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_NAME'.a'
	SOURCE_LIBFILE  ~= s,/,\\,g
	DEST_LIBFILE     = $$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_NAME'.lib'
	DEST_LIBFILE    ~= s,/,\\,g
	
	QMAKE_POST_LINK += $$quote(cmd /c copy /Y $$SOURCE_LIBFILE $$DEST_LIBFILE)
}

### making sure that writeImage.R and zzzWrappers.R are available to jaspEngine:
SRC_WRITE_IMAGE = $$winPathFix($${PWD}/jaspResults/R/writeImage.R)
SRC_WRAPPERS    = $$winPathFix($${PWD}/jaspResults/R/zzzWrappers.R)
SRC_WORKAROUNDS = $$winPathFix($${PWD}/R/workarounds.R)
SRC_SYMLINKTOOL = $$winPathFix($${PWD}/R/symlinkTools.R)
DEST_DIR_AUX_R  = $$winPathFix($$OUT_PWD/$$DESTDIR)

auxillaryRFiles.path   = $$INSTALLPATH
auxillaryRFiles.files  = $${PWD}/jaspResults/R/writeImage.R
auxillaryRFiles.files += $${PWD}/jaspResults/R/zzzWrappers.R
auxillaryRFiles.files += $${PWD}/R/workarounds.R
INSTALLS += auxillaryRFiles

win32 {
    copyRFiles.commands  += $$quote(cmd /c xcopy /I /Y $${SRC_WRITE_IMAGE} $${DEST_DIR_AUX_R}) $$escape_expand(\n\t)
	copyRFiles.commands  += $$quote(cmd /c xcopy /I /Y $${SRC_WRAPPERS}    $${DEST_DIR_AUX_R}) $$escape_expand(\n\t)
	copyRFiles.commands  += $$quote(cmd /c xcopy /I /Y $${SRC_WORKAROUNDS} $${DEST_DIR_AUX_R}) $$escape_expand(\n\t)
	copyRFiles.commands  += $$quote(cmd /c xcopy /I /Y $${SRC_SYMLINKTOOL} $${DEST_DIR_AUX_R}) $$escape_expand(\n\t)
}

unix {
    copyRFiles.commands += $(MKDIR) $$DEST_DIR_AUX_R ;
    copyRFiles.commands += cp $$SRC_WRITE_IMAGE $$DEST_DIR_AUX_R ;
	copyRFiles.commands += cp $$SRC_WRAPPERS    $$DEST_DIR_AUX_R ;
	copyRFiles.commands += cp $$SRC_WORKAROUNDS $$DEST_DIR_AUX_R ;
	copyRFiles.commands += cp $$SRC_SYMLINKTOOL $$DEST_DIR_AUX_R ;
}

! equals(PWD, $${OUT_PWD}) {
    QMAKE_EXTRA_TARGETS += copyRFiles
    POST_TARGETDEPS     += copyRFiles
}

DISTFILES += \
    jaspResults/R/RcppExports.R \
    jaspResults/R/zzaLoadModule.R \
    jaspResults/R/zzzWrappers.R \
    jaspResults/R/writeImage.R \
    jaspResults/DESCRIPTION \
    jaspResults/NAMESPACE \
    jaspResults/man/jaspList.Rd \
    jaspResults/man/jaspTable.Rd \
    jaspResults/man/jaspResultsClass.Rd \
    jaspResults/man/jaspPlot.Rd \
    jaspResults/man/jaspObject.Rd \
    jaspResults/man/jaspHtml.Rd \
    jaspResults/man/jaspContainer.Rd \
    jaspResults/man/jaspState.Rd \
    jaspResults/man/jaspResults-package.Rd
